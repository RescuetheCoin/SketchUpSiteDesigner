# frozen_string_literal: true

require 'sketchup.rb'
require 'json'
require 'time'
require 'fileutils'

# ------------------------------------------------------------------
# ActionLogger – captures user & plugin events to JSONL for later review
# ------------------------------------------------------------------
module ActionLogger
  LOG_DIR  = 'Z:/Shared/Templates and Libraries/SketchUp Library/PlugIns/Log Data'.freeze
  LOG_FILE = 'sketchup_action_log.jsonl'.freeze
  LOG_PATH = File.join(LOG_DIR, LOG_FILE)

  # Ensure log directory exists
  FileUtils.mkdir_p(LOG_DIR) unless Dir.exist?(LOG_DIR)

  # Write a generic event payload
  def self.log_event(payload)
    entry = { time: Time.now.utc.iso8601 }.merge(payload)
    begin
      File.open(LOG_PATH, 'a') { |f| f.puts(entry.to_json) }
    rescue => e
      UI.messagebox("ActionLogger error: #{e.message}")
    end
  end

  # Log an error with feature context and trimmed backtrace
  def self.log_error(feature, exception)
    log_event(
      event:     'Error',
      feature:   feature,
      message:   exception.message,
      backtrace: exception.backtrace[0..4]
    )
  end

  # Observe tool changes
  class ToolObserver < Sketchup::ToolsObserver
    def onToolChanged(tools, previous_tool)
      ActionLogger.log_event(
        event:         'ToolChanged',
        new_tool:      tools.class.name,
        previous_tool: previous_tool && previous_tool.class.name
      )
    end
  end

  # Observe entity additions/removals
  class EntitiesObserver < Sketchup::EntitiesObserver
    def onElementAdded(entities, element)
      case element
      when Sketchup::Edge
        p1, p2 = element.start.position, element.end.position
        ActionLogger.log_event(
          event:  'EdgeAdded',
          length: element.length,
          start:  [p1.x, p1.y, p1.z],
          end:    [p2.x, p2.y, p2.z]
        )
      when Sketchup::Face
        ActionLogger.log_event(
          event: 'FaceAdded',
          area:  element.area
        )
      end
    end

    # Updated for SketchUp 2024: onElementRemoved receives (entities, element)
    def onElementRemoved(entities, element)
      # element is the removed object
      ActionLogger.log_event(
        event:     'ElementRemoved',
        entity_id: element.respond_to?(:persistent_id) ? element.persistent_id : nil,
        type:      element.class.name
      )
    end
  end

  # Observe selection changes
  class SelectionObserver < Sketchup::SelectionObserver
    def onSelectionBulkChange(selection)
      ids = selection.map(&:persistent_id)
      ActionLogger.log_event(
        event:         'SelectionChanged',
        selection_ids: ids
      )
    end
  end

  # Wrap model operations for logging
  class ::Sketchup::Model
    unless method_defined?(:al_original_start_operation)
      alias_method :al_original_start_operation,  :start_operation
      alias_method :al_original_commit_operation, :commit_operation
    end

    def start_operation(name, disable_ui = true, disable_update = false)
      ActionLogger.log_event(event: 'OperationStart', name: name)
      al_original_start_operation(name, disable_ui, disable_update)
    end

    def commit_operation(*args)
      result = al_original_commit_operation(*args)
      ActionLogger.log_event(event: 'OperationCommit')
      result
    end
  end

  unless @action_logger_installed
    mdl = Sketchup.active_model
    mdl.tools.add_observer(ToolObserver.new)
    mdl.entities.add_observer(EntitiesObserver.new)
    mdl.selection.add_observer(SelectionObserver.new)
    UI.messagebox("ActionLogger started.\nLogging to:\n#{LOG_PATH}")
    @action_logger_installed = true
  end
end

# ------------------------------------------------------------------
# Stringfellow Design Assistant – core plugin
#------------------------------------------------------------------
module StringfellowDesignAssistant
  # Constants
  CATEGORY_COLORS = {
    'Estate Homes'     => [255, 222, 201],
    'Garden Homes'     => [255, 255, 153],
    'Bungalows'        => [255, 199, 50],
    'Townhomes'        => [255, 178, 101],
    'Multi-Family'     => [204, 102,   0],
    'Office'           => [160,  82,  45],
    'Commercial'       => [205,  92,  93],
    'Open Space'       => [115, 155,  67],
    'Wetlands'         => [ 64,  99,   5],
    'Wetland Buffer'   => [ 91, 115,  63],
    'Stormwater Ponds' => [145, 183, 247],
    'Asphalt'          => [170, 170, 170],
    'Concrete'         => [255, 255, 255],
    'Easements'        => [153, 255, 153]
  }.freeze
  TOLERANCE     = 10
  SQFT_PER_SQIN = 1.0 / 144.0
  SQFT_PER_ACRE = 43_560.0

  # Utility
  def self.color_close?(a, b)
    (0..2).all? { |i| (a[i] - b[i]).abs <= TOLERANCE }
  end

  def self.face_color(face)
    mat = face.material || face.back_material
    return unless mat.is_a?(Sketchup::Color)
    [mat.red, mat.green, mat.blue]
  end

  def self.ensure_layer(name)
    layers = Sketchup.active_model.layers
    layers[name] || layers.add(name)
  end

  # Recursive traversal
  def self.traverse_entities(entities, path = [], tf = Geom::Transformation.new)
    entities.each do |e|
      case e
      when Sketchup::Face, Sketchup::Edge
        yield(e, path, tf)
      when Sketchup::Group
        new_path = path + ["Group:#{e.name}"]
        traverse_entities(e.entities, new_path, tf * e.transformation) { |*a| yield(*a) }
      when Sketchup::ComponentInstance
        yield(e, path, tf)
        new_path = path + ["Comp:#{e.definition.name}"]
        traverse_entities(e.definition.entities, new_path, tf * e.transformation) { |*a| yield(*a) }
      end
    end
  end

  # Build face adjacency
  def self.build_adjacency(faces_data, edges_data)
    adj = Hash.new { |h, k| h[k] = [] }
    edges_data.each do |ed|
      ed[:faces].combination(2) do |f1, f2|
        adj[f1] << f2 unless adj[f1].include?(f2)
        adj[f2] << f1 unless adj[f2].include?(f1)
      end
    end
    adj
  end

  # Dissect model → JSON
  def self.dissect_model
    model      = Sketchup.active_model
    vertices   = []
    v_index    = {}
    faces_data = []
    edges_data = []
    comps_data = []

    # Helper to intern world‐space vertices
    intern = ->(pt) do
      key = [pt.x, pt.y, pt.z]
      v_index[key] ||= vertices.size.tap { vertices << key }
    end

    traverse_entities(model.entities) do |ent, path, tf|
      case ent
      when Sketchup::Face
        loops = ent.loops.map { |lp| lp.vertices.map { |v| intern.call(tf * v.position) } }
        faces_data << { id: ent.persistent_id, loops: loops, path: path }
      when Sketchup::Edge
        i1   = intern.call(tf * ent.start.position)
        i2   = intern.call(tf * ent.end.position)
        fids = ent.faces.map(&:persistent_id)
        edges_data << {
          id:       ent.persistent_id,
          v1:       i1,
          v2:       i2,
          faces:    fids,
          length:   (ent.length * SQFT_PER_SQIN).round(6),
          path:     path
        }
      when Sketchup::ComponentInstance
        tfw = tf * ent.transformation
        comps_data << {
          id:         ent.persistent_id,
          definition: ent.definition.name,
          transform:  tfw.to_a,
          path:       path + ["Comp:#{ent.definition.name}"]
        }
      end
    end

    payload = {
      vertices:   vertices,
      faces:      faces_data,
      edges:      edges_data,
      components: comps_data,
      adjacency:  build_adjacency(faces_data, edges_data)
    }

    # Write JSON to the ActionLogger directory
    output_dir = ActionLogger::LOG_DIR
    FileUtils.mkdir_p(output_dir) unless Dir.exist?(output_dir)
    file = File.join(output_dir, 'dissect_model_connectivity.json')

    begin
      File.open(file, 'w') { |f| f.write(JSON.pretty_generate(payload)) }
      UI.messagebox("🔍 Dissect Model written to:\n#{file}")
    rescue => e
      ActionLogger.log_error('dissect_model', e)
      UI.messagebox("❌ dissect_model error: #{e.message}")
    end
  end


  # Fix stray edges and extend chains
  def self.fix_edges
    model     = Sketchup.active_model
    ents      = model.active_entities
    threshold = 12.inch
    model.start_operation('Fix Edges', true)
    begin
      remove_strays(ents.grep(Sketchup::Edge), threshold)
      chains = build_edge_chains(ents.grep(Sketchup::Edge))
      extend_chains(chains, ents, model)
    rescue => e
      ActionLogger.log_error('fix_edges', e)
      UI.messagebox("❌ fix_edges error: #{e.message}")
    ensure
      model.commit_operation
    end
  end

  def self.remove_strays(edges, threshold)
    edges.each { |e| e.erase! if e.length < threshold }
  end

  def self.build_edge_chains(edges)
    visited = {}
    chains  = []
    edges.each do |e|
      next if visited[e]
      visited[e] = true
      chain = [e]
      chain = extend_chain(chain, visited, true)
      chain = extend_chain(chain, visited, false)
      chains << chain
    end
    chains
  end

  def self.extend_chain(chain, visited, forward)
    loop do
      edge    = forward ? chain.last : chain.first
      free_v  = forward ? edge.end : edge.start
      prev_v  = forward ? edge.start : edge.end
      dir_vec = prev_v.position.vector_to(free_v.position).normalize
      cands   = free_v.edges.reject { |x| visited[x] }
      break if cands.empty?
      best, min_ang = nil, Math::PI
      cands.each do |cand|
        other_v = cand.start == free_v ? cand.end : cand.start
        cand_vec = free_v.position.vector_to(other_v.position).normalize
        ang      = dir_vec.angle_between(cand_vec)
        if ang < min_ang
          min_ang = ang; best = cand
        end
      end
      break unless best
      visited[best] = true
      forward ? chain.push(best) : chain.unshift(best)
    end
    chain
  end

  def self.extend_chains(chains, ents, model)
    chains.each do |chain|
      next if chain.empty?
      [[chain.first, false], [chain.last, true]].each do |edge, forward|
        free_v = forward ? edge.end : edge.start
        prev_v = forward ? edge.start : edge.end
        dir    = prev_v.position.vector_to(free_v.position).normalize
        res    = model.raytest([free_v.position, dir])
        next unless res&.[](0)
        ents.add_line(free_v.position, res[0])
      end
    end
  end

  # Auto Layer Management
  def self.run_auto_layer_management
    model = Sketchup.active_model
    faces = model.active_entities.grep(Sketchup::Face)
    buckets = Hash.new { |h, k| h[k] = [] }
    faces.each do |f|
      col = face_color(f)
      next unless col
      CATEGORY_COLORS.each { |cat, rgb| if color_close?(col, rgb)
        buckets[cat] << f; break
      end }
    end
    return UI.messagebox('No matching faces found.') if buckets.empty?
    model.start_operation('Auto Layer Management', true)
    begin
      buckets.each do |cat, flist|
        grp = model.active_entities.add_group(flist)
        grp.layer = ensure_layer(cat)
        grp.name  = "#{cat} Group"
      end
    rescue => e
      ActionLogger.log_error('run_auto_layer_management', e)
      UI.messagebox("❌ run_auto_layer_management error: #{e.message}")
    ensure
      model.commit_operation
    end
  end

      # Site Summary Report
  def self.run_site_summary_report
    model        = Sketchup.active_model
    groups       = model.entities.grep(Sketchup::Group)
    areas, counts = Hash.new(0.0), Hash.new(0)
    total_area   = 0.0
    concrete_len = 0.0
    unit_count   = 0

    groups.each do |g|
      tag = g.layer&.name
      next unless CATEGORY_COLORS.key?(tag)
      area = 0.0
      count = 0
      g.entities.each do |e|
        case e
        when Sketchup::Face
          a = e.area * SQFT_PER_SQIN
          area += a
          count += 1 if a >= 1000
        when Sketchup::Edge
          concrete_len += e.length / 12.0 if tag == 'Concrete'
        end
      end
      areas[tag]   += area
      counts[tag] += count
      total_area   += area
      if ['Garden Homes','Estate Homes','Bungalows','Townhomes'].include?(tag)
        unit_count += count
      end
    end

    if total_area.zero?
      UI.messagebox('No measurable areas found.')
      return
    end

    net_sqft     = total_area - areas['Wetlands']
    gross_ac     = total_area / SQFT_PER_ACRE
    net_ac       = net_sqft / SQFT_PER_ACRE
    gross_density = unit_count / gross_ac
    net_density   = unit_count / net_ac

    html = <<~HTML.dup
      <html><body style="font-family:Arial;padding:10px">
      <h2>📊 Site Summary Report</h2>
      <p><b>Gross Area:</b> #{'%.2f' % total_area} ft² (#{'%.2f' % gross_ac} ac)</p>
      <p><b>Net Area:</b> #{'%.2f' % net_sqft} ft² (#{'%.2f' % net_ac} ac)</p>
      <p><b>Gross Density:</b> #{'%.2f' % gross_density} units/ac</p>
      <p><b>Net Density:</b> #{'%.2f' % net_density} units/ac</p>
    HTML

    CATEGORY_COLORS.each_key do |cat|
      next if areas[cat].zero?
      pct = (areas[cat] / net_sqft) * 100
      html << "<p><b>#{cat}</b>: #{'%.2f' % areas[cat]} ft² (#{'%.2f' % (areas[cat] / SQFT_PER_ACRE)} ac; #{'%.2f' % pct}% of net)"
      if ['Garden Homes','Estate Homes','Bungalows'].include?(cat)
        html << ": #{counts[cat]} units"
      end
      html << "</p>"
    end

    html << "<p><b>Total Units:</b> #{unit_count}</p>"
    html << "<p><b>Concrete Frontage:</b> #{'%.2f' % concrete_len} ft</p></body></html>"

    dlg = UI::HtmlDialog.new(
      dialog_title:   'Site Summary Report',
      preferences_key:'site_summary',
      scrollable:     true,
      resizable:      true,
      width:          600, height: 600,
      style:          UI::HtmlDialog::STYLE_DIALOG
    )
    dlg.set_html(html)
    dlg.show
  end

  # Explode All Groups
  def self.explode_all_groups
    model = Sketchup.active_model
    ents  = model.active_entities
    groups = ents.grep(Sketchup::Group).reject { |g| g.name =~ /palette|snapshot/i }
    model.start_operation('Explode All Groups', true)
    begin
      groups.each { |g| g.explode rescue nil }
    rescue => e
      ActionLogger.log_error('explode_all_groups', e)
      UI.messagebox("❌ explode_all_groups error: #{e.message}")
    ensure
      model.commit_operation
    end
  end

  # Subdivide Block Faces
  def self.subdivide_block_faces
    model   = Sketchup.active_model
    ents    = model.active_entities
    prompts = ['Lot width (feet)']
    results = UI.inputbox(prompts, [50.0], 'Subdivide Block Faces')
    return unless results
    lw = results[0].to_f.feet
    sel = model.selection.to_a
    fronts = sel.grep(Sketchup::Edge)
    faces  = sel.grep(Sketchup::Face)
    model.start_operation('Subdivide Block Faces', true)
    begin
      faces.each do |face|
        edges = fronts.any? ? fronts.select { |e| face.edges.include?(e) } : [face.edges.max_by(&:length)].compact
        edges.each do |fe|
          cnt = (fe.length / lw).floor
          next if cnt < 1
          cnt.times do |i|
            t  = (i + 1).to_f / (cnt + 1)
            pt = fe.point_at(t)
            dir = face.normal.cross(fe.start.position.vector_to(fe.end.position).normalize).normalize
            hit = model.raytest([pt, dir])
            ents.add_line(pt, hit[0]) if hit&.[](0)
          end
        end
      end
    rescue => e
      ActionLogger.log_error('subdivide_block_faces', e)
      UI.messagebox("❌ subdivide_block_faces error: #{e.message}")
    ensure
      model.commit_operation
    end
  end

  # Flip All Faces Upward
  def self.flip_faces_upward
    model = Sketchup.active_model
    faces = model.active_entities.grep(Sketchup::Face)
    model.start_operation('Flip All Faces Upward', true)
    begin
      faces.each { |f| f.reverse! if f.normal.z < 0 }
    rescue => e
      ActionLogger.log_error('flip_faces_upward', e)
      UI.messagebox("❌ flip_faces_upward error: #{e.message}")
    ensure
      model.commit_operation
    end
  end

  # Color Palette Dialog
  def self.draw_color_palette
    html = <<~HTML.dup
      <html><head><style>
        body{font-family:sans-serif;margin:10px}
        table{border-collapse:collapse;width:100%}
        td{padding:4px}
        .swatch{width:24px;height:24px;border:1px solid #000}
      </style></head><body>
      <h2>Color Palette</h2><table>
    HTML
    CATEGORY_COLORS.each do |name, rgb|
      hex = sprintf('#%02X%02X%02X', *rgb)
      html << "<tr><td><div class='swatch' style='background:#{hex}'></div></td><td>#{name} — RGB(#{rgb.join(',')}) — #{hex}</td></tr>"
    end
    html << '</table></body></html>'
    dlg = UI::HtmlDialog.new(dialog_title: 'Color Palette', preferences_key: 'palette', scrollable: true, resizable: true, width: 400, height: 300, style: UI::HtmlDialog::STYLE_DIALOG)
    dlg.set_html(html)
    dlg.show
  end

  # Self-Test
  def self.run_self_test
    results = {}
    tests = {
      'Dissect Model'    => method(:dissect_model),
      'Fix Edges'        => method(:fix_edges),
      'Auto Layers'      => method(:run_auto_layer_management),
      'Site Summary'     => method(:run_site_summary_report),
      'Explode Groups'   => method(:explode_all_groups),
      'Subdivide Blocks' => method(:subdivide_block_faces),
      'Flip Faces'       => method(:flip_faces_upward),
      'Color Palette'    => method(:draw_color_palette),
    }
    tests.each do |name, fn|
      begin
        fn.call
        results[name] = 'OK'
      rescue => e
        ActionLogger.log_error(name, e)
        results[name] = "Error: #{e.message}"
      end
    end
    UI.messagebox(results.map { |k, v| "#{k}: #{v}" }.join("
"))
  end

    # Menu Installation
  unless @installed
    menu = UI.menu('Extensions').add_submenu('🧰 Stringfellow Design Assistant')
    menu.add_item('🔧 Fix Edges')                    { StringfellowDesignAssistant.fix_edges }
    menu.add_item('⤴️ Flip All Faces Upward')         { StringfellowDesignAssistant.flip_faces_upward }
    menu.add_item('🎨 Color Palette')                { StringfellowDesignAssistant.draw_color_palette }
    menu.add_item('📏 Subdivide Block Faces')        { StringfellowDesignAssistant.subdivide_block_faces }
    menu.add_item('⚙️ Auto Layer Management')       { StringfellowDesignAssistant.run_auto_layer_management }
    menu.add_item('📊 Site Summary Report')          { StringfellowDesignAssistant.run_site_summary_report }
    menu.add_item('💣 Explode All Groups')           { StringfellowDesignAssistant.explode_all_groups }
    menu.add_item('🧩 Dissect Model → JSON')         { StringfellowDesignAssistant.dissect_model }
    menu.add_separator
    menu.add_item('🔬 Run Self-Test')                { StringfellowDesignAssistant.run_self_test }
    @installed = true
end  # module StringfellowDesignAssistant
end
file_loaded(__FILE__)
