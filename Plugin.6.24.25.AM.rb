# frozen_string_literal: true

require 'sketchup.rb'
require 'json'
require 'time'
require 'fileutils'
require 'set'

# Return if not running in SketchUp
return unless defined?(Sketchup) && defined?(UI)

# -- Observers for logging events --

class ToolObserver < Sketchup::ToolsObserver
  def onActiveToolChanged(tools, tool_name, tool_id)
    ActionLogger.log_event(event: 'ToolChanged', tool: tool_name, id: tool_id)
  end
end

class EntitiesObserver < Sketchup::EntitiesObserver
  def onElementAdded(entities, entity)
    ActionLogger.log_event(event: 'ElementAdded', type: entity.typename)
  end

  def onElementRemoved(entities, entity_id)
    ActionLogger.log_event(event: 'ElementRemoved', id: entity_id)
  end
end

class SelectionObserver < Sketchup::SelectionObserver
  def onSelectionBulkChange(selection)
    ActionLogger.log_event(event: 'SelectionBulkChange', size: selection.length)
  end
end

class AcreageDisplayObserver < Sketchup::SelectionObserver
  def initialize(dialog)
    @dialog = dialog
  end

  def onSelectionBulkChange(selection)
    faces = selection.grep(Sketchup::Face)
    if faces.empty?
      @dialog.execute_script("updateAcreage('0.0')")
    else
      area_ft = faces.map(&:area).sum / 144.0  # SketchUp reports area in square inches
      acres = (area_ft / 43_560.0).round(4)
      @dialog.execute_script("updateAcreage('#{acres}')")
    end
  end
end


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
      UI.messagebox("ActionLogger error: #{e.message}") if defined?(UI) && UI.respond_to?(:messagebox)
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
      # Skip if the element is nil
      return unless element
      
      # Guard against any errors during observation
      begin
        case element
        when Sketchup::Edge
          # Super cautious validity checking before accessing properties
          return unless element.valid? rescue false
          return unless element.start && element.start.valid? rescue false
          return unless element.end && element.end.valid? rescue false
          
          # Only now that we're sure everything is valid, get positions
          p1 = element.start.position
          p2 = element.end.position
          
          ActionLogger.log_event(
            event:  'EdgeAdded',
            length: element.length,
            start:  [p1.x, p1.y, p1.z],
            end:    [p2.x, p2.y, p2.z]
          )
        when Sketchup::Face
          # Skip if face is invalid
          return unless element.valid? rescue false
          
          begin
            area = element.area
            ActionLogger.log_event(
              event: 'FaceAdded',
              area:  area
            )
          rescue => e
            # Specific error handling for face area calculation
            ActionLogger.log_error('face_area_calculation', e)
          end
        end
      rescue => e
        # Global error handling for any other issues
        ActionLogger.log_error('element_added_observer', e)
      end
    end

    # Updated for SketchUp 2024: onElementRemoved receives (entities, element)
    def onElementRemoved(entities, element)
      # element is the removed object - can't check validity here
      begin
        # Be very cautious when accessing deleted objects
        type = element.class.name rescue 'Unknown'
        id = nil
        if element.respond_to?(:persistent_id)
          begin
            id = element.persistent_id
          rescue
            # Silently ignore errors getting persistent_id from deleted objects
          end
        end
        
        ActionLogger.log_event(
          event:     'ElementRemoved',
          entity_id: id,
          type:      type
        )
      rescue => e
        ActionLogger.log_error('element_removed_observer', e)
      end
    end
  end

  # Observe selection changes
class ::Sketchup::Model
  unless method_defined?(:al_original_start_operation)
    alias_method :al_original_start_operation,  :start_operation
    alias_method :al_original_commit_operation, :commit_operation
  end

  def start_operation(name, disable_ui = true, disable_update = false, transparent = false)
    ActionLogger.log_event(event: 'OperationStart', name: name)
    al_original_start_operation(name, disable_ui, disable_update, transparent)
  end

  def commit_operation(*args)
    result = al_original_commit_operation(*args)
    ActionLogger.log_event(event: 'OperationCommit')
    result
  end
end


  unless @action_logger_installed
    mdl = Sketchup.active_model
    mdl.tools.add_observer(::ToolObserver.new)
    mdl.entities.add_observer(::EntitiesObserver.new)
    mdl.selection.add_observer(::SelectionObserver.new)
    # Only show startup message on first installation (removed popup)
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
  
  # Lot parameters by residential category
  LOT_PARAMETERS = {
    'Estate Homes'   => { min_width: 80.0 },
    'Garden Homes'   => { min_width: 60.0 },
    'Bungalows'      => { min_width: 50.0 }
  }.freeze
  
  # Define residential categories that support subdivision
  RESIDENTIAL_CATEGORIES = ['Estate Homes', 'Garden Homes', 'Bungalows'].freeze
  
  TOLERANCE     = 10
  SQFT_PER_SQIN = 1.0 / 144.0
  SQFT_PER_ACRE = 43_560.0

  # Constants for edge cleaning
  STRAY_THRESHOLD_DEFAULT = 0.5     # inches
  VERTEX_TOLERANCE_DEFAULT = 0.001  # inches
  COLINEAR_ANGLE_DEFAULT = 1.0      # degrees
  MAX_EXTENSION_DISTANCE = 1000.0   # inches

  # Group cleanup parameters for easier management
  CLEANUP_DEFAULTS = {
    stray: STRAY_THRESHOLD_DEFAULT.inch,
    vertex_tol: VERTEX_TOLERANCE_DEFAULT.inch,
    colinear_deg: COLINEAR_ANGLE_DEFAULT.degrees,
    max_ext: MAX_EXTENSION_DISTANCE.inch
  }.freeze
  
  # UI text constants
  CLEANUP_PROMPTS = [
    'Layers to process (comma-separated, blank for all)', 
    'Tiny stray threshold (inches)', 
    'Vertex merge tolerance (inches)',
    'Colinear angle tolerance (degrees)',
    'Create faces in closed loops?',
    'Process entire model?'
  ].freeze
  
  CLEANUP_DEFAULTS_UI = [
    '',
    STRAY_THRESHOLD_DEFAULT.to_s,
    VERTEX_TOLERANCE_DEFAULT.to_s,
    COLINEAR_ANGLE_DEFAULT.to_s,
    'Yes',
    'Yes'
  ].freeze
  
  CLEANUP_DROPDOWNS = [
    '',
    '',
    '',
    '',
    'Yes|No',
    'Yes|No'
  ].freeze

  # Add this constant somewhere in the constants section
  LOG_EDGE_CLEANUP = true  # Default to enabled

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
      if defined?(UI) && UI.respond_to?(:messagebox)
        UI.messagebox("🔍 Dissect Model written to:\n#{file}")
      end
    rescue => e
      ActionLogger.log_error('dissect_model', e)
      if defined?(UI) && UI.respond_to?(:messagebox)
        UI.messagebox("❌ dissect_model error: #{e.message}")
      end
    end
  end


# Fix stray edges, extend chains, and create faces
 
# STEP 1–3: Remove stray short edges, then re-isolate faceless edges for repair ops.

def self.build_edge_graph(edges)
  # Constructs a hash graph where each key is a Point3d and values are edges connected to it.
  graph = Hash.new { |h, k| h[k] = [] }
  edges.each do |e|
    next unless e.valid?
    graph[e.start.position] << e
    graph[e.end.position] << e
  end
  graph
end

# Performs DFS from a given point, walking edges until a closed loop or open path ends.
# Returns an ordered list of connected points.
def self.walk_edge_graph(graph, start_point, visited = Set.new)
  path = [start_point]
  stack = [start_point]

  while stack.any?
    current = stack.pop
    neighbors = graph[current].reject do |edge|
      visited.include?(edge)
    end

    break if neighbors.empty?
    edge = neighbors.first
    visited << edge

    next_point = (edge.start.position == current) ? edge.end.position : edge.start.position
    path << next_point
    stack << next_point

    return path if next_point == start_point && path.size > 2  # loop detected
  end

  path
end

# Snaps a point to a 0.5" grid to reduce micro-gaps and support merge logic.
def self.quantize_point(point, grid = 0.5.inch)
  x = (point.x / grid).round * grid
  y = (point.y / grid).round * grid
  z = (point.z / grid).round * grid
  Geom::Point3d.new(x, y, z)
end

def self.fix_edges
  model     = Sketchup.active_model
  ents      = model.active_entities
  threshold = 4.inch
  max_ray_dist = 36.inch
  merge_threshold = 1.inch
  edge_add_count = 0
  edge_del_count = 0
  create_faces = false  # Change via dialog box

  prompts = ['Create faces from loops?']
  defaults = ['No']
  list = ['Yes|No']
  results = UI.inputbox(prompts, defaults, list, 'Fix Edges Options')
  return unless results
  create_faces = results[0] == 'Yes'

  model.start_operation('Fix Edges', true, false)
  begin
# Prompt user to include edges from groups/components
prompts = ['Create faces from loops?', 'Include nested group/component edges?']
defaults = ['No', true]
list = ['Yes|No', 'true|false']
results = UI.inputbox(prompts, defaults, list, 'Fix Edges Options')
return unless results
create_faces = results[0] == 'Yes'
include_nested = results[1]

# Step 1: Collect edges
Sketchup.status_text = "Step 1: Collecting edges..."

def collect_all_edges(entities, edges = [])
  entities.each do |e|
    if e.is_a?(Sketchup::Edge)
      edges << e
    elsif e.respond_to?(:definition) # ComponentInstance
      collect_all_edges(e.definition.entities, edges)
    elsif e.respond_to?(:entities)   # Group
      collect_all_edges(e.entities, edges)
    end
  end
  edges
end

edges = if include_nested
  collect_all_edges(Sketchup.active_model.entities)
else
  ents.grep(Sketchup::Edge)
end

puts "Collected #{edges.size} edges"

# Step 2: Removing short stray edges...
Sketchup.status_text = "Step 2: Removing short stray edges..."

# Allow user to set stray-edge threshold
threshold_prompt = ['Edge length threshold for deletion (inches)']
threshold_defaults = [4.0]
threshold_input = UI.inputbox(threshold_prompt, threshold_defaults, 'Stray Edge Removal Settings')
return unless threshold_input
threshold = threshold_input[0].to_f.inch

deleted_edges = []

to_delete = edges.select do |e|
  e.valid? && e.length.to_l < threshold
end

to_delete.each do |e|
  begin
    deleted_edges << { id: e.persistent_id, length: e.length.to_f }
    e.erase!
    edge_del_count += 1
  rescue => ex
    ActionLogger.log_error('fix_edges_erase', ex)
  end
end

if LOG_EDGE_CLEANUP
  ActionLogger.log_event(
    event: 'FixEdges_ShortEdgesRemoved',
    count: edge_del_count,
    threshold_inches: threshold.to_l.to_f
  )
end

if LOG_EDGE_CLEANUP
  ActionLogger.log_event(
    event: 'FixEdges_ShortEdgesRemoved',
    count: edge_del_count,
    threshold_inches: threshold.to_l.to_f
  )
end

Sketchup.status_text = "Step 3: Re-checking stray edges..."

edges = if include_nested
  collect_all_edges(Sketchup.active_model.entities).select { |e| e.faces.empty? }
else
  ents.grep(Sketchup::Edge).select { |e| e.faces.empty? }
end

puts "Remaining stray edges: #{edges.size}"


# STEP 4: Extend edges by raycasting along their direction vector to find nearby hits.
Sketchup.status_text = "Step 4: Extending edges via raycast..."
raycast_limit = 500
raycast_count = 0
unmatched_points = []

edges.each do |edge|
  break if raycast_count >= raycast_limit
  next unless edge.valid?

  from = edge.start
  to   = edge.end

  dir = from.position.vector_to(to.position).normalize
  res = model.raytest([to.position, dir])

  if res && res[0].is_a?(Geom::Point3d) && res[0].distance(to.position) <= max_ray_dist
    ents.add_line(to.position, res[0])
    edge_add_count += 1
    raycast_count += 1
  else
    # Step 4a+: Rectangle perimeter search for nearby endpoints
    ortho1 = dir.axes.x
    ortho2 = dir.axes.y
    size = 6.inch

    begin
      corners = [
        to.position.offset(ortho1,  size).offset(dir,  size),
        to.position.offset(ortho1,  size).offset(dir, -size),
        to.position.offset(ortho1, -size).offset(dir, -size),
        to.position.offset(ortho1, -size).offset(dir,  size)
      ]

      rect_edges = []
      4.times do |i|
        rect_edges << [corners[i], corners[(i+1)%4]]
      end

      edge_points = ents.grep(Sketchup::Edge).flat_map { |e| [e.start.position, e.end.position] }.uniq

      rect_edges.each do |p1, p2|
        edge_points.each do |ep|
          next if ep == to.position
          dist = Geom.closest_points([p1, p2], ep)[1].distance(ep)
          if dist < 1.inch
            ents.add_line(to.position, ep)
            edge_add_count += 1
            raycast_count += 1
            raise :connected
          end
        end
      end

      unmatched_points << to.position
    rescue :connected
    rescue => ex
      ActionLogger.log_error('fix_edges_rect_extend', ex)
    end
  end
end


   # STEP 5: Re-check faceless edges after extensions
Sketchup.status_text = "Step 5: Finding near stray edges again..."
edges = ents.grep(Sketchup::Edge).select { |e| e.faces.empty? }

# STEP 6: Group vertices within 1" and connect them unless already connected.
# Helps eliminate loose or barely-separated points.
Sketchup.status_text = "Step 6: Closing near vertices..."

# Re-fetch a clean list of valid, faceless edges
edges = ents.grep(Sketchup::Edge).select { |e| e.valid? && e.faces.empty? }

# Collect all unique endpoints
points = edges.flat_map { |e| [e.start.position, e.end.position] if e.start && e.end }.compact.uniq

# Group nearby points within the merge_threshold
grouped = []
until points.empty?
  seed = points.shift
  cluster = points.select { |pt| pt.distance(seed) < merge_threshold }
  points -= cluster
  grouped << [seed, *cluster] if cluster.size <= 10
end

# Avoid reconnecting already-connected points
existing_keys = Set.new
ents.each do |e|
  next unless e.is_a?(Sketchup::Edge) && e.valid?
  coords = [[e.start.position.to_a], [e.end.position.to_a]].sort
  existing_keys << coords
end

# Connect close clustered points unless already connected
grouped.each do |group|
  group.combination(2) do |p1, p2|
    key = [[p1.to_a], [p2.to_a]].sort
    unless existing_keys.include?(key)
      ents.add_line(p1, p2)
      edge_add_count += 1
    end
  end
end

# ✅ ADDITIONAL STITCHING STEP:
# After all previous logic, this closes remaining near-loops by connecting open ends.
def self.close_small_open_loops(ents, max_gap = 3.inch)
  # Collect all faceless edges
  edges = ents.grep(Sketchup::Edge).select { |e| e.valid? && e.faces.empty? }
  graph = build_edge_graph(edges)
  visited = Set.new

  graph.keys.each do |start|
    next if visited.include?(start)
    path = walk_edge_graph(graph, start, visited)
    next if path.size < 2

    first = path.first
    last  = path.last

    # Skip if it's already closed or the gap is too large
    next if first == last
    next if first.distance(last) > max_gap

    # Add a bridging edge to close the loop
    ents.add_line(first, last)
  end
end

# ✅ Run loop-closing logic before face creation
close_small_open_loops(ents)

# STEP 7: Traverse edge graph to find loops and attempt face creation
if create_faces
  Sketchup.status_text = "Step 7: Creating faces from graph-traced loops..."

  # Re-fetch post-connection edge graph
  edges = ents.grep(Sketchup::Edge).select { |e| e.valid? && e.faces.empty? }
  graph = build_edge_graph(edges)
  visited = Set.new

  graph.keys.each do |start|
    next if visited.include?(start)
    path = walk_edge_graph(graph, start, visited)
    next if path.size < 3
    next unless path.first.distance(path.last) < 0.01  # Must be closed

    # Best-fit plane for checking planarity
    plane = Geom.fit_plane_to_points(path)
    next unless plane

    normal = Geom::Vector3d.new(plane[0], plane[1], plane[2])
    origin = Geom::Point3d.new(plane[3] * normal.x, plane[3] * normal.y, plane[3] * normal.z)

    # Skip if path is not nearly planar
    next unless path.all? { |pt| (pt.vector_to(origin).dot(normal)).abs < 0.01 }

    # Attempt to create a face
    ents.add_face(path)
  end
end

  # Auto Layer Management
# Returns an [r,g,b] array for a face’s front or back material, or nil
def self.face_color(face)
  mat = face.material || face.back_material
  return unless mat.is_a?(Sketchup::Material)
  col = mat.color
  [col.red, col.green, col.blue]
end

# Group all faces whose material color matches one of our CATEGORY_COLORS
def self.run_auto_layer_management
  model = Sketchup.active_model
  faces = model.active_entities.grep(Sketchup::Face)
  buckets = Hash.new { |h, k| h[k] = [] }

  faces.each do |f|
    rgb = face_color(f)
    next unless rgb
    CATEGORY_COLORS.each do |category, target_rgb|
      if color_close?(rgb, target_rgb)
        buckets[category] << f
        break
      end
    end
  end

  if buckets.empty?
    UI.messagebox("No matching faces found for Auto Layer Management.")
    return
  end

  model.start_operation("Auto Layer Management", true)
  begin
    buckets.each do |category, face_list|
      grp = model.active_entities.add_group(face_list)
      grp.layer = ensure_layer(category)
      grp.name  = "#{category} Group"
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
  model  = Sketchup.active_model
  groups = model.entities.grep(Sketchup::Group)
  areas, counts = Hash.new(0.0), Hash.new(0)

  groups.each do |g|
    tag = g.layer&.name
    next unless CATEGORY_COLORS.key?(tag)
    g.entities.grep(Sketchup::Face).each do |f|
      sf = f.area * SQFT_PER_SQIN
      areas[tag] += sf
      counts[tag] += 1
    end
  end

  residential_tags = [
    'Estate Homes', 'Garden Homes', 'Bungalows',
    'Townhomes', 'Multi-Family'
  ]

  total_sf = areas.values.sum
  gross_acres = total_sf / SQFT_PER_ACRE
  wetlands_sf = areas['Wetlands'] || 0.0
  net_acres = (total_sf - wetlands_sf) / SQFT_PER_ACRE
  total_units = residential_tags.sum { |cat| counts[cat] }

  html = "<html><body><h2>Site Summary Report</h2>".dup

  areas.each do |tag, sf|
    ac = sf / SQFT_PER_ACRE
    if residential_tags.include?(tag)
      html << "<p><b>#{tag}:</b> #{'%.2f' % sf} sf (#{'%.2f' % ac} ac) — #{counts[tag]} lots</p>"
    else
      html << "<p><b>#{tag}:</b> #{'%.2f' % sf} sf (#{'%.2f' % ac} ac)</p>"
    end
  end

  html << "<hr>"
  html << "<p><b>Gross Acres:</b> #{'%.2f' % gross_acres} ac</p>"
  html << "<p><b>Net Acres (excl. Wetlands):</b> #{'%.2f' % net_acres} ac</p>"
  html << "<p><b>Total Residential Units:</b> #{total_units}</p>"
  html << "<p><b>Units per Gross Acre:</b> #{'%.2f' % (total_units / gross_acres)}</p>"
  html << "<p><b>Units per Net Acre:</b> #{'%.2f' % (total_units / net_acres)}</p>"

  html << '</body></html>'
  dlg = UI::HtmlDialog.new(
    dialog_title: 'Site Summary Report',
    preferences_key: 'site_summary',
    scrollable: true,
    resizable: true,
    width: 400,
    height: 400,
    style: UI::HtmlDialog::STYLE_DIALOG
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
      UI.messagebox("❌ explode_all_groups error: #{e.message}") if defined?(UI) && UI.respond_to?(:messagebox)
    ensure
      model.commit_operation
    end
  end

  # Subdivide Block Faces with Category-Based Parameters
  def self.subdivide_block_faces_enhanced
    model = Sketchup.active_model
    ents = model.active_entities
    
    # Get user input for lot width parameters for each category
    prompts = ['Estate Homes min width (feet)', 'Garden Homes min width (feet)', 'Bungalows min width (feet)', 'Create faces?', 'Maintain parent materials?', 'Group by lot?']
    defaults = [80.0, 60.0, 50.0, 'Yes', 'Yes', 'Yes']
    list = ['', '', '', 'Yes|No', 'Yes|No', 'Yes|No']
    results = UI.inputbox(prompts, defaults, list, 'Lot Width Configuration')
    return unless results
    
    # Update lot parameters with user values
    LOT_PARAMETERS['Estate Homes'][:min_width] = results[0].to_f
    LOT_PARAMETERS['Garden Homes'][:min_width] = results[1].to_f
    LOT_PARAMETERS['Bungalows'][:min_width] = results[2].to_f
    
    create_faces = results[3] == 'Yes'
    maintain_materials = results[4] == 'Yes'
    group_by_lot = results[5] == 'Yes'
    
    # Collect selected groups
    selected_groups = model.selection.grep(Sketchup::Group)
    
    # Show warning if no groups selected
    if selected_groups.empty?
      UI.messagebox("Please select at least one group to subdivide.") if defined?(UI) && UI.respond_to?(:messagebox)
      return
    end
    
    # Extract all faces from selected groups and determine their categories
    group_faces = {}
    selected_groups.each do |group|
      # Try to determine category from group layer or name
      group_category = detect_group_category(group)
      
      # If group doesn't have a recognized category, try to determine from faces
      if !RESIDENTIAL_CATEGORIES.include?(group_category)
        # Look at faces within the group
        residential_faces = group.entities.grep(Sketchup::Face).select do |face|
          face_category = detect_face_category(face)
          RESIDENTIAL_CATEGORIES.include?(face_category)
        end
        
        # Use the most common face category if available
        if residential_faces.any?
          category_counts = Hash.new(0)
          residential_faces.each do |face|
            category_counts[detect_face_category(face)] += 1
          end
          group_category = category_counts.max_by { |_, count| count }.first
        end
      end
      
      # Skip groups with unrecognized categories
      unless RESIDENTIAL_CATEGORIES.include?(group_category)
        next
      end
      
      # Store all faces in this group with their detected category
      group_faces[group] = {
        category: group_category,
        faces: group.entities.grep(Sketchup::Face),
        fronts: model.selection.grep(Sketchup::Edge).select { |e| group.entities.include?(e) }
      }
    end
    
    # Check if any valid groups were found
    if group_faces.empty?
      UI.messagebox("None of the selected groups contain residential categories that can be subdivided: #{RESIDENTIAL_CATEGORIES.join(', ')}") if defined?(UI) && UI.respond_to?(:messagebox)
      return
    end
    
    model.start_operation('Enhanced Subdivision', true)
    begin
      # Track created lots for summary
      created_lots_by_category = Hash.new(0)
      
      # Process each group and its faces
      group_faces.each do |group, data|
        group_category = data[:category]
        faces = data[:faces]
        fronts = data[:fronts]
        
        # Determine lot width based on category
        lot_width = LOT_PARAMETERS[group_category][:min_width].feet
        
        # Process each face in the group
        faces.each do |face|
          # Store group info for later reference
          parent_group = group
          parent_layer = group.layer
          parent_material = face.material
          parent_back_material = face.back_material
          
          # Track original face edges for reference
          original_edges = face.edges.to_a
          
          # Get edges to subdivide - either selected edges in this group or longest edge
          edges = fronts.any? ? fronts.select { |e| face.edges.include?(e) } : [face.edges.max_by(&:length)].compact
          
          # Track all subdivision lines and their endpoints
          subdivision_data = []
          
          # Create division lines
          edges.each do |fe|
            # Skip if edge too short
            cnt = (fe.length / lot_width).floor
            next if cnt < 1
            
            # Calculate perpendicular direction for rays
            edge_vector = fe.line[1]
            perp_vector = face.normal.cross(edge_vector).normalize
            
            # Calculate all subdivision points
            (1..cnt).each do |i|
              t = i.to_f / (cnt + 1)
              pt = fe.point_at(t)
              
              # Find opposite edge intersection within the face
              hit = model.raytest([pt, perp_vector])
              if hit && hit[0]
                # Create the dividing line edge within the group
                div_edge = parent_group.entities.add_line(pt, hit[0])
                
                # Store subdivision data for face creation
                subdivision_data << {
                  start_point: pt,
                  end_point: hit[0],
                  edge: div_edge,
                  parent_edge: fe,
                  category: group_category
                }
              end
            end
          end
          
          # Skip face creation if not requested or no subdivisions created
          next unless create_faces && subdivision_data.any?
          
          # Get all edges in the group after subdivision
          all_edges = parent_group.entities.grep(Sketchup::Edge)
          
          # Find all edges that were part of or created within this face
          face_edges = all_edges.select do |e|
            original_edges.include?(e) || subdivision_data.any? { |sd| sd[:edge] == e }
          end
          
          # Create lots by finding edge loops
          lot_faces = []
          
          # Helper method to find connected edges
          find_connected_edges = lambda do |start_point, available_edges, edge_chain = [], visited = {}, current_direction = nil|
            connected = available_edges.select { |e| (e.start.position == start_point || e.end.position == start_point) && !visited[e] }
            return edge_chain if connected.empty?
            
            # If we have a direction, prioritize edges that continue in that direction
            if current_direction && connected.size > 1
              # Sort by angle to current direction (smallest first)
              connected.sort_by! do |e|
                next_pt = (e.start.position == start_point) ? e.end.position : e.start.position
                vector = start_point.vector_to(next_pt)
                angle = current_direction.angle_between(vector)
                angle
              end
            end
            
            # Try each connected edge
            connected.each do |edge|
              visited[edge] = true
              edge_chain << edge
              
              # Determine next point and direction
              next_pt = (edge.start.position == start_point) ? edge.end.position : edge.start.position
              new_direction = start_point.vector_to(next_pt)
              
              # Recursively find next edges
              result = find_connected_edges.call(next_pt, available_edges, edge_chain, visited, new_direction)
              
              # If we found a closed loop, return it
              if result.first.start.position == result.last.end.position || 
                 result.first.start.position == result.last.start.position ||
                 result.first.end.position == result.last.end.position ||
                 result.first.end.position == result.last.start.position
                return result
              end
              
              # Otherwise backtrack
              edge_chain.pop
              visited[edge] = false
            end
            
            # No closed loop found from this edge
            return edge_chain
          end
          
          # Simple approach: identify zones bounded by original edges and subdivision lines
          next_zone_index = 1
          
          # Process each subdivision line to create potential faces on either side
          subdivision_data.each do |sd|
            # For each subdivision line, we try to create two potential faces
            # (one on each side of the subdividing line)
            
            # Find intersecting subdivision lines
            intersecting = subdivision_data.select do |other_sd| 
              other_sd != sd && 
              (other_sd[:start_point] == sd[:start_point] || 
               other_sd[:start_point] == sd[:end_point] ||
               other_sd[:end_point] == sd[:start_point] || 
               other_sd[:end_point] == sd[:end_point])
            end
            
            # Skip if this subdivision line doesn't form corners with others
            next if intersecting.empty?
            
            # Try to create a face using this subdivision line as one edge
            # This is a simplification - in a full implementation, we'd need more robust
            # algorithms to identify all possible closed polygons
            
            # Attempt to create a face directly if we have a simple quadrilateral
            # defined by two subdivision lines and original face edges
            if intersecting.size == 1
              other_sd = intersecting.first
              
              # Check if they share exactly one point
              shared_point = nil
              if sd[:start_point] == other_sd[:start_point]
                shared_point = sd[:start_point]
              elsif sd[:start_point] == other_sd[:end_point]
                shared_point = sd[:start_point]
              elsif sd[:end_point] == other_sd[:start_point]
                shared_point = sd[:end_point]
              elsif sd[:end_point] == other_sd[:end_point]
                shared_point = sd[:end_point]
              end
              
              # If they share a point, try to create a face
              if shared_point
                # Get the three corner points of the potential face
                corners = [
                  sd[:start_point] == shared_point ? sd[:end_point] : sd[:start_point],
                  shared_point,
                  other_sd[:start_point] == shared_point ? other_sd[:end_point] : other_sd[:start_point]
                ]
                
                # Find the fourth corner by ray intersection
                direction1 = corners[0].vector_to(corners[1])
                direction2 = corners[2].vector_to(corners[1])
                
                # Create the face if possible
                begin
                  # Create a group for this lot if requested
                  lot_group = nil
                  lot_entities = nil
                  
                  if group_by_lot
                    # Create nested group within the parent group
                    lot_group = parent_group.entities.add_group
                    lot_group.name = "#{group_category} Lot #{next_zone_index}"
                    lot_group.layer = parent_layer if parent_layer
                    lot_entities = lot_group.entities
                  else
                    # Add directly to parent group
                    lot_entities = parent_group.entities
                  end
                  
                  # Try to create face by adding necessary edges and connecting corners
                  lot_face = lot_entities.add_face(corners)
                  
                  # Apply materials if requested
                  if maintain_materials && lot_face
                    lot_face.material = parent_material if parent_material
                    lot_face.back_material = parent_back_material if parent_back_material
                  end
                  
                  # Track the created face and increment category counter
                  if lot_face
                    lot_faces << lot_face
                    created_lots_by_category[group_category] += 1
                    next_zone_index += 1
                  end
                rescue => e
                  ActionLogger.log_error('lot_face_creation', e)
                  # Just continue to the next attempt if this one fails
                end
              end
            end
          end
          
          # More comprehensive approach for complex subdivisions
          if lot_faces.empty? && subdivision_data.any?
            # Alternative approach: use SketchUp's native face finding
            # When direct face creation fails, we can try to let SketchUp find faces
            # after creating all necessary edges
            
            # First, ensure all necessary edges exist
            subdivision_data.each do |sd|
              # If the edge was created earlier but doesn't exist now,
              # recreate it to ensure all boundaries are present
              unless sd[:edge].valid?
                sd[:edge] = parent_group.entities.add_line(sd[:start_point], sd[:end_point])
              end
            end
            
            # Let SketchUp try to find faces automatically
            parent_group.entities.grep(Sketchup::Face).each do |new_face|
              # Check if this is a face we just created
              if new_face.valid? && !faces.include?(new_face)
                # Apply materials if requested
                if maintain_materials
                  new_face.material = parent_material if parent_material
                  new_face.back_material = parent_back_material if parent_back_material
                end
                
                # Create a group for this lot if requested
                if group_by_lot
                  lot_group = parent_group.entities.add_group
                  lot_group.name = "#{group_category} Lot #{next_zone_index}"
                  lot_group.layer = parent_layer if parent_layer
                  
                  # Move the face to the group
                  lot_face = lot_group.entities.add_face(new_face.vertices.map(&:position))
                  
                  # Apply materials to the grouped face
                  if maintain_materials && lot_face
                    lot_face.material = parent_material if parent_material
                    lot_face.back_material = parent_back_material if parent_back_material
                  end
                  
                  # Delete the original face since we've moved it into a group
                  new_face.erase! if new_face.valid?
                  created_lots_by_category[group_category] += 1
                  next_zone_index += 1
                end
              end
            end
          end
        end
      end
      
      # Inform user of results with category breakdown
      if create_faces
        message = "Subdivision complete.\n\n"
        created_lots_by_category.each do |category, count|
          if count > 0
            message += "#{category}: #{count} lots\n"
          end
        end
        if defined?(UI) && UI.respond_to?(:messagebox)
          UI.messagebox(message)
        end
      else
        if defined?(UI) && UI.respond_to?(:messagebox)
          UI.messagebox("Subdivision lines created.")
        end
      end
    rescue => e
      ActionLogger.log_error('enhanced_subdivision', e)
      if defined?(UI) && UI.respond_to?(:messagebox)
        UI.messagebox("❌ Enhanced subdivision error: #{e.message}")
      end
    ensure
      model.commit_operation
    end
  end

  # Helper method to detect face category based on color
  def self.detect_face_category(face)
    face_color = self.face_color(face)
    return "Unknown" unless face_color
    
    # Check if face color matches any category
    CATEGORY_COLORS.each do |category, rgb|
      return category if color_close?(face_color, rgb)
    end
    
    # If face has a layer that matches a category, use that
    if face.layer && CATEGORY_COLORS.key?(face.layer.name)
      return face.layer.name
    end
    
    # Check parent group's layer if any
    model = Sketchup.active_model
    model.entities.grep(Sketchup::Group).each do |g|
      if g.entities.include?(face) && g.layer && CATEGORY_COLORS.key?(g.layer.name)
        return g.layer.name
      end
    end
    
    "Unknown"
  end

  # Helper method to detect group category based on layer or name
  def self.detect_group_category(group)
    # First check the group's layer
    if group.layer && CATEGORY_COLORS.key?(group.layer.name)
      return group.layer.name
    end
    
    # Check if the group name contains a category
    RESIDENTIAL_CATEGORIES.each do |category|
      if group.name.include?(category)
        return category
      end
    end
    
    # Try to detect from the most common face category
    face_categories = Hash.new(0)
    group.entities.grep(Sketchup::Face).each do |face|
      category = detect_face_category(face)
      face_categories[category] += 1 if RESIDENTIAL_CATEGORIES.include?(category)
    end
    
    # Return the most common category if any
    return face_categories.max_by { |_, count| count }&.first || "Unknown" if face_categories.any?
    
    "Unknown"
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

def self.launch_acreage_display
  dlg = UI::HtmlDialog.new({
    dialog_title: "Selected Area (Acres)",
    preferences_key: "com.stringfellow.acreage_display",
    style: UI::HtmlDialog::STYLE_UTILITY,
    width: 200,
    height: 80,
    resizable: false
  })

  html = <<-HTML
    <html>
      <body style="font-family:sans-serif;text-align:center;padding-top:20px;">
        <div id="acreage">Acreage: 0.0</div>
        <script>
          function updateAcreage(acres) {
            document.getElementById('acreage').innerText = "Acreage: " + acres;
          }
        </script>
      </body>
    </html>
  HTML

  dlg.set_html(html)
  dlg.show

  @acreage_dialog = dlg
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
      'Subdivide Blocks' => method(:subdivide_block_faces_enhanced),
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
    menu.add_item('📏 Subdivide Block Faces')        { StringfellowDesignAssistant.subdivide_block_faces_enhanced }
    menu.add_item('⚙️ Auto Layer Management')       { StringfellowDesignAssistant.run_auto_layer_management }
    menu.add_item('📊 Site Summary Report')          { StringfellowDesignAssistant.run_site_summary_report }
    menu.add_item('💣 Explode All Groups')           { StringfellowDesignAssistant.explode_all_groups }
    menu.add_item('🧩 Dissect Model → JSON')         { StringfellowDesignAssistant.dissect_model }
    menu.add_separator
    menu.add_item('🔬 Run Self-Test')                { StringfellowDesignAssistant.run_self_test }
    @installed = true
  end

  # Helper method for building faces
  def self.build_face_perimeter_from_edges(edges)
    edges.map do |e| 
      begin
        # Safely get positions if edge is valid
        if e && (e.valid? rescue false) && 
           e.start && (e.start.valid? rescue false) && 
           e.end && (e.end.valid? rescue false)
          [e.start.position, e.end.position]
        else
          []
        end
      rescue => ex
        ActionLogger.log_error('get_edge_perimeter', ex) if defined?(ActionLogger)
        []
      end
    end.flatten.compact.uniq
  end
end

unless @acreage_display_initialized
  UI.start_timer(1, false) do
    StringfellowDesignAssistant.launch_acreage_display
    dlg = StringfellowDesignAssistant.instance_variable_get(:@acreage_dialog)
    if dlg
      observer = AcreageDisplayObserver.new(dlg)
      Sketchup.active_model.selection.add_observer(observer)
    end
  end
  @acreage_display_initialized = true
end


file_loaded(__FILE__)
