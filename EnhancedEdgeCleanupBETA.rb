def self.enhanced_fix_edges
    model = Sketchup.active_model
    
    # Get all layers/tags in the model
    all_layers = model.layers.to_a.map(&:name)
    
    # Configure cleanup parameters
    results = UI.inputbox(CLEANUP_PROMPTS, CLEANUP_DEFAULTS_UI, CLEANUP_DROPDOWNS, 'Enhanced Edge Cleanup')
    return unless results
    
    # Parse user input with better variable names
    layer_filter = results[0].strip
    stray_threshold = results[1].to_f.inch
    vertex_tolerance = results[2].to_f.inch
    colinear_tolerance = results[3].to_f.degrees
    create_faces = results[4].strip.casecmp?('yes')
    process_entire_model = results[5].strip.casecmp?('yes')
    
    selected_layers = layer_filter.empty? ? all_layers : layer_filter.split(',').map(&:strip)
    
    # Determine which entities to process
    if process_entire_model
      ents = model.entities
    else
      # See if a group or component is selected
      selection = model.selection
      if selection.length == 1 && (selection[0].is_a?(Sketchup::Group) || selection[0].is_a?(Sketchup::ComponentInstance))
        if selection[0].is_a?(Sketchup::Group)
          ents = selection[0].entities
        else
          ents = selection[0].definition.entities
        end
      else
        ents = model.active_entities
      end
    end
    
    # Start the operation
    model.start_operation('Enhanced Edge Cleanup', true,false)
    
    begin
      # 1. Collect all edges to process based on tag selection
      all_edges = ents.grep(Sketchup::Edge).to_a # Convert to array to avoid mutating while iterating
      edges_to_process = all_edges.select do |edge|
        edge && (edge.valid? rescue false) && edge.layer && 
        selected_layers.include?(edge.layer.name.to_s)
      end
      
      # If no edges match the tag filter, use all valid edges
      if edges_to_process.empty?
        edges_to_process = all_edges.select { |edge| edge && (edge.valid? rescue false) }
      end
      
            # 2. Remove tiny stray edges (collect first, then erase)      edges_to_erase = []      stray_count = 0            edges_to_process.each do |edge|        if edge && (edge.valid? rescue false) && edge.length < stray_threshold          edges_to_erase << edge          stray_count += 1        end                # Check for cancel every 1000 edges        if stray_count % 1000 == 0 && escape_pressed?          model.abort_operation          self.show_message("Edge cleanup cancelled by user.")          return        end      end
      
      # Now erase all strays at once
      erase_edges(edges_to_erase)
      
      # 3. Merge duplicate vertices (those closer than tolerance)
      merged_count = merge_close_vertices!(ents, edges_to_process, vertex_tolerance)
      
      # 4. Purge unused entities if possible
      ents.purge_unused(true) if ents.respond_to?(:purge_unused)
      
      # 5. Build chains of edges
      chains = build_enhanced_edge_chains(ents.grep(Sketchup::Edge).select(&:valid?))
      
      # Remove any stray nils from every chain right after building them
      chains.each(&:compact!)
      
      # 6. Merge colinear segments within chains
      colinear_merged = 0
      chains.each do |chain|
        colinear_merged += merge_colinear_segments(chain, ents, colinear_tolerance)
      end
      
      # Remove any stray nils from every chain
      chains.each(&:compact!)
      
      # 7. Extend chains where possible
      extended = extend_enhanced_chains(chains, ents, model)
      
      # 8. Create faces in closed loops if requested
      faces_created = create_faces ? build_faces_from_loops!(chains, ents) : 0
      
      # Report results
      result_message = "Edge cleanup complete:\n"
      result_message += "• #{stray_count} tiny stray edges removed\n"
      result_message += "• #{merged_count} duplicate vertices merged\n"
      result_message += "• #{colinear_merged} colinear segments merged\n"
      result_message += "• #{extended} chain endpoints extended\n"
      result_message += "• #{faces_created} faces created" if create_faces
      
      UI.messagebox(result_message) if defined?(UI) && UI.respond_to?(:messagebox)
    rescue => e
      ActionLogger.log_error('enhanced_fix_edges', e)
      UI.messagebox("❌ Enhanced edge cleanup error: #{e.message}") if defined?(UI) && UI.respond_to?(:messagebox)
    ensure
      model.commit_operation
    end
  end
  
  # Helper method to erase multiple edges (original implementation)
  # @param edges [Array<Sketchup::Edge>] edges to erase
  # @return [void]
  def self.erase_edges(edges)
    # Forward to the safer implementation at the end of the file
    self.erase_edges_safely(edges)
  end

  # Helper method to safely erase a collection of edges
  # @param edges [Array<Sketchup::Edge>] edges to erase
  # @return [void]
  def self.erase_edges_safely(edges)
    return unless edges && !edges.empty?
    
    begin
      # Filter and erase valid edges only
      edges.each do |edge|
        if edge && (edge.valid? rescue false)
          begin
            edge.erase!
          rescue => e
            # Log error but continue with other edges
            ActionLogger.log_error('erase_edge', e) if defined?(ActionLogger)
          end
        end
      end
    rescue => e
      # Catch any unexpected errors
      ActionLogger.log_error('erase_edges_batch', e) if defined?(ActionLogger)
    end
  end
  
  # Helper method to create multiple edges
  # @param ents [Sketchup::Entities] entities collection to add to
  # @param segments [Array<Array<Geom::Point3d>>] array of point pairs
  # @return [Array<Sketchup::Edge>] newly created edges
  def self.create_edges(ents, segments)
    return [] unless ents && segments && !segments.empty?
    
    result = []
    segments.each do |points|
      begin
        next unless points && points.size >= 2
        p1, p2 = points
        next unless p1 && p2
        
        new_edge = ents.add_line(p1, p2)
        result << new_edge if new_edge
      rescue => e
        ActionLogger.log_error('create_edge', e) if defined?(ActionLogger)
      end
    end
    result
  end
  
  # Helper to create faces from chains that form closed loops
  # @param chains [Array<Array<Sketchup::Edge>>] array of edge chains
  # @param ents [Sketchup::Entities] entities collection to add faces to
  # @param created_faces [Array] optional array to store created faces for logging
  # @return [Integer] number of faces created
  def self.build_faces_from_loops!(chains, ents, created_faces = nil)
    return 0 if chains.nil? || ents.nil?
    
    faces_created = 0
    
    chains.each do |chain|
      # Skip chains that are nil or too short to form a face
      next if chain.nil? || chain.length < 3
      
      # Ensure all edges in the chain are still valid
      begin
        valid_chain = chain.select { |edge| edge && (edge.valid? rescue false) }
        # Skip if we've lost too many edges to form a face
        next if valid_chain.length < 3
        
        # Only proceed if we have a closed loop, checking with rescue to handle any validity issues
        is_closed = false
        begin
          is_closed = chain_is_closed?(valid_chain)
        rescue => e
          ActionLogger.log_error('check_chain_closed', e)
          next
        end
        
        if is_closed
          # Get unique vertices in the correct order, with error handling
          vertices = []
          ordered_edges = []
          
          # Start with the first valid edge
          current_edge = valid_chain.first
          
          # Skip if the first edge is somehow invalid
          next unless current_edge && (current_edge.valid? rescue false)
          
          # Get a valid start vertex
          current_vertex = nil
          begin
            current_vertex = current_edge.start
            next unless current_vertex && (current_vertex.valid? rescue false)
          rescue => e
            ActionLogger.log_error('get_start_vertex', e)
            next
          end
          
          # Trace the entire loop, following connected edges with extensive error handling
          valid_loop = true
          begin
            valid_chain.length.times do
              # Add current vertex to the vertices list if it's valid
              if current_vertex && (current_vertex.valid? rescue false)
                vertices << current_vertex
                ordered_edges << current_edge
              else
                valid_loop = false
                break
              end
              
              # Find the next edge connected to the current vertex
              begin
                next_edge = find_next_edge(current_vertex, current_edge, valid_chain)
                break unless next_edge && (next_edge.valid? rescue false)
                
                # Find the vertex at the other end of the next edge
                next_vertex = nil
                begin
                  next_vertex = other_vertex(next_edge, current_vertex)
                  break unless next_vertex && (next_vertex.valid? rescue false)
                  
                  # Continue the chain
                  current_edge = next_edge
                  current_vertex = next_vertex
                rescue => e
                  ActionLogger.log_error('get_next_vertex', e)
                  valid_loop = false
                  break
                end
              rescue => e
                ActionLogger.log_error('find_next_edge', e)
                valid_loop = false
                break
              end
            end
          rescue => e
            ActionLogger.log_error('trace_edge_loop', e)
            valid_loop = false
          end
          
          # Skip if we couldn't trace a valid loop
          next unless valid_loop && vertices.size >= 3
          
          # Verify we have a true closed loop (last vertex connects back to first)
          begin
            # Skip if the first or last vertex is invalid
            next unless vertices.first && (vertices.first.valid? rescue false) && 
                        vertices.last && (vertices.last.valid? rescue false)
            
            # Get the positions
            first_pos = vertices.first.position
            last_pos = vertices.last.position
            
            # Check if the loop is closed (endpoints are close enough)
            if first_pos.distance(last_pos) <= 0.001.inch
              # Get unique positions while preserving order (required for face creation)
              position_points = []
              seen_positions = {}
              
              # Collect valid positions
              vertices.each do |v|
                begin
                  next unless v && (v.valid? rescue false)
                  
                  # Get the position
                  pos = v.position
                  
                  # Skip duplicate positions while preserving order
                  rounded_pos = [pos.x.round(6), pos.y.round(6), pos.z.round(6)]
                  
                  unless seen_positions[rounded_pos]
                    position_points << pos
                    seen_positions[rounded_pos] = true
                  end
                rescue => e
                  ActionLogger.log_error('collect_positions', e)
                end
              end
              
              # Make sure we have at least 3 unique points for a valid face
              if position_points.size >= 3
                # Check if points are roughly coplanar (using the normal calculation)
                begin
                  # Calculate an approximate normal by using the first three points
                  p1, p2, p3 = position_points[0], position_points[1], position_points[2]
                  v1 = p2 - p1
                  v2 = p3 - p1
                  normal = v1 * v2  # Cross product
                  
                  # Skip if normal is too short (indicates collinear points)
                  next if normal.length < 0.0001.inch
                  
                  # Simple test for self-intersection - check if any non-adjacent edges cross
                  has_self_intersection = false
                  
                  if position_points.size > 3
                    # Perform a basic self-intersection test
                    begin
                      # Very simple test: just check if any non-adjacent line segments intersect
                      # This is not comprehensive but catches many issues
                      (0...position_points.size).each do |i|
                        next_i = (i + 1) % position_points.size
                        p1, p2 = position_points[i], position_points[next_i]
                        
                        ((i + 2) % position_points.size...((i - 1) % position_points.size)).each do |j|
                          j = j % position_points.size
                          next_j = (j + 1) % position_points.size
                          p3, p4 = position_points[j], position_points[next_j]
                          
                          # Skip adjacent edges
                          next if next_i == j || i == next_j
                          
                          # Very simple intersection test in 2D (ignore Z)
                          begin
                            if segments_intersect_2d(p1, p2, p3, p4)
                              has_self_intersection = true
                              break
                            end
                          rescue => e
                            ActionLogger.log_error('segment_intersection', e)
                          end
                        end
                        break if has_self_intersection
                      end
                    rescue => e
                      ActionLogger.log_error('self_intersection_check', e)
                    end
                  end
                  
                  # Skip if self-intersection detected
                  next if has_self_intersection
                  
                  # Create the face
                  face = nil
                  begin
                    face = ents.add_face(position_points)
                    
                    # Check if face was created successfully
                    if face && (face.valid? rescue false)
                      # Add face to logging array if enabled
                      created_faces << face if created_faces
                      
                      faces_created += 1
                      
                      # Orient face normal to point "upward" if possible
                      if face.normal.z < 0
                        face.reverse!
                      end
                    end
                  rescue => e
                    ActionLogger.log_error('face_creation', e)
                  end
                rescue => e
                  # Some loops may have numerical issues, we just skip those
                  ActionLogger.log_error('face_geometry_calculation', e)
                end
              end
            end
          rescue => e
            ActionLogger.log_error('check_closed_loop', e)
          end
        end
      rescue => e
        ActionLogger.log_error('build_face_from_chain', e)
      end
    end
    
    faces_created
  end
  
  # Helper method to check if two 2D line segments intersect
  # @param p1 [Geom::Point3d] first point of first segment
  # @param p2 [Geom::Point3d] second point of first segment
  # @param p3 [Geom::Point3d] first point of second segment
  # @param p4 [Geom::Point3d] second point of second segment
  # @return [Boolean] true if segments intersect in 2D (ignoring Z)
  def self.segments_intersect_2d(p1, p2, p3, p4)
    # Calculate direction vectors
    d1 = Geom::Vector3d.new(p2.x - p1.x, p2.y - p1.y, 0)
    d2 = Geom::Vector3d.new(p4.x - p3.x, p4.y - p3.y, 0)
    d3 = Geom::Vector3d.new(p3.x - p1.x, p3.y - p1.y, 0)
    
    # Calculate determinants
    denom = (d2.y * d1.x) - (d2.x * d1.y)
    return false if denom.abs < 0.0001  # Parallel or collinear
    
    ua = ((d2.x * d3.y) - (d2.y * d3.x)) / denom
    ub = ((d1.x * d3.y) - (d1.y * d3.x)) / denom
    
    # Check if intersection point is within both segments
    ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1
  end
  
  # Merge vertices that are closer than tolerance
  # @param ents [Sketchup::Entities] entities collection
  # @param edges [Array<Sketchup::Edge>] edges to process
  # @param tolerance [Float] distance tolerance for merging
  # @param vertex_pairs_merged [Array] optional array to store merged vertex pairs for logging
  # @return [Integer] number of vertices merged
  def self.merge_close_vertices!(ents, edges, tolerance, vertex_pairs_merged = nil)
    # Safety check for inputs
    return 0 unless ents && edges
    
    # Build spatial index for vertices (rounded coordinates → vertex)
    v_index = Hash.new { |h, k| h[k] = [] }
    
    # Update our edges list after removing strays - skip invalid edges
    edges = edges.select { |edge| edge && edge.valid? rescue false }
    return 0 if edges.empty?
    
    # Collect all vertices from remaining edges, checking validity
    vertices = []
    edges.each do |edge|
      begin
        if edge.valid? && edge.start && edge.start.valid?
          vertices << edge.start unless vertices.include?(edge.start)
        end
        
        if edge.valid? && edge.end && edge.end.valid?
          vertices << edge.end unless vertices.include?(edge.end)
        end
      rescue => e
        # Skip edges with deleted vertices
        ActionLogger.log_error('collect_vertices', e)
      end
    end
    
    # Skip if no valid vertices to process
    return 0 if vertices.empty?
    
    # Index vertices by rounded coordinates for quick lookup
    scale = 1.0 / tolerance
    vertices.each do |v|
      begin
        next unless v && v.valid?
        position = v.position
        key = [
          (position.x * scale).round,
          (position.y * scale).round,
          (position.z * scale).round
        ]
        v_index[key] << v
      rescue => e
        # Skip vertices that can't be indexed
        ActionLogger.log_error('vertex_indexing', e)
      end
    end
    
    # Prepare edge operations for vertex merging
    edges_to_create = []
    edges_to_erase_for_merge = []
    merged_count = 0
    
    v_index.each_value do |vertex_list|
      # Skip if we don't have multiple vertices at this location
      next if vertex_list.size <= 1
      
      # Filter for valid vertices only
      valid_vertices = vertex_list.select { |v| v && v.valid? rescue false }
      next if valid_vertices.size <= 1
      
      # Use the first vertex as the "master" vertex
      master = valid_vertices.first
      next unless master.valid?
      
      # Move all other vertices to the master position
      valid_vertices[1..-1].each do |v|
        begin
          next unless v.valid?
          
          # Record the vertex pair being merged if logging is enabled
          vertex_pairs_merged << [master, v] if vertex_pairs_merged
          
          # Find all edges connected to this vertex
          connected_edges = v.edges.select { |e| e && e.valid? rescue false }
          
          connected_edges.each do |edge|
            begin
              next unless edge.valid?
              
              # Store edge to be erased
              edges_to_erase_for_merge << edge
              
              # Get the other endpoint, checking for validity
              other_end = nil
              if edge.start == v && edge.end && edge.end.valid?
                other_end = edge.end
              elsif edge.end == v && edge.start && edge.start.valid?
                other_end = edge.start
              end
              
              # Skip if no valid other endpoint
              next unless other_end && other_end.valid?
              
              # Only create if this connection doesn't already exist
              # Check if master already has an edge to other_end
              existing_connection = false
              master_edges = master.edges.select { |e| e && e.valid? rescue false }
              
              master_edges.each do |e|
                if (e.end == other_end || e.start == other_end)
                  existing_connection = true
                  break
                end
              end
              
              unless existing_connection
                edges_to_create << [master.position, other_end.position]
              end
            rescue => e
              # Skip edges that can't be processed
              ActionLogger.log_error('edge_processing', e)
            end
          end
          merged_count += 1
        rescue => e
          # Skip vertices that can't be merged
          ActionLogger.log_error('vertex_merging', e)
        end
      end
    end
    
    # Apply vertex merging operations
    # Safely erase edges
    begin
      erase_edges(edges_to_erase_for_merge)
    rescue => e
      ActionLogger.log_error('erase_edges_for_merge', e)
    end
    
    # Safely create new edges
    begin
      create_edges(ents, edges_to_create)
    rescue => e
      ActionLogger.log_error('create_edges_for_merge', e)
    end
    
    merged_count
  end
  
  # Helper method to get the "other" vertex of an edge
  # @param edge [Sketchup::Edge] the edge
  # @param vertex [Sketchup::Vertex] one vertex of the edge
  # @return [Sketchup::Vertex] the other vertex of the edge
  def self.other_vertex(edge, vertex)
    return nil if edge.nil? || vertex.nil?
    edge.start == vertex ? edge.end : edge.start
  end
  
  # Helper method to check if a chain is closed
  # @param chain [Array<Sketchup::Edge>] chain of edges
  # @return [Boolean] true if chain forms a closed loop
  def self.chain_is_closed?(chain)
    return false if chain.nil? || chain.empty?
    
    # Get the endpoints of the first and last edges
    first_edge = chain.first
    last_edge = chain.last
    
    return false if first_edge.nil? || last_edge.nil?
    
    # Check if they share an endpoint
    first_edge.start == last_edge.start ||
    first_edge.start == last_edge.end ||
    first_edge.end == last_edge.start ||
    first_edge.end == last_edge.end
  end
  
  # Helper method to find the next edge in a chain
  # @param vertex [Sketchup::Vertex] current vertex
  # @param current_edge [Sketchup::Edge] current edge
  # @param chain [Array<Sketchup::Edge>] chain of edges
  # @return [Sketchup::Edge, nil] the next edge or nil if none found
  def self.find_next_edge(vertex, current_edge, chain)
    return nil if vertex.nil? || current_edge.nil? || chain.nil?
    
    chain.each do |edge|
      next if edge.nil? || edge == current_edge || !edge.valid?
      return edge if edge.start == vertex || edge.end == vertex
    end
    nil
  end
  
  # Build enhanced edge chains with better connectivity detection
  # @param edges [Array<Sketchup::Edge>] edges to process
  # @return [Array<Array<Sketchup::Edge>>] array of edge chains
  def self.build_enhanced_edge_chains(edges)
    # Build a graph representation of edges
    graph = Hash.new { |h, k| h[k] = [] }
    
    # We'll modify this collection, so convert to array first
    edges = edges.to_a
    
    edges.each do |edge|
      next unless edge.valid?
      graph[edge.start] << [edge.end, edge]
      graph[edge.end] << [edge.start, edge]
    end
    
    # Find all vertices with exactly one connection (endpoints)
    endpoints = graph.select { |v, connections| connections.size == 1 }.keys
    
    # Start chains from endpoints for better results
    chains = []
    visited_edges = {}
    
    # First start with endpoints
    endpoints.each do |start_v|
      next if graph[start_v].empty?
      
      # Get the edge from this endpoint
      v2, edge = graph[start_v].first
      next if visited_edges[edge]
      
      # Start a new chain
      chain = [edge]
      visited_edges[edge] = true
      
      # Traverse from v2
      current_v = v2
      prev_v = start_v
      
      # Follow the chain until we hit an endpoint or junction
      loop do
        # Find edges from current_v that aren't the one we just came from
        next_edges = graph[current_v].reject { |_, e| visited_edges[e] }
        
        # If no edges or a junction (more than 1 option), we're done
        break if next_edges.empty? || next_edges.size > 1
        
        # Add this edge to our chain
        next_v, next_edge = next_edges.first
        chain << next_edge
        visited_edges[next_edge] = true
        
        # Move to the next vertex
        prev_v = current_v
        current_v = next_v
      end
      
      chains << chain
    end
    
    # Then handle any remaining edges (loops and isolated segments)
    edges.each do |edge|
      next unless edge.valid?
      next if visited_edges[edge]
      
      # Start a new chain
      chain = [edge]
      visited_edges[edge] = true
      
      # Try to extend in both directions
      [
        [edge.end, edge.start],
        [edge.start, edge.end]
      ].each do |current_v, prev_v|
        # Follow the chain until we hit an endpoint or junction
        loop do
          # Find edges from current_v that aren't the one we just came from
          next_edges = graph[current_v].reject { |_, e| visited_edges[e] }
          
          # If no edges or a junction (more than 1 option), we're done
          break if next_edges.empty? || next_edges.size > 1
          
          # Add this edge to our chain (in the correct order)
          next_v, next_edge = next_edges.first
          
          if current_v == edge.end && prev_v == edge.start
            chain << next_edge # Append to the end
          else
            chain.unshift(next_edge) # Prepend to the start
          end
          
          visited_edges[next_edge] = true
          
          # Move to the next vertex
          prev_v = current_v
          current_v = next_v
        end
      end
      
      chains << chain
    end
    
    chains
  end
  
  # Merge colinear segments within a chain
  # @param chain [Array<Sketchup::Edge>] chain of edges
  # @param ents [Sketchup::Entities] entities collection to modify
  # @param angle_tolerance [Float] angle tolerance in radians
  # @param colinear_edges_merged [Array] optional array to store merged edge groups for logging
  # @return [Integer] number of segments merged
  def self.merge_colinear_segments(chain, ents, angle_tolerance, colinear_edges_merged = nil)
    # Basic validation
    return 0 if chain.nil? || ents.nil? || chain.size < 2
    
    # Remove any nil entries from the chain
    chain.compact!
    
    # Ensure we're working with valid edges
    chain.select! { |edge| edge && (edge.valid? rescue false) }
    return 0 if chain.size < 2
    
    merged_count = 0
    index = 0
    
    while index < chain.size - 1
      # Skip if either edge is no longer valid - using rescue to handle any errors
      current_edge = chain[index]
      next_edge = chain[index + 1]
      
      # First validity check with rescue
      begin
        # This will raise an error if either edge is invalid
        unless current_edge && next_edge && current_edge.valid? && next_edge.valid?
          index += 1
          next
        end
      rescue => e
        # If any error occurs during validity checking, log and skip
        ActionLogger.log_error('edge_validity_check', e)
        index += 1
        next
      end
      
      # Now verify edge endpoints exist and are valid
      begin
        unless current_edge.start && current_edge.end && 
               next_edge.start && next_edge.end &&
               current_edge.start.valid? && current_edge.end.valid? && 
               next_edge.start.valid? && next_edge.end.valid?
          index += 1
          next
        end
      rescue => e
        # If any error occurs during endpoint checking, log and skip
        ActionLogger.log_error('endpoint_validity_check', e)
        index += 1
        next
      end
      
      # Find the shared vertex between edges - with extensive error handling
      shared_vertex = nil
      
      # Safely find shared vertex
      begin
        # Try to find shared vertex safely, checking each possibility with rescue
        if (current_edge.start == next_edge.start rescue false) || 
           (current_edge.start == next_edge.end rescue false)
          shared_vertex = current_edge.start
        elsif (current_edge.end == next_edge.start rescue false) || 
              (current_edge.end == next_edge.end rescue false)
          shared_vertex = current_edge.end
        end
        
        # Skip if no shared vertex or it's invalid
        unless shared_vertex && shared_vertex.valid?
          index += 1
          next
        end
      rescue => e
        ActionLogger.log_error('find_shared_vertex', e)
        index += 1
        next
      end
      
      # Variables for edge direction calculations
      dir1 = nil
      dir2 = nil
      
      # Safely get position for each vertex
      begin
        start_pos = shared_vertex.position
        
        # Get positions of other endpoints, with validity checks
        p1 = nil
        p2 = nil
        
        if (current_edge.start == shared_vertex rescue false)
          if current_edge.end && current_edge.end.valid?
            p1 = current_edge.end.position
          else
            # Invalid endpoint, skip
            index += 1
            next
          end
        elsif (current_edge.end == shared_vertex rescue false)
          if current_edge.start && current_edge.start.valid?
            p1 = current_edge.start.position
          else
            # Invalid endpoint, skip
            index += 1
            next
          end
        end
        
        if (next_edge.start == shared_vertex rescue false)
          if next_edge.end && next_edge.end.valid?
            p2 = next_edge.end.position
          else
            # Invalid endpoint, skip
            index += 1
            next
          end
        elsif (next_edge.end == shared_vertex rescue false)
          if next_edge.start && next_edge.start.valid?
            p2 = next_edge.start.position
          else
            # Invalid endpoint, skip
            index += 1
            next
          end
        end
        
        # Skip if we couldn't get valid endpoints
        unless p1 && p2
          index += 1
          next
        end
        
        # Calculate directions
        dir1 = (p1 - start_pos).normalize
        dir2 = (p2 - start_pos).normalize
        
        # Check if directions are valid vectors
        unless dir1.valid? && dir2.valid?
          index += 1
          next
        end
        
        # Calculate angle between directions
        angle = dir1.angle_between(dir2)
      rescue => e
        ActionLogger.log_error('calculate_directions', e)
        index += 1
        next
      end
      
      # Check if angles indicate colinear segments (nearly opposite directions)
      begin
        if (angle > 3.14159 - angle_tolerance) || (angle < angle_tolerance)
          # At this point we have valid p1 and p2 from above
          
          # Create the replacement edge inside a begin/rescue block
          new_edge = nil
          begin
            # Create new edge connecting the non-shared endpoints
            new_edge = ents.add_line(p1, p2)
            
            # Record the edges being merged if logging is enabled
            merged_edges = [current_edge, next_edge]
            
            # Copy properties from the original edge (layer, etc) if still valid
            if current_edge && current_edge.valid? && current_edge.layer
              new_edge.layer = current_edge.layer
            end
            
            # Safely erase the original edges
            begin
              if current_edge && (current_edge.valid? rescue false)
                current_edge.erase!
              end
            rescue => e
              ActionLogger.log_error('erase_current_edge', e)
            end
            
            begin
              if next_edge && (next_edge.valid? rescue false)
                next_edge.erase!
              end
            rescue => e
              ActionLogger.log_error('erase_next_edge', e)
            end
            
            # Update the chain - replace both edges with the new one
            chain[index] = new_edge
            chain.delete_at(index + 1)  # Remove the second edge
            
            # Log the merged edges if requested
            if colinear_edges_merged
              colinear_edges_merged << merged_edges
            end
            
            merged_count += 1
            
            # Don't increment index since we've modified the array
            # and the next edge is now at the current position
          rescue => e
            # If creating the new edge fails, skip this pair
            ActionLogger.log_error('create_merged_edge', e)
            index += 1
          end
        else
          # Not colinear, move to next pair
          index += 1
        end
      rescue => e
        # Catch any other errors in the colinearity check and merge process
        ActionLogger.log_error('merge_colinear_process', e)
        index += 1
      end
    end
    
    # Ensure we return a valid number
    merged_count
  end
  
  # Extend chains where endpoints are close to each other
  # @param chains [Array<Array<Sketchup::Edge>>] array of edge chains
  # @param ents [Sketchup::Entities] entities collection to modify
  # @param model [Sketchup::Model] model for creating temp groups
  # @param extended_pairs [Array] optional array to store extended edge pairs for logging
  # @return [Integer] number of extensions made
  def self.extend_enhanced_chains(chains, ents, model, extended_pairs = nil)
    return 0 if chains.nil? || ents.nil? || model.nil?
    
    # Remove any nils from chains
    chains.each(&:compact!)
    
    # Skip empty chains
    chains.reject!(&:empty?)
    return 0 if chains.empty?
    
    # Find all endpoint vertices of chains
    endpoints = []
    
    chains.each do |chain|
      # Skip empty or invalid chains
      next if chain.empty? || !chain.first.valid?
      
      # Skip closed loops
      next if chain_is_closed?(chain)
      
      # Get first and last edges
      first_edge = chain.first
      last_edge = chain.last
      
      next unless first_edge && last_edge && first_edge.valid? && last_edge.valid?
      
      # Find disconnected endpoints of the chain
      if chain.size == 1
        # Single edge chain - both endpoints are free
        endpoints << { vertex: first_edge.start, edge: first_edge, chain: chain }
        endpoints << { vertex: first_edge.end, edge: first_edge, chain: chain }
      else
        # First endpoint:
        # Find which vertex of the first edge doesn't connect to the second edge
        free_vertex = nil
        
        if chain.size > 1 && chain[1] && chain[1].valid?
          second_edge = chain[1]
          if first_edge.start == second_edge.start || first_edge.start == second_edge.end
            free_vertex = first_edge.end
          else
            free_vertex = first_edge.start
          end
        else
          # Default to start if we can't determine
          free_vertex = first_edge.start
        end
        
        endpoints << { vertex: free_vertex, edge: first_edge, chain: chain, position: :start }
        
        # Last endpoint:
        # Find which vertex of the last edge doesn't connect to the second-to-last edge
        free_vertex = nil
        
        if chain.size > 1 && chain[-2] && chain[-2].valid?
          second_last_edge = chain[-2]
          if last_edge.start == second_last_edge.start || last_edge.start == second_last_edge.end
            free_vertex = last_edge.end
          else
            free_vertex = last_edge.start
          end
        else
          # Default to end if we can't determine
          free_vertex = last_edge.end
        end
        
        endpoints << { vertex: free_vertex, edge: last_edge, chain: chain, position: :end }
      end
    end
    
    # Skip if no endpoints
    return 0 if endpoints.empty?
    
    # Create a spatial index for finding nearby endpoints
    extension_dist = 0.1.inch  # Starting small - we'll increase this if needed
    max_dist = MAX_EXTENSION_DISTANCE
    extension_count = 0
    
    # Attempt extension at increasing distances
    while extension_dist < max_dist && extension_count == 0
      # Double the distance each time
      extension_dist *= 2
      
      # Try to extend endpoints that are within our current distance
      endpoints.combination(2).each do |e1, e2|
        # Skip if either vertex is no longer valid
        next unless e1[:vertex].valid? && e2[:vertex].valid?
        
        # Skip if these are from the same chain
        next if e1[:chain] == e2[:chain]
        
        # Check distance between vertices
        p1 = e1[:vertex].position
        p2 = e2[:vertex].position
        dist = p1.distance(p2)
        
        if dist <= extension_dist
          # These endpoints are close enough to connect
          begin
            # Create new edge connecting these endpoints
            new_edge = ents.add_line(p1, p2)
            
            # Inherit layer from one of the original edges
            if e1[:edge].layer
              new_edge.layer = e1[:edge].layer
            elsif e2[:edge].layer
              new_edge.layer = e2[:edge].layer
            end
            
            # Add this edge to both chains
            if e1[:position] == :start
              e1[:chain].unshift(new_edge)
            else
              e1[:chain] << new_edge
            end
            
            if e2[:position] == :start
              e2[:chain].unshift(new_edge)
            else
              e2[:chain] << new_edge
            end
            
            # Record the extension if logging is enabled
            if extended_pairs
              extended_pairs << [new_edge, e1[:edge]]
              extended_pairs << [new_edge, e2[:edge]]
            end
            
            extension_count += 1
          rescue => e
            # If extension fails, just continue with other pairs
            ActionLogger.log_error('chain_extension', e)
          end
        end
      end
    end
    
    extension_count
  end
