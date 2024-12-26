#!/usr/bin/env ruby

ORIENTATIONS = [:north, :south, :west, :east]
TURN_COST = 1000

Coord = Struct.new(:x, :y)

Node = Struct.new(:position, :orientation) do
	def neighbor_in_line
		c = case orientation
		when :north
			Coord.new(position.x, position.y - 1)
		when :south
			Coord.new(position.x, position.y + 1)
		when :west
			Coord.new(position.x - 1, position.y)
		when :east
			Coord.new(position.x + 1, position.y)
		else
			raise "Unknown orientaion #{orientation}"
		end
	
		Node.new(c, orientation)
	end
end

def horizontal?(orientation)
	orientation == :west || orientation == :east
end

def parse_input(input)
	start = nil
	goal = nil
	vacant_spaces = []
	lines = input.split

	lines.each_with_index do |line, y|
		line.chars.each_with_index do |c, x|
			if c == '.' || c == 'S' || c == 'E'
				coord = Coord.new(x, y)
				vacant_spaces.push(coord)

				if c == 'S'
					start = coord
				elsif c == 'E'
					goal = coord
				end
			end
		end
	end

	[vacant_spaces, start, goal]
end

def spaces_to_nodes(spaces)
	spaces.flat_map { |space|
		ORIENTATIONS.map { |o| Node.new(space, o) }
	}
end

# Returns a hash of src => [dest, cost] pairs
def nodes_to_edges(nodes)
	nodes = Set.new(nodes)
	edges = Hash.new
	puts "Building edges among #{nodes.length} nodes"

	nodes.each do |src|
		edges[src] = []

		if nodes.include?(src.neighbor_in_line)
			edges[src].push([src.neighbor_in_line, 1])
		end

		ORIENTATIONS.each do |o|
			if o != src.orientation
				cost = TURN_COST

				if horizontal?(o) == horizontal?(src.orientation)
					cost *= 2
				end

				edges[src].push([Node.new(src.position, o), cost])
			end
		end
	end

	edges
end

# start: node
# goals: arrays of nodes
# edges: hash of src => [dest, cost] pairs
def dijkstra(start, goals, edges)
	puts "dijkstra-ing"
	unvisited = Set.new(edges.keys)
	costs = Hash.new

	edges.keys.each do |node|
		costs[node] = Float::INFINITY
	end

	costs[start] = 0

	until unvisited.empty?
		current = unvisited.min_by { |n| costs[n] }
		unvisited.delete(current)
		current_cost = costs[current]

		edges[current].each do |dest, cost|
			costs[dest] = [costs[dest], current_cost + cost].min
		end
	end

	goals.map { |goal| costs[goal] }.min
end

vacant_spaces, start_pos, goal_pos = parse_input(ARGF.read)
nodes = spaces_to_nodes(vacant_spaces)
edges = nodes_to_edges(nodes)
puts "edges built"
goals = ORIENTATIONS.map { |o| Node.new(goal_pos, o) }
start = Node.new(start_pos, :east)
result = dijkstra(start, goals, edges).inspect
puts "Result: #{result}"
