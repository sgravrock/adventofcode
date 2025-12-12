#!/usr/bin/env ruby


MachineDef = Data.define(:target_lights, :buttons) do
	def initial_state
		MachineState.new(self, target_lights.map { || false })
	end
end

MachineState = Data.define(:definition, :lights) do
	def solved?
		lights == definition.target_lights
	end
	
	def press(button)
		new_lights = lights.clone
		
		button.each do |li|
			new_lights[li] = !new_lights[li]
		end
		
		MachineState.new(definition, new_lights)
	end
end

def parse(input)
	input.split("\n").map { |line|
		words = line.split(" ")
		words.pop # drop irrelevant joltage requirements
		
		# "[.##.]" => [false, true, true, false]
		target_lights = words.shift
			.gsub(/[\[\]]/, '')
			.chars.map { |c| c == '#' }
			
		buttons = words.map { |w|
			# "(1,2,3)" => [1, 2, 3]
			w.gsub(/[\(\)]/, '').split(",").map(&:to_i)
		}
		
		MachineDef.new(target_lights, buttons).initial_state
	}
end

# Returns the number of steps until done? returns true
# done: a function that takes a state and returns boolean
# next_states: a function that takes a state and returns a list of states
#   reachable from there on one move
def bfs(initial_state, done, next_states)
	queue = [[initial_state, 0]]
	seen = Set.new
	
	until queue.empty?
		state, n_moves = queue.shift
		seen.add(state)
		
		if done.call(state)
			return n_moves
		end
		
		next_states.call(state).each do |ns|
			unless seen.include?(ns)
				queue.append([ns, n_moves + 1])
			end
		end
	end
	
	raise "bfs terminated without succeeding"
end

def solve_machine(machine)
	solved = -> (state) { state.solved? }
	next_states = -> (state) { 
		state.definition.buttons.map { |b| state.press(b) }
	}
	bfs(machine, solved, next_states)
end

def solve(input)
	machines = parse(input)
	machines.map { |m| solve_machine(m) }.reduce(&:+)
end

puts(solve(ARGF.read))