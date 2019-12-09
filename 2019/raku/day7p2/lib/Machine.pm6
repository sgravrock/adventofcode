use v6;

class Machine is export {
	has Int @.mem;
	has $.ip;

	method new(@program) {
		self.bless( mem => @program.clone, ip => 0 );
	}

	method execute() {
		while self.mem[self.ip] != 99 {
			self!do-current-instruction();
		}
	}

	method !do-current-instruction() {
		my $opcode = self.mem[self.ip];
		given $opcode {
			when 1 {
				self!poke(3, self!peek(1) + self!peek(2));
				$!ip += 4;
			}
			when 2 {
				self!poke(3, self!peek(1) * self!peek(2));
				$!ip += 4;
			}
			when 99 {}
			default { die "Invalid opcode $opcode" }
		}
	}

	method !peek($ip-offset) {
		@!mem[@!mem[$!ip + $ip-offset]];
	}

	method !poke($ip-offset, $value) {
		@!mem[@!mem[$!ip + $ip-offset]] = $value;
	}
}
