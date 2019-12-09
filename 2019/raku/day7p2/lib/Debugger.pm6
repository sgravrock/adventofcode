use v6;

grammar Command {
	token TOP { | <quit> | <step> | <run> | <mem> }
	token quit { | 'q' | 'quit' }
	token step { | 's' | 'step' }
	token run { | 'r' | 'run' }
	rule mem { 'mem' <start=.number> <end=.number> }
	token number {  \d+ }
}

sub debug($machine) is export {
	show-machine-state $machine;

	loop {
		my $line = prompt "debugger> " or return;
		my $cmd = Command.parse($line);

		if $cmd.<quit> {
			return;
		} elsif $cmd.<step> {
			step $machine;
		} elsif $cmd.<run> {
			run $machine;
		} elsif $cmd.<mem> {
			if $cmd.<mem>.<start> < $cmd.<mem>.<end> {
				show-mem $machine, $cmd.<mem>.<start>, $cmd.<mem>.<end>;
			} else {
				say "The first address must be before the second.";
			}
		} else {
			say "Commands:";
			say "mem [start end]: dump memory";
			say "s, step:         perform the instruction at ip";
			say "r, run:          run from current position";
			say "q, quit:         exit";
		
		}
	}
}

sub show-machine-state($machine) {
	"%d words of memory\n".printf($machine.mem.elems);
	"ip=%d\n".printf($machine.ip);
	"mem[ip..ip+3]=%d %d %d %d\n".printf(
		$machine.mem[$machine.ip],
		$machine.mem[$machine.ip + 1],
		$machine.mem[$machine.ip + 2],
		$machine.mem[$machine.ip + 3]
	);
}

sub show-mem($machine, $start, $end) {
	for $start..$end {
		my $word = $machine.mem[$_];
		print "$word ";
	}

	print "\n";
}

sub run($machine) {
	try { $machine.execute(); }
	if $! { say $!.message; }
}

sub step($machine) {
	try { $machine.step(); }
	if $! { say $!.message; }

	show-machine-state $machine;
}
