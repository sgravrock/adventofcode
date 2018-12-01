use Data::Dumper;
my %h = ( foo => 42 );
my $r = \%h;
print Dumper($r->{foo});
