# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use Char::EUCJP qw(getc);
print "1..1\n";

my $__FILE__ = __FILE__;

my @getc = ();
while (my $c = getc(DATA)) {
    last if $c eq "\n";
    push @getc, $c;
}
my $result = join('', map {"($_)"} @getc);

if ($result eq '(1)(2)(��)(��)(��)(��)') {
    print "ok - 1 $^X $__FILE__ 12�������� --> $result.\n";
}
else {
    print "not ok - 1 $^X $__FILE__ 12�������� --> $result.\n";
}

__END__
12��������