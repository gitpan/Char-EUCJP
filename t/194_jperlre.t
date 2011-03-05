# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{あ} ne "\xa4\xa2";

use Char::EUCJP;
print "1..6\n";

my $__FILE__ = __FILE__;

if ('アABC DEF GHI' =~ /\bABC/) {
    print "ok - 1 $^X $__FILE__ ('アABC DEF GHI' =~ /\\bABC/)\n";
}
else {
    print "not ok - 1 $^X $__FILE__ ('アABC DEF GHI' =~ /\\bABC/)\n";
}

if ('アABC DEF GHI' =~ /\bDEF/) {
    print "ok - 2 $^X $__FILE__ ('アABC DEF GHI' =~ /\\bDEF/)\n";
}
else {
    print "not ok - 2 $^X $__FILE__ ('アABC DEF GHI' =~ /\\bDEF/)\n";
}

if ('アABC DEF GHI' =~ /\bGHI/) {
    print "ok - 3 $^X $__FILE__ ('アABC DEF GHI' =~ /\\bGHI/)\n";
}
else {
    print "not ok - 3 $^X $__FILE__ ('アABC DEF GHI' =~ /\\bGHI/)\n";
}

if ('アABC DEF GHI' =~ /ABC\b/) {
    print "ok - 4 $^X $__FILE__ ('アABC DEF GHI' =~ /ABC\\b/)\n";
}
else {
    print "not ok - 4 $^X $__FILE__ ('アABC DEF GHI' =~ /ABC\\b/)\n";
}

if ('アABC DEF GHI' =~ /DEF\b/) {
    print "ok - 5 $^X $__FILE__ ('アABC DEF GHI' =~ /DEF\\b/)\n";
}
else {
    print "not ok - 5 $^X $__FILE__ ('アABC DEF GHI' =~ /DEF\\b/)\n";
}

if ('アABC DEF GHI' =~ /GHI\b/) {
    print "ok - 6 $^X $__FILE__ ('アABC DEF GHI' =~ /GHI\\b/)\n";
}
else {
    print "not ok - 6 $^X $__FILE__ ('アABC DEF GHI' =~ /GHI\\b/)\n";
}

__END__
