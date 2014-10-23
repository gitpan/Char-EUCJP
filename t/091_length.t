# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use Char::EUCJP;
print "1..2\n";

my $__FILE__ = __FILE__;

if (length('����������') == 10) {
    print qq{ok - 1 length('����������') == 10 $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 length('����������') == 10 $^X $__FILE__\n};
}

if (Char::EUCJP::length('����������') == 5) {
    print qq{ok - 2 Char::EUCJP::length('����������') == 5 $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 Char::EUCJP::length('����������') == 5 $^X $__FILE__\n};
}

__END__
