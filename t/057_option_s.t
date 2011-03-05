# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{あ} ne "\xa4\xa2";

use Char::EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

# s///g
$a = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

if ($a =~ s/CD|JK|UV/あいう/g) {
    if ($a eq "ABあいうEFGHIあいうLMNOPQRSTあいうWXYZ") {
        print qq{ok - 1 \$a =~ s/CD|JK|UV/あいう/g ($a) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 1 \$a =~ s/CD|JK|UV/あいう/g ($a) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 1 \$a =~ s/CD|JK|UV/あいう/g ($a) $^X $__FILE__\n};
}

__END__
