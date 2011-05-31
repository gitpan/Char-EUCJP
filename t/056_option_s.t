# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use Char::EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

# s///m
$a = "ABCDEFG\nHIJKLMNOPQRSTUVWXYZ";
if ($a =~ s/^HI/������/m) {
    if ($a eq "ABCDEFG\n������JKLMNOPQRSTUVWXYZ") {
        print qq{ok - 1 \$a =~ s/^HI/������/m ($a) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 1 \$a =~ s/^HI/������/m ($a) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 1 \$a =~ s/^HI/������/m ($a) $^X $__FILE__\n};
}

__END__