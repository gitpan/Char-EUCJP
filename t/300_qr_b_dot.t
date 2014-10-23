# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use strict;
use Char::EUCJP;
print "1..2\n";

my $__FILE__ = __FILE__;

if ('��' =~ qr/(.)/b) {
    if (length($1) == 1) {
        print qq{ok - 1 '��'=~qr/(.)/b; length(\$1)==1 $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 1 '��'=~qr/(.)/b; length(\$1)==1 $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 1 '��'=~qr/(.)/b; length(\$1)==1 $^X $__FILE__\n};
}

if ('��' =~ qr'(.)'b) {
    if (length($1) == 1) {
        print qq{ok - 2 '��'=~qr'(.)'b; length(\$1)==1 $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 2 '��'=~qr'(.)'b; length(\$1)==1 $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 2 '��'=~qr'(.)'b; length(\$1)==1 $^X $__FILE__\n};
}

__END__

