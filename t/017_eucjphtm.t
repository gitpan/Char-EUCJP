# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{あ} ne "\xa4\xa2";

use Char::EUCJP;
print "1..1\n";

# マッチするはずなのにマッチしない（１）
if ("運転免許" =~ /運転/) {
    print qq<ok - 1 "UNTENMENKYO" =~ /UNTEN/\n>;
}
else {
    print qq<not ok - 1 "UNTENMENKYO" =~ /UNTEN/\n>;
}

__END__

Shift-JISテキストを正しく扱う
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
