# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use Char::EUCJP;
print "1..1\n";

# �ޥå�����Ϥ��ʤΤ˥ޥå����ʤ��ʣ���
if ("��ž�ȵ�" =~ /��ž/) {
    print qq<ok - 1 "UNTENMENKYO" =~ /UNTEN/>;
}
else {
    print qq<not ok - 1 "UNTENMENKYO" =~ /UNTEN/>;
}

__END__

Shift-JIS�ƥ����Ȥ�����������
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm