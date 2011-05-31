# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

my $__FILE__ = __FILE__;

use Char::EUCJP;
print "1..1\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    print "ok - 1 # SKIP $^X $0\n";
    exit;
}

mkdir('directory',0777);
mkdir('D��ǽ',0777);
open(FILE,'>D��ǽ/file1.txt') || die "Can't open file: D��ǽ/file1.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D��ǽ/file2.txt') || die "Can't open file: D��ǽ/file2.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D��ǽ/file3.txt') || die "Can't open file: D��ǽ/file3.txt\n";
print FILE "1\n";
close(FILE);

# mkdir
if (mkdir('D2��ǽ',0777)) {
    print "ok - 1 mkdir $^X $__FILE__\n";
    system('rmdir D2��ǽ 2>NUL');
}
else {
    print "not ok - 1 mkdir: $! $^X $__FILE__\n";
}

unlink('D��ǽ/file1.txt');
unlink('D��ǽ/file2.txt');
unlink('D��ǽ/file3.txt');
rmdir('directory');
rmdir('D��ǽ');

__END__