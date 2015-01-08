# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use Char::EUCJP;

print "1..12\n";

# Char::EUCJP::eval (omit) has Char::EUCJP::eval "..."
$_ = <<'END';
Char::EUCJP::eval " if ('����' !~ /��/) { return 1 } else { return 0 } "
END
if (Char::EUCJP::eval) {
    print qq{ok - 1 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 1 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has Char::EUCJP::eval qq{...}
$_ = <<'END';
Char::EUCJP::eval qq{ if ('����' !~ /��/) { return 1 } else { return 0 } }
END
if (Char::EUCJP::eval) {
    print qq{ok - 2 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 2 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has Char::EUCJP::eval '...'
$_ = <<'END';
Char::EUCJP::eval ' if (qq{����} !~ /��/) { return 1 } else { return 0 } '
END
if (Char::EUCJP::eval) {
    print qq{ok - 3 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 3 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has Char::EUCJP::eval q{...}
$_ = <<'END';
Char::EUCJP::eval q{ if ('����' !~ /��/) { return 1 } else { return 0 } }
END
if (Char::EUCJP::eval) {
    print qq{ok - 4 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 4 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has Char::EUCJP::eval $var
$_ = <<'END';
Char::EUCJP::eval $var2
END
my $var2 = q{ if ('����' !~ /��/) { return 1 } else { return 0 } };
if (Char::EUCJP::eval) {
    print qq{ok - 5 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 5 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has Char::EUCJP::eval (omit)
$_ = <<'END';
$_ = "if ('����' !~ /��/) { return 1 } else { return 0 }";
Char::EUCJP::eval
END
if (Char::EUCJP::eval) {
    print qq{ok - 6 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 6 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has Char::EUCJP::eval {...}
$_ = <<'END';
Char::EUCJP::eval { if ('����' !~ /��/) { return 1 } else { return 0 } }
END
if (Char::EUCJP::eval) {
    print qq{ok - 7 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 7 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has "..."
$_ = <<'END';
if ('����' !~ /��/) { return "1" } else { return "0" }
END
if (Char::EUCJP::eval) {
    print qq{ok - 8 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 8 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has qq{...}
$_ = <<'END';
if ('����' !~ /��/) { return qq{1} } else { return qq{0} }
END
if (Char::EUCJP::eval) {
    print qq{ok - 9 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 9 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has '...'
$_ = <<'END';
if ('����' !~ /��/) { return '1' } else { return '0' }
END
if (Char::EUCJP::eval) {
    print qq{ok - 10 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 10 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has q{...}
$_ = <<'END';
if ('����' !~ /��/) { return q{1} } else { return q{0} }
END
if (Char::EUCJP::eval) {
    print qq{ok - 11 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 11 $^X @{[__FILE__]}\n};
}

# Char::EUCJP::eval (omit) has $var
$_ = <<'END';
if ('����' !~ /��/) { return $var1 } else { return $var0 }
END
my $var1 = 1;
my $var0 = 0;
if (Char::EUCJP::eval) {
    print qq{ok - 12 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 12 $^X @{[__FILE__]}\n};
}

__END__
