package Char::Eeucjp;
######################################################################
#
# Char::Eeucjp - Run-time routines for Char/EUCJP.pm
#
# Copyright (c) 2008, 2009, 2010, 2011, 2012 INABA Hitoshi <ina@cpan.org>
#
######################################################################

use 5.00503;

BEGIN {
    if ($^X =~ / jperl /oxmsi) {
        die __FILE__, ": needs perl(not jperl) 5.00503 or later. (\$^X==$^X)";
    }
    if (ord('A') == 193) {
        die __FILE__, ": is not US-ASCII script (may be EBCDIC or EBCDIK script).";
    }
    if (ord('A') != 0x41) {
        die __FILE__, ": is not US-ASCII script (must be US-ASCII script).";
    }
}

# 12.3. Delaying use Until Runtime
# in Chapter 12. Packages, Libraries, and Modules
# of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
# (and so on)

BEGIN { eval q{ use vars qw($VERSION) } }
$VERSION = sprintf '%d.%02d', q$Revision: 0.82 $ =~ /(\d+)/xmsg;

BEGIN {
    my $PERL5LIB = __FILE__;

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $PERL5LIB =~ s{[^\x8E\x8F\xA1-\xFE/]*$}{Char::EUCJP};
    }

    # UNIX-like system
    else {
        $PERL5LIB =~ s{[^\x8E\x8F\xA1-\xFE/]*$}{Char::EUCJP};
    }

    my @inc = ();
    my %inc = ();
    for my $path ($PERL5LIB, @INC) {
        if (not exists $inc{$path}) {
            push @inc, $path;
            $inc{$path} = 1;
        }
    }
    @INC = @inc;
}

BEGIN {

    # instead of utf8.pm
    eval q{
        no warnings qw(redefine);
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    };
    if ($@) {
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    }
}

# poor Symbol.pm - substitute of real Symbol.pm
BEGIN {
    my $genpkg = "Symbol::";
    my $genseq = 0;

    sub gensym () {
        my $name = "GEN" . $genseq++;

        # here, no strict qw(refs); if strict.pm exists

        my $ref = \*{$genpkg . $name};
        delete $$genpkg{$name};
        $ref;
    }

    sub qualify ($;$) {
        my ($name) = @_;
        if (!ref($name) && (Char::Eeucjp::index($name, '::') == -1) && (Char::Eeucjp::index($name, "'") == -1)) {
            my $pkg;
            my %global = map {$_ => 1} qw(ARGV ARGVOUT ENV INC SIG STDERR STDIN STDOUT);

            # Global names: special character, "^xyz", or other.
            if ($name =~ /^(([^\x8E\x8F\xA1-\xFEa-z])|(\^[a-z_]+))\z/i || $global{$name}) {
                # RGS 2001-11-05 : translate leading ^X to control-char
                $name =~ s/^\^([a-z_])/'qq(\c'.$1.')'/eei;
                $pkg = "main";
            }
            else {
                $pkg = (@_ > 1) ? $_[1] : caller;
            }
            $name = $pkg . "::" . $name;
        }
        $name;
    }

    sub qualify_to_ref ($;$) {

        # here, no strict qw(refs); if strict.pm exists

        return \*{ qualify $_[0], @_ > 1 ? $_[1] : caller };
    }
}

# use strict; if strict.pm exists
BEGIN {
    if (eval {CORE::require strict}) {
        strict::->import;
    }
}

# P.714 29.2.39. flock
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

# P.863 flock
# in Chapter 27: Functions
# of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

sub LOCK_SH() {1}
sub LOCK_EX() {2}
sub LOCK_UN() {8}
sub LOCK_NB() {4}

# instead of Carp.pm
sub carp(@);
sub croak(@);
sub cluck(@);
sub confess(@);

my $your_char = q{\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[\x00-\xFF]};

# regexp of character
my $q_char = qr/$your_char/oxms;

#
# EUC-JP character range per length
#
my %range_tr = ();

#
# alias of encoding name
#
BEGIN { eval q{ use vars qw($encoding_alias) } }

#
# EUC-JP case conversion
#
my %lc = ();
@lc{qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)} =
    qw(a b c d e f g h i j k l m n o p q r s t u v w x y z);
my %uc = ();
@uc{qw(a b c d e f g h i j k l m n o p q r s t u v w x y z)} =
    qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z);
my %fc = ();
@fc{qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)} =
    qw(a b c d e f g h i j k l m n o p q r s t u v w x y z);

if (0) {
}

elsif (__PACKAGE__ =~ / \b Eeucjp \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x8D],
               [0x90..0xA0],
               [0xFF..0xFF],
             ],
        2 => [ [0x8E..0x8E],[0xA1..0xDF],
               [0xA1..0xFE],[0xA1..0xFE],
             ],
        3 => [ [0x8F..0x8F],[0xA1..0xFE],[0xA1..0xFE],
             ],
    );
    $encoding_alias = qr/ \b (?: euc.*jp | jp.*euc | ujis ) \b /oxmsi;
}

else {
    croak "Don't know my package name '@{[__PACKAGE__]}'";
}

#
# Prototypes of subroutines
#
sub import() {}
sub unimport() {}
sub Char::Eeucjp::split(;$$$);
sub Char::Eeucjp::tr($$$$;$);
sub Char::Eeucjp::chop(@);
sub Char::Eeucjp::index($$;$);
sub Char::Eeucjp::rindex($$;$);
sub Char::Eeucjp::lcfirst(@);
sub Char::Eeucjp::lcfirst_();
sub Char::Eeucjp::lc(@);
sub Char::Eeucjp::lc_();
sub Char::Eeucjp::ucfirst(@);
sub Char::Eeucjp::ucfirst_();
sub Char::Eeucjp::uc(@);
sub Char::Eeucjp::uc_();
sub Char::Eeucjp::fc(@);
sub Char::Eeucjp::fc_();
sub Char::Eeucjp::ignorecase(@);
sub Char::Eeucjp::classic_character_class($);
sub Char::Eeucjp::capture($);
sub Char::Eeucjp::chr(;$);
sub Char::Eeucjp::chr_();
sub Char::Eeucjp::glob($);
sub Char::Eeucjp::glob_();

sub Char::EUCJP::ord(;$);
sub Char::EUCJP::ord_();
sub Char::EUCJP::reverse(@);
sub Char::EUCJP::length(;$);
sub Char::EUCJP::substr($$;$$);
sub Char::EUCJP::index($$;$);
sub Char::EUCJP::rindex($$;$);

#
# Character class
#
BEGIN { eval q{ use vars qw(
    @anchor
    @dot
    @dot_s
    @eD
    @eS
    @eW
    @eH
    @eV
    @eR
    @eN
    @not_alnum
    @not_alpha
    @not_ascii
    @not_blank
    @not_cntrl
    @not_digit
    @not_graph
    @not_lower
    @not_lower_i
    @not_print
    @not_punct
    @not_space
    @not_upper
    @not_upper_i
    @not_word
    @not_xdigit
    @eb
    @eB
) } }
@{Char::Eeucjp::anchor}      = qr{\G(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE])*?};
@{Char::Eeucjp::dot}         = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x0A])};
@{Char::Eeucjp::dot_s}       = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE])};
@{Char::Eeucjp::eD}          = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE0-9])};

@{Char::Eeucjp::eS}          = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x09\x0A\x0C\x0D\x20])};

# Incompatible Changes
# \s in regular expressions now matches a Vertical Tab (experimental)
# http://search.cpan.org/~zefram/perl-5.17.0/pod/perldelta.pod

# @{Char::Eeucjp::eS}        = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x09\x0A\x0B\x0C\x0D\x20])};

@{Char::Eeucjp::eW}          = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE0-9A-Z_a-z])};
@{Char::Eeucjp::eH}          = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x09\x20])};
@{Char::Eeucjp::eV}          = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x0A\x0B\x0C\x0D])};
@{Char::Eeucjp::eR}          = qr{(?:\x0D\x0A|[\x0A\x0D])};
@{Char::Eeucjp::eN}          = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x0A])};
@{Char::Eeucjp::not_alnum}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x30-\x39\x41-\x5A\x61-\x7A])};
@{Char::Eeucjp::not_alpha}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x41-\x5A\x61-\x7A])};
@{Char::Eeucjp::not_ascii}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x00-\x7F])};
@{Char::Eeucjp::not_blank}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x09\x20])};
@{Char::Eeucjp::not_cntrl}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x00-\x1F\x7F])};
@{Char::Eeucjp::not_digit}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x30-\x39])};
@{Char::Eeucjp::not_graph}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x21-\x7F])};
@{Char::Eeucjp::not_lower}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x61-\x7A])};
@{Char::Eeucjp::not_lower_i} = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE])};
@{Char::Eeucjp::not_print}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x20-\x7F])};
@{Char::Eeucjp::not_punct}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E])};
@{Char::Eeucjp::not_space}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x09\x0A\x0B\x0C\x0D\x20])};
@{Char::Eeucjp::not_upper}   = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x41-\x5A])};
@{Char::Eeucjp::not_upper_i} = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE])};
@{Char::Eeucjp::not_word}    = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x30-\x39\x41-\x5A\x5F\x61-\x7A])};
@{Char::Eeucjp::not_xdigit}  = qr{(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\x30-\x39\x41-\x46\x61-\x66])};
@{Char::Eeucjp::eb}          = qr{(?:\A(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[0-9A-Z_a-z])|(?<=[0-9A-Z_a-z])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]|\z))};
@{Char::Eeucjp::eB}          = qr{(?:(?<=[0-9A-Z_a-z])(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]))};

#
# @ARGV wildcard globbing
#
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    if ($ENV{'ComSpec'} =~ / (?: COMMAND\.COM | CMD\.EXE ) \z /oxmsi) {
        my @argv = ();
        for (@ARGV) {
            if (/\A ' ((?:$q_char)*) ' \z/oxms) {
                push @argv, $1;
            }
            elsif (/\A (?:$q_char)*? [*?] /oxms and (my @glob = Char::Eeucjp::glob($_))) {
                push @argv, @glob;
            }
            else {
                push @argv, $_;
            }
        }
        @ARGV = @argv;
    }
}

#
# EUC-JP split
#
sub Char::Eeucjp::split(;$$$) {

    # P.794 29.2.161. split
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    # P.951 split
    # in Chapter 27: Functions
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

    my $pattern = $_[0];
    my $string  = $_[1];
    my $limit   = $_[2];

    # if $string is omitted, the function splits the $_ string
    if (not defined $string) {
        if (defined $_) {
            $string = $_;
        }
        else {
            $string = '';
        }
    }

    my @split = ();

    # when string is empty
    if ($string eq '') {

        # resulting list value in list context
        if (wantarray) {
            return @split;
        }

        # count of substrings in scalar context
        else {
            carp "Use of implicit split to \@_ is deprecated" if $^W;
            @_ = @split;
            return scalar @_;
        }
    }

    # if $limit is negative, it is treated as if an arbitrarily large $limit has been specified
    if ((not defined $limit) or ($limit <= 0)) {

        # if $pattern is also omitted or is the literal space, " ", the function splits
        # on whitespace, /\s+/, after skipping any leading whitespace
        # (and so on)

        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;

            # P.1024 Appendix W.10 Multibyte Processing
            # of ISBN 1-56592-224-7 CJKV Information Processing
            # (and so on)

            # the //m modifier is assumed when you split on the pattern /^/
            # (and so on)

            while ($string =~ s/\A((?:$q_char)*?)\s+//m) {

                # if the $pattern contains parentheses, then the substring matched by each pair of parentheses
                # is included in the resulting list, interspersed with the fields that are ordinarily returned
                # (and so on)

                local $@;
                for (my $digit=1; $digit <= 1; $digit++) {
                    push @split, eval('$' . $digit);
                }
            }
        }

        # a pattern capable of matching either the null string or something longer than the
        # null string will split the value of $string into separate characters wherever it
        # matches the null string between characters
        # (and so on)

        elsif ('' =~ / \A $pattern \z /xms) {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);
            while ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                local $@;
                for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                    push @split, eval('$' . $digit);
                }
            }
        }

        else {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);
            while ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                local $@;
                for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                    push @split, eval('$' . $digit);
                }
            }
        }
    }

    else {
        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)\s+//m) {
                    local $@;
                    for (my $digit=1; $digit <= 1; $digit++) {
                        push @split, eval('$' . $digit);
                    }
                }
            }
        }
        elsif ('' =~ / \A $pattern \z /xms) {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                    local $@;
                    for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                        push @split, eval('$' . $digit);
                    }
                }
            }
        }
        else {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                    local $@;
                    for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                        push @split, eval('$' . $digit);
                    }
                }
            }
        }
    }

    push @split, $string;

    # if $limit is omitted or zero, trailing null fields are stripped from the result
    if ((not defined $limit) or ($limit == 0)) {
        while ((scalar(@split) >= 1) and ($split[-1] eq '')) {
            pop @split;
        }
    }

    # resulting list value in list context
    if (wantarray) {
        return @split;
    }

    # count of substrings in scalar context
    else {
        carp "Use of implicit split to \@_ is deprecated" if $^W;
        @_ = @split;
        return scalar @_;
    }
}

#
# get last subexpression offsets
#
sub _last_subexpression_offsets {
    my $pattern = $_[0];

    # remove comment
    $pattern =~ s/\(\?\# .*? \)//oxmsg;

    my $modifier = '';
    if ($pattern =~ /\(\?\^? ([\-A-Za-z]+) :/oxms) {
        $modifier = $1;
        $modifier =~ s/-[A-Za-z]*//;
    }

    # with /x modifier
    my @char = ();
    if ($modifier =~ /x/oxms) {
        @char = $pattern =~ /\G(
            \\ (?:$q_char)                  |
            \# (?:$q_char)*? $              |
            \[ (?: \\\] | (?:$q_char))+? \] |
            \(\?                            |
            (?:$q_char)
        )/oxmsg;
    }

    # without /x modifier
    else {
        @char = $pattern =~ /\G(
            \\ (?:$q_char)                  |
            \[ (?: \\\] | (?:$q_char))+? \] |
            \(\?                            |
            (?:$q_char)
        )/oxmsg;
    }

    return scalar grep { $_ eq '(' } @char;
}

#
# EUC-JP transliteration (tr///)
#
sub Char::Eeucjp::tr($$$$;$) {

    my $bind_operator   = $_[1];
    my $searchlist      = $_[2];
    my $replacementlist = $_[3];
    my $modifier        = $_[4] || '';

    if ($modifier =~ /r/oxms) {
        if ($bind_operator =~ / !~ /oxms) {
            croak "Using !~ with tr///r doesn't make sense";
        }
    }

    my @char            = $_[0] =~ /\G ($q_char) /oxmsg;
    my @searchlist      = _charlist_tr($searchlist);
    my @replacementlist = _charlist_tr($replacementlist);

    my %tr = ();
    for (my $i=0; $i <= $#searchlist; $i++) {
        if (not exists $tr{$searchlist[$i]}) {
            if (defined $replacementlist[$i] and ($replacementlist[$i] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[$i];
            }
            elsif ($modifier =~ /d/oxms) {
                $tr{$searchlist[$i]} = '';
            }
            elsif (defined $replacementlist[-1] and ($replacementlist[-1] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[-1];
            }
            else {
                $tr{$searchlist[$i]} = $searchlist[$i];
            }
        }
    }

    my $tr = 0;
    my $replaced = '';
    if ($modifier =~ /c/oxms) {
        while (defined(my $char = shift @char)) {
            if (not exists $tr{$char}) {
                if (defined $replacementlist[0]) {
                    $replaced .= $replacementlist[0];
                }
                $tr++;
                if ($modifier =~ /s/oxms) {
                    while (@char and (not exists $tr{$char[0]})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $replaced .= $char;
            }
        }
    }
    else {
        while (defined(my $char = shift @char)) {
            if (exists $tr{$char}) {
                $replaced .= $tr{$char};
                $tr++;
                if ($modifier =~ /s/oxms) {
                    while (@char and (exists $tr{$char[0]}) and ($tr{$char[0]} eq $tr{$char})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $replaced .= $char;
            }
        }
    }

    if ($modifier =~ /r/oxms) {
        return $replaced;
    }
    else {
        $_[0] = $replaced;
        if ($bind_operator =~ / !~ /oxms) {
            return not $tr;
        }
        else {
            return $tr;
        }
    }
}

#
# EUC-JP chop
#
sub Char::Eeucjp::chop(@) {

    my $chop;
    if (@_ == 0) {
        my @char = /\G ($q_char) /oxmsg;
        $chop = pop @char;
        $_ = join '', @char;
    }
    else {
        for (@_) {
            my @char = /\G ($q_char) /oxmsg;
            $chop = pop @char;
            $_ = join '', @char;
        }
    }
    return $chop;
}

#
# EUC-JP index by octet
#
sub Char::Eeucjp::index($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= 0;
    my $pos = 0;

    while ($pos < CORE::length($str)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            if ($pos >= $position) {
                return $pos;
            }
        }
        if (CORE::substr($str,$pos) =~ /\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return -1;
}

#
# EUC-JP reverse index
#
sub Char::Eeucjp::rindex($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= CORE::length($str) - 1;
    my $pos = 0;
    my $rindex = -1;

    while (($pos < CORE::length($str)) and ($pos <= $position)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            $rindex = $pos;
        }
        if (CORE::substr($str,$pos) =~ /\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return $rindex;
}

#
# EUC-JP lower case first with parameter
#
sub Char::Eeucjp::lcfirst(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return Char::Eeucjp::lc(CORE::substr($s,0,1)) . CORE::substr($s,1), @_;
        }
        else {
            return Char::Eeucjp::lc(CORE::substr($s,0,1)) . CORE::substr($s,1);
        }
    }
    else {
        return Char::Eeucjp::lc(CORE::substr($_,0,1)) . CORE::substr($_,1);
    }
}

#
# EUC-JP lower case first without parameter
#
sub Char::Eeucjp::lcfirst_() {
    return Char::Eeucjp::lc(CORE::substr($_,0,1)) . CORE::substr($_,1);
}

#
# EUC-JP lower case with parameter
#
sub Char::Eeucjp::lc(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return join('', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg)), @_;
        }
        else {
            return join('', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg));
        }
    }
    else {
        return Char::Eeucjp::lc_();
    }
}

#
# EUC-JP lower case without parameter
#
sub Char::Eeucjp::lc_() {
    my $s = $_;
    return join '', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg);
}

#
# EUC-JP upper case first with parameter
#
sub Char::Eeucjp::ucfirst(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return Char::Eeucjp::uc(CORE::substr($s,0,1)) . CORE::substr($s,1), @_;
        }
        else {
            return Char::Eeucjp::uc(CORE::substr($s,0,1)) . CORE::substr($s,1);
        }
    }
    else {
        return Char::Eeucjp::uc(CORE::substr($_,0,1)) . CORE::substr($_,1);
    }
}

#
# EUC-JP upper case first without parameter
#
sub Char::Eeucjp::ucfirst_() {
    return Char::Eeucjp::uc(CORE::substr($_,0,1)) . CORE::substr($_,1);
}

#
# EUC-JP upper case with parameter
#
sub Char::Eeucjp::uc(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return join('', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg)), @_;
        }
        else {
            return join('', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg));
        }
    }
    else {
        return Char::Eeucjp::uc_();
    }
}

#
# EUC-JP upper case without parameter
#
sub Char::Eeucjp::uc_() {
    my $s = $_;
    return join '', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg);
}

#
# EUC-JP fold case with parameter
#
sub Char::Eeucjp::fc(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return join('', map {defined($fc{$_}) ? $fc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg)), @_;
        }
        else {
            return join('', map {defined($fc{$_}) ? $fc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg));
        }
    }
    else {
        return Char::Eeucjp::fc_();
    }
}

#
# EUC-JP fold case lower case without parameter
#
sub Char::Eeucjp::fc_() {
    my $s = $_;
    return join '', map {defined($fc{$_}) ? $fc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg);
}

#
# EUC-JP regexp capture
#
{
    # 10.3. Creating Persistent Private Variables
    # in Chapter 10. Subroutines
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.

    my $last_s_matched = 0;

    sub Char::Eeucjp::capture($) {
        if ($last_s_matched and ($_[0] =~ /\A [1-9][0-9]* \z/oxms)) {
            return $_[0] + 1;
        }
        return $_[0];
    }

    # EUC-JP mark last regexp matched
    sub Char::Eeucjp::matched() {
        $last_s_matched = 0;
    }

    # EUC-JP mark last s/// matched
    sub Char::Eeucjp::s_matched() {
        $last_s_matched = 1;
    }

    # P.854 31.17. use re
    # in Chapter 31. Pragmatic Modules
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    # P.1026 re
    # in Chapter 29. Pragmatic Modules
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

    @Char::Eeucjp::matched = (qr/(?{Char::Eeucjp::matched})/);
}

#
# EUC-JP regexp ignore case modifier
#
sub Char::Eeucjp::ignorecase(@) {

    my @string = @_;
    my $metachar = qr/[\@\\|[\]{]/oxms;

    # ignore case of $scalar or @array
    for my $string (@string) {

        # split regexp
        my @char = $string =~ /\G(
            \[\^ |
                \\? (?:$q_char)
        )/oxmsg;

        # unescape character
        for (my $i=0; $i <= $#char; $i++) {
            next if not defined $char[$i];

            # open character class [...]
            if ($char[$i] eq '[') {
                my $left = $i;

                # [] make die "unmatched [] in regexp ..."

                if ($char[$i+1] eq ']') {
                    $i++;
                }

                while (1) {
                    if (++$i > $#char) {
                        croak "Unmatched [] in regexp";
                    }
                    if ($char[$i] eq ']') {
                        my $right = $i;
                        my @charlist = charlist_qr(@char[$left+1..$right-1], 'i');

                        # escape character
                        for my $char (@charlist) {

                            # do not use quotemeta here
                            if ($char =~ /\A ([\x80-\xFF].*) ($metachar) \z/oxms) {
                                $char = $1 . '\\' . $2;
                            }
                            elsif ($char =~ /\A [.|)] \z/oxms) {
                                $char = $1 . '\\' . $char;
                            }
                        }

                        # [...]
                        splice @char, $left, $right-$left+1, '(?:' . join('|', @charlist) . ')';

                        $i = $left;
                        last;
                    }
                }
            }

            # open character class [^...]
            elsif ($char[$i] eq '[^') {
                my $left = $i;

                # [^] make die "unmatched [] in regexp ..."

                if ($char[$i+1] eq ']') {
                    $i++;
                }

                while (1) {
                    if (++$i > $#char) {
                        croak "Unmatched [] in regexp";
                    }
                    if ($char[$i] eq ']') {
                        my $right = $i;
                        my @charlist = charlist_not_qr(@char[$left+1..$right-1], 'i');

                        # escape character
                        for my $char (@charlist) {

                            # do not use quotemeta here
                            if ($char =~ /\A ([\x80-\xFF].*) ($metachar) \z/oxms) {
                                $char = $1 . '\\' . $2;
                            }
                            elsif ($char =~ /\A [.|)] \z/oxms) {
                                $char = '\\' . $char;
                            }
                        }

                        # [^...]
                        splice @char, $left, $right-$left+1, '(?!' . join('|', @charlist) . ")(?:$your_char)";

                        $i = $left;
                        last;
                    }
                }
            }

            # rewrite classic character class or escape character
            elsif (my $char = classic_character_class($char[$i])) {
                $char[$i] = $char;
            }

            # with /i modifier
            elsif ($char[$i] =~ /\A [\x00-\xFF] \z/oxms) {
                my $uc = Char::Eeucjp::uc($char[$i]);
                my $fc = Char::Eeucjp::fc($char[$i]);
                if ($uc ne $fc) {
                    if (CORE::length($fc) == 1) {
                        $char[$i] = '['   . $uc       . $fc . ']';
                    }
                    else {
                        $char[$i] = '(?:' . $uc . '|' . $fc . ')';
                    }
                }
            }
        }

        # characterize
        for (my $i=0; $i <= $#char; $i++) {
            next if not defined $char[$i];

            # escape last octet of multiple-octet
            if ($char[$i] =~ /\A ([\x80-\xFF].*) ($metachar) \z/oxms) {
                $char[$i] = $1 . '\\' . $2;
            }

            # quote character before ? + * {
            elsif (($i >= 1) and ($char[$i] =~ /\A [\?\+\*\{] \z/oxms)) {
                if ($char[$i-1] !~ /\A [\x00-\xFF] \z/oxms) {
                    $char[$i-1] = '(?:' . $char[$i-1] . ')';
                }
            }
        }

        $string = join '', @char;
    }

    # make regexp string
    return @string;
}

#
# classic character class ( \D \S \W \d \s \w \C \X \H \V \h \v \R \N \b \B )
#
sub classic_character_class($) {
    my($char) = @_;

    return {
        '\D' => '@{Char::Eeucjp::eD}',
        '\S' => '@{Char::Eeucjp::eS}',
        '\W' => '@{Char::Eeucjp::eW}',
        '\d' => '[0-9]',
                 # \t  \n  \f  \r space
        '\s' => '[\x09\x0A\x0C\x0D\x20]',

        # Incompatible Changes
        # \s in regular expressions now matches a Vertical Tab (experimental)
        # http://search.cpan.org/~zefram/perl-5.17.0/pod/perldelta.pod

        # '\s' => '[\x09\x0A\x0B\x0C\x0D\x20]',

        '\w' => '[0-9A-Z_a-z]',
        '\C' => '[\x00-\xFF]',
        '\X' => 'X',

        # \h \v \H \V

        # P.114 Character Class Shortcuts
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        # P.357 13.2.3 Whitespace
        # in Chapter 13: perlrecharclass: Perl Regular Expression Character Classes
        # of ISBN-13: 978-1-906966-02-7 The Perl Language Reference Manual (for Perl version 5.12.1)
        #
        # 0x00009   CHARACTER TABULATION  h s
        # 0x0000a         LINE FEED (LF)   vs
        # 0x0000b        LINE TABULATION   v
        # 0x0000c         FORM FEED (FF)   vs
        # 0x0000d   CARRIAGE RETURN (CR)   vs
        # 0x00020                  SPACE  h s

        # P.196 Table 5-9. Alphanumeric regex metasymbols
        # in Chapter 5. Pattern Matching
        # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

        # (and so on)

        '\H' => '@{Char::Eeucjp::eH}',
        '\V' => '@{Char::Eeucjp::eV}',
        '\h' => '[\x09\x20]',
        '\v' => '[\x0A\x0B\x0C\x0D]',
        '\R' => '@{Char::Eeucjp::eR}',

        # \N
        #
        # http://perldoc.perl.org/perlre.html
        # Character Classes and other Special Escapes
        # Any character but \n (experimental). Not affected by /s modifier

        '\N' => '@{Char::Eeucjp::eN}',

        # \b \B

        # P.180 Boundaries: The \b and \B Assertions
        # in Chapter 5: Pattern Matching
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.

        # P.219 Boundaries: The \b and \B Assertions
        # in Chapter 5: Pattern Matching
        # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

        # '\b' => '(?:(?<=\A|\W)(?=\w)|(?<=\w)(?=\W|\z))',
        '\b' => '@{Char::Eeucjp::eb}',

        # '\B' => '(?:(?<=\w)(?=\w)|(?<=\W)(?=\W))',
        '\B' => '@{Char::Eeucjp::eB}',

    }->{$char} || '';
}

#
# prepare EUC-JP characters per length
#

# 1 octet characters
my @chars1 = ();
sub chars1 {
    if (@chars1) {
        return @chars1;
    }
    if (exists $range_tr{1}) {
        my @ranges = @{ $range_tr{1} };
        while (my @range = splice(@ranges,0,1)) {
            for my $oct0 (@{$range[0]}) {
                push @chars1, pack 'C', $oct0;
            }
        }
    }
    return @chars1;
}

# 2 octets characters
my @chars2 = ();
sub chars2 {
    if (@chars2) {
        return @chars2;
    }
    if (exists $range_tr{2}) {
        my @ranges = @{ $range_tr{2} };
        while (my @range = splice(@ranges,0,2)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    push @chars2, pack 'CC', $oct0,$oct1;
                }
            }
        }
    }
    return @chars2;
}

# 3 octets characters
my @chars3 = ();
sub chars3 {
    if (@chars3) {
        return @chars3;
    }
    if (exists $range_tr{3}) {
        my @ranges = @{ $range_tr{3} };
        while (my @range = splice(@ranges,0,3)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        push @chars3, pack 'CCC', $oct0,$oct1,$oct2;
                    }
                }
            }
        }
    }
    return @chars3;
}

# 4 octets characters
my @chars4 = ();
sub chars4 {
    if (@chars4) {
        return @chars4;
    }
    if (exists $range_tr{4}) {
        my @ranges = @{ $range_tr{4} };
        while (my @range = splice(@ranges,0,4)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        for my $oct3 (@{$range[3]}) {
                            push @chars4, pack 'CCCC', $oct0,$oct1,$oct2,$oct3;
                        }
                    }
                }
            }
        }
    }
    return @chars4;
}

#
# EUC-JP open character list for tr
#
sub _charlist_tr {

    local $_ = shift @_;

    # unescape character
    my @char = ();
    while (not /\G \z/oxmsgc) {
        if (/\G (\\0?55|\\x2[Dd]|\\-) /oxmsgc) {
            push @char, '\-';
        }
        elsif (/\G \\ ([0-7]{2,3}) /oxmsgc) {
            push @char, CORE::chr(oct $1);
        }
        elsif (/\G \\x ([0-9A-Fa-f]{1,2}) /oxmsgc) {
            push @char, CORE::chr(hex $1);
        }
        elsif (/\G \\c ([\x40-\x5F]) /oxmsgc) {
            push @char, CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif (/\G (\\ [0nrtfbae]) /oxmsgc) {
            push @char, {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
            }->{$1};
        }
        elsif (/\G \\ ($q_char) /oxmsgc) {
            push @char, $1;
        }
        elsif (/\G ($q_char) /oxmsgc) {
            push @char, $1;
        }
    }

    # join separated multiple-octet
    @char = join('',@char) =~ /\G (\\-|$q_char) /oxmsg;

    # unescape '-'
    my @i = ();
    for my $i (0 .. $#char) {
        if ($char[$i] eq '\-') {
            $char[$i] = '-';
        }
        elsif ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                push @i, $i;
            }
        }
    }

    # open character list (reverse for splice)
    for my $i (CORE::reverse @i) {
        my @range = ();

        # range error
        if ((CORE::length($char[$i-1]) > CORE::length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
            croak "Invalid tr/// range \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
        }

        # range of multiple-octet code
        if (CORE::length($char[$i-1]) == 1) {
            if (CORE::length($char[$i+1]) == 1) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars1();
            }
            elsif (CORE::length($char[$i+1]) == 2) {
                push @range, grep {$char[$i-1] le $_}                           chars1();
                push @range, grep {$_ le $char[$i+1]}                           chars2();
            }
            elsif (CORE::length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           chars1();
                push @range,                                                    chars2();
                push @range, grep {$_ le $char[$i+1]}                           chars3();
            }
            elsif (CORE::length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           chars1();
                push @range,                                                    chars2();
                push @range,                                                    chars3();
                push @range, grep {$_ le $char[$i+1]}                           chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        elsif (CORE::length($char[$i-1]) == 2) {
            if (CORE::length($char[$i+1]) == 2) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars2();
            }
            elsif (CORE::length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           chars2();
                push @range, grep {$_ le $char[$i+1]}                           chars3();
            }
            elsif (CORE::length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           chars2();
                push @range,                                                    chars3();
                push @range, grep {$_ le $char[$i+1]}                           chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        elsif (CORE::length($char[$i-1]) == 3) {
            if (CORE::length($char[$i+1]) == 3) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars3();
            }
            elsif (CORE::length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           chars3();
                push @range, grep {$_ le $char[$i+1]}                           chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        elsif (CORE::length($char[$i-1]) == 4) {
            if (CORE::length($char[$i+1]) == 4) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        else {
            croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
        }

        splice @char, $i-1, 3, @range;
    }

    return @char;
}

#
# EUC-JP open character class
#
sub _cc {
    if (scalar(@_) == 0) {
        die __FILE__, ": function cc got no parameter.";
    }
    elsif (scalar(@_) == 1) {
        return sprintf('\x%02X',$_[0]);
    }
    elsif (scalar(@_) == 2) {
        if ($_[0] > $_[1]) {
            die __FILE__, ": function cc got \$_[0] > \$_[1] parameters).";
        }
        elsif ($_[0] == $_[1]) {
            return sprintf('\x%02X',$_[0]);
        }
        elsif (($_[0]+1) == $_[1]) {
            return sprintf('[\\x%02X\\x%02X]',$_[0],$_[1]);
        }
        else {
            return sprintf('[\\x%02X-\\x%02X]',$_[0],$_[1]);
        }
    }
    else {
        die __FILE__, ": function cc got 3 or more parameters (@{[scalar(@_)]} parameters).";
    }
}

#
# EUC-JP octet range
#
sub _octets {
    my $length = shift @_;

    if ($length == 1) {
        my($a1) = unpack 'C', $_[0];
        my($z1) = unpack 'C', $_[1];

        if ($a1 > $z1) {
            croak 'Invalid [] range in regexp (ord(A) > ord(B)) ' . '\x' . unpack('H*',$a1) . '-\x' . unpack('H*',$z1);
        }

        if ($a1 == $z1) {
            return sprintf('\x%02X',$a1);
        }
        elsif (($a1+1) == $z1) {
            return sprintf('\x%02X\x%02X',$a1,$z1);
        }
        else {
            return sprintf('\x%02X-\x%02X',$a1,$z1);
        }
    }
    elsif ($length == 2) {
        my($a1,$a2) = unpack 'CC', $_[0];
        my($z1,$z2) = unpack 'CC', $_[1];
        my($A1,$A2) = unpack 'CC', $_[2];
        my($Z1,$Z2) = unpack 'CC', $_[3];

        if ($a1 == $z1) {
            return (
            #   11111111   222222222222
            #        A          A   Z
                _cc($a1) . _cc($a2,$z2), # a2-z2
            );
        }
        elsif (($a1+1) == $z1) {
            return (
            #   11111111111   222222222222
            #        A  Z          A   Z
                _cc($a1)    . _cc($a2,$Z2), # a2-
                _cc(   $z1) . _cc($A2,$z2), #   -z2
            );
        }
        else {
            return (
            #   1111111111111111   222222222222
            #        A     Z            A   Z
                _cc($a1)         . _cc($a2,$Z2), # a2-
                _cc($a1+1,$z1-1) . _cc($A2,$Z2), #   -
                _cc(      $z1)   . _cc($A2,$z2), #   -z2
            );
        }
    }
    elsif ($length == 3) {
        my($a1,$a2,$a3) = unpack 'CCC', $_[0];
        my($z1,$z2,$z3) = unpack 'CCC', $_[1];
        my($A1,$A2,$A3) = unpack 'CCC', $_[2];
        my($Z1,$Z2,$Z3) = unpack 'CCC', $_[3];

        if ($a1 == $z1) {
            if ($a2 == $z2) {
                return (
                #   11111111   22222222   333333333333
                #        A          A          A   Z
                    _cc($a1) . _cc($a2) . _cc($a3,$z3), # a3-z3
                );
            }
            elsif (($a2+1) == $z2) {
                return (
                #   11111111   22222222222   333333333333
                #        A          A  Z          A   Z
                    _cc($a1) . _cc($a2)    . _cc($a3,$Z3), # a3-
                    _cc($a1) . _cc(   $z2) . _cc($A3,$z3), #   -z3
                );
            }
            else {
                return (
                #   11111111   2222222222222222   333333333333
                #        A          A     Z            A   Z
                    _cc($a1) . _cc($a2)         . _cc($a3,$Z3), # a3-
                    _cc($a1) . _cc($a2+1,$z2-1) . _cc($A3,$Z3), #   -
                    _cc($a1) . _cc(      $z2)   . _cc($A3,$z3), #   -z3
                );
            }
        }
        elsif (($a1+1) == $z1) {
            return (
            #   11111111111   22222222222222   333333333333
            #        A  Z          A     Z          A   Z
                _cc($a1)    . _cc($a2)       . _cc($a3,$Z3), # a3-
                _cc($a1)    . _cc($a2+1,$Z2) . _cc($A3,$Z3), #   -
                _cc(   $z1) . _cc($A2,$z2-1) . _cc($A3,$Z3), #   -
                _cc(   $z1) . _cc(    $z2)   . _cc($A3,$z3), #   -z3
            );
        }
        else {
            return (
            #   1111111111111111   22222222222222   333333333333
            #        A     Z            A     Z          A   Z
                _cc($a1)         . _cc($a2)       . _cc($a3,$Z3), # a3-
                _cc($a1)         . _cc($a2+1,$Z2) . _cc($A3,$Z3), #   -
                _cc($a1+1,$z1-1) . _cc($A2,$Z2)   . _cc($A3,$Z3), #   -
                _cc(      $z1)   . _cc($A2,$z2-1) . _cc($A3,$Z3), #   -
                _cc(      $z1)   . _cc(    $z2)   . _cc($A3,$z3), #   -z3
            );
        }
    }
    elsif ($length == 4) {
        my($a1,$a2,$a3,$a4) = unpack 'CCCC', $_[0];
        my($z1,$z2,$z3,$z4) = unpack 'CCCC', $_[1];
        my($A1,$A2,$A3,$A4) = unpack 'CCCC', $_[0];
        my($Z1,$Z2,$Z3,$Z4) = unpack 'CCCC', $_[1];

        if ($a1 == $z1) {
            if ($a2 == $z2) {
                if ($a3 == $z3) {
                    return (
                    #   11111111   22222222   33333333   444444444444
                    #        A          A          A          A   Z
                        _cc($a1) . _cc($a2) . _cc($a3) . _cc($a4,$z4), # a4-z4
                    );
                }
                elsif (($a3+1) == $z3) {
                    return (
                    #   11111111   22222222   33333333333   444444444444
                    #        A          A          A  Z          A   Z
                        _cc($a1) . _cc($a2) . _cc($a3)    . _cc($a4,$Z4), # a4-
                        _cc($a1) . _cc($a2) . _cc(   $z3) . _cc($A4,$z4), #   -z4
                    );
                }
                else {
                    return (
                    #   11111111   22222222   3333333333333333   444444444444
                    #        A          A          A     Z            A   Z
                        _cc($a1) . _cc($a2) . _cc($a3)         . _cc($a4,$Z4), # a4-
                        _cc($a1) . _cc($a2) . _cc($a3+1,$z3-1) . _cc($A4,$Z4), #   -
                        _cc($a1) . _cc($a2) . _cc(      $z3)   . _cc($A4,$z4), #   -z4
                    );
                }
            }
            elsif (($a2+1) == $z2) {
                return (
                #   11111111   22222222222   33333333333333   444444444444
                #        A          A  Z          A     Z          A   Z
                    _cc($a1) . _cc($a2)    . _cc($a3)       . _cc($a4,$Z4), # a4-
                    _cc($a1) . _cc($a2)    . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(   $z2) . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(   $z2) . _cc(    $z3)   . _cc($A4,$z4), #   -z4
                );
            }
            else {
                return (
                #   11111111   2222222222222222   33333333333333   444444444444
                #        A          A     Z            A     Z          A   Z
                    _cc($a1) . _cc($a2)         . _cc($a3)       . _cc($a4,$Z4), # a4-
                    _cc($a1) . _cc($a2)         . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc($a2+1,$z2-1) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(      $z2)   . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(      $z2)   . _cc(    $z3)   . _cc($A4,$z4), #   -z4
                );
            }
        }
        elsif (($a1+1) == $z1) {
            return (
            #   11111111111   22222222222222   33333333333333   444444444444
            #        A  Z          A     Z          A     Z          A   Z
                _cc($a1)    . _cc($a2)       . _cc($a3)       . _cc($a4,$Z4), # a4-
                _cc($a1)    . _cc($a2)       . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                _cc($a1)    . _cc($a2+1,$Z2) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(   $z1) . _cc($A2,$z2-1) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(   $z1) . _cc(    $z2)   . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                _cc(   $z1) . _cc(    $z2)   . _cc(    $z3)   . _cc($A4,$z4), #   -z4
            );
        }
        else {
            return (
            #   1111111111111111   22222222222222   33333333333333   444444444444
            #        A     Z            A     Z          A     Z          A   Z
                _cc($a1)         . _cc($a2)       . _cc($a3)       . _cc($a4,$Z4), # a4-
                _cc($a1)         . _cc($a2)       . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                _cc($a1)         . _cc($a2+1,$Z2) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc($a1+1,$z1-1) . _cc($A2,$Z2)   . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(      $z1)   . _cc($A2,$z2-1) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(      $z1)   . _cc(    $z2)   . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                _cc(      $z1)   . _cc(    $z2)   . _cc(    $z3)   . _cc($A4,$z4), #   -z4
            );
        }
    }
    else {
        die __FILE__, ": function _octets got invalid length ($length).";
    }
}

#
# EUC-JP range regexp
#
sub _range_regexp {
    my($length,$first,$last) = @_;

    my @range_regexp = ();
    if (not exists $range_tr{$length}) {
        return @range_regexp;
    }

    my @ranges = @{ $range_tr{$length} };
    while (my @range = splice(@ranges,0,$length)) {
        my $min = '';
        my $max = '';
        for (my $i=0; $i < $length; $i++) {
            $min .= pack 'C', $range[$i][0];
            $max .= pack 'C', $range[$i][-1];
        }

# min___max
#            FIRST_____________LAST
#       (nothing)

        if ($max lt $first) {
        }

#            **********
#       min_________max
#            FIRST_____________LAST
#            **********

        elsif (($min le $first) and ($first le $max) and ($max le $last)) {
            push @range_regexp, _octets($length,$first,$max,$min,$max);
        }

#            **********************
#            min________________max
#            FIRST_____________LAST
#            **********************

        elsif (($min eq $first) and ($max eq $last)) {
            push @range_regexp, _octets($length,$first,$last,$min,$max);
        }

#                   *********
#                   min___max
#            FIRST_____________LAST
#                   *********

        elsif (($first le $min) and ($max le $last)) {
            push @range_regexp, _octets($length,$min,$max,$min,$max);
        }

#            **********************
#       min__________________________max
#            FIRST_____________LAST
#            **********************

        elsif (($min le $first) and ($last le $max)) {
            push @range_regexp, _octets($length,$first,$last,$min,$max);
        }

#                         *********
#                         min________max
#            FIRST_____________LAST
#                         *********

        elsif (($first le $min) and ($min le $last) and ($last le $max)) {
            push @range_regexp, _octets($length,$min,$last,$min,$max);
        }

#                                    min___max
#            FIRST_____________LAST
#                              (nothing)

        elsif ($last lt $min) {
        }

        else {
            die __FILE__, ": function _range_regexp panic.";
        }
    }

    return @range_regexp;
}

#
# EUC-JP open character list for qr and not qr
#
sub _charlist {

    my $modifier = pop @_;
    my @char = @_;

    my $ignorecase = ($modifier =~ /i/oxms) ? 1 : 0;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {

        # escape - to ...
        if ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                $char[$i] = '...';
            }
        }

        # octal escape sequence
        elsif ($char[$i] =~ /\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ /\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = hexchr($1);
        }

        # \N{CHARNAME} --> N{CHARNAME}
        elsif ($char[$i] =~ /\A \\ ( N\{ ([^\x8E\x8F\xA1-\xFE0-9\}][^\x8E\x8F\xA1-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \p{PROPERTY} --> p{PROPERTY}
        elsif ($char[$i] =~ /\A \\ ( p\{ ([^\x8E\x8F\xA1-\xFE0-9\}][^\x8E\x8F\xA1-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \P{PROPERTY} --> P{PROPERTY}
        elsif ($char[$i] =~ /\A \\ ( P\{ ([^\x8E\x8F\xA1-\xFE0-9\}][^\x8E\x8F\xA1-\xFE\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \p, \P, \X --> p, P, X
        elsif ($char[$i] =~ /\A \\ ( [pPX] ) \z/oxms) {
            $char[$i] = $1;
        }

        elsif ($char[$i] =~ /\A \\ ([0-7]{2,3}) \z/oxms) {
            $char[$i] = CORE::chr oct $1;
        }
        elsif ($char[$i] =~ /\A \\x ([0-9A-Fa-f]{1,2}) \z/oxms) {
            $char[$i] = CORE::chr hex $1;
        }
        elsif ($char[$i] =~ /\A \\c ([\x40-\x5F]) \z/oxms) {
            $char[$i] = CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif ($char[$i] =~ /\A (\\ [0nrtfbaedswDSWHVhvR]) \z/oxms) {
            $char[$i] = {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
                '\d' => '[0-9]',

                         # \t  \n  \f  \r space
                '\s' => '[\x09\x0A\x0C\x0D\x20]',

                # Incompatible Changes
                # \s in regular expressions now matches a Vertical Tab (experimental)
                # http://search.cpan.org/~zefram/perl-5.17.0/pod/perldelta.pod

                # '\s' => '[\x09\x0A\x0B\x0C\x0D\x20]',

                '\w' => '[0-9A-Z_a-z]',
                '\D' => '@{Char::Eeucjp::eD}',
                '\S' => '@{Char::Eeucjp::eS}',
                '\W' => '@{Char::Eeucjp::eW}',

                '\H' => '@{Char::Eeucjp::eH}',
                '\V' => '@{Char::Eeucjp::eV}',
                '\h' => '[\x09\x20]',
                '\v' => '[\x0A\x0B\x0C\x0D]',
                '\R' => '@{Char::Eeucjp::eR}',

            }->{$1};
        }

        # POSIX-style character classes
        elsif ($ignorecase and ($char[$i] =~ /\A ( \[\: \^? (?:lower|upper) :\] ) \z/oxms)) {
            $char[$i] = {

                '[:lower:]'   => '[\x41-\x5A\x61-\x7A]',
                '[:upper:]'   => '[\x41-\x5A\x61-\x7A]',
                '[:^lower:]'  => '@{Char::Eeucjp::not_lower_i}',
                '[:^upper:]'  => '@{Char::Eeucjp::not_upper_i}',

            }->{$1};
        }
        elsif ($char[$i] =~ /\A ( \[\: \^? (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] ) \z/oxms) {
            $char[$i] = {

                '[:alnum:]'   => '[\x30-\x39\x41-\x5A\x61-\x7A]',
                '[:alpha:]'   => '[\x41-\x5A\x61-\x7A]',
                '[:ascii:]'   => '[\x00-\x7F]',
                '[:blank:]'   => '[\x09\x20]',
                '[:cntrl:]'   => '[\x00-\x1F\x7F]',
                '[:digit:]'   => '[\x30-\x39]',
                '[:graph:]'   => '[\x21-\x7F]',
                '[:lower:]'   => '[\x61-\x7A]',
                '[:print:]'   => '[\x20-\x7F]',
                '[:punct:]'   => '[\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E]',
                '[:space:]'   => '[\x09\x0A\x0B\x0C\x0D\x20]',
                '[:upper:]'   => '[\x41-\x5A]',
                '[:word:]'    => '[\x30-\x39\x41-\x5A\x5F\x61-\x7A]',
                '[:xdigit:]'  => '[\x30-\x39\x41-\x46\x61-\x66]',
                '[:^alnum:]'  => '@{Char::Eeucjp::not_alnum}',
                '[:^alpha:]'  => '@{Char::Eeucjp::not_alpha}',
                '[:^ascii:]'  => '@{Char::Eeucjp::not_ascii}',
                '[:^blank:]'  => '@{Char::Eeucjp::not_blank}',
                '[:^cntrl:]'  => '@{Char::Eeucjp::not_cntrl}',
                '[:^digit:]'  => '@{Char::Eeucjp::not_digit}',
                '[:^graph:]'  => '@{Char::Eeucjp::not_graph}',
                '[:^lower:]'  => '@{Char::Eeucjp::not_lower}',
                '[:^print:]'  => '@{Char::Eeucjp::not_print}',
                '[:^punct:]'  => '@{Char::Eeucjp::not_punct}',
                '[:^space:]'  => '@{Char::Eeucjp::not_space}',
                '[:^upper:]'  => '@{Char::Eeucjp::not_upper}',
                '[:^word:]'   => '@{Char::Eeucjp::not_word}',
                '[:^xdigit:]' => '@{Char::Eeucjp::not_xdigit}',

            }->{$1};
        }
        elsif ($char[$i] =~ /\A \\ ($q_char) \z/oxms) {
            $char[$i] = $1;
        }
    }

    # open character list
    my @singleoctet   = ();
    my @multipleoctet = ();
    for (my $i=0; $i <= $#char; ) {

        # escaped -
        if (defined($char[$i+1]) and ($char[$i+1] eq '...')) {
            $i += 1;
            next;
        }

        # make range regexp
        elsif ($char[$i] eq '...') {

            # range error
            if (CORE::length($char[$i-1]) > CORE::length($char[$i+1])) {
                croak 'Invalid [] range in regexp (length(A) > length(B)) ' . '\x' . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]);
            }
            elsif (CORE::length($char[$i-1]) == CORE::length($char[$i+1])) {
                if ($char[$i-1] gt $char[$i+1]) {
                    croak 'Invalid [] range in regexp (ord(A) > ord(B)) ' . '\x' . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]);
                }
            }

            # make range regexp per length
            for my $length (CORE::length($char[$i-1]) .. CORE::length($char[$i+1])) {
                my @regexp = ();

                # is first and last
                if (($length == CORE::length($char[$i-1])) and ($length == CORE::length($char[$i+1]))) {
                    push @regexp, _range_regexp($length, $char[$i-1], $char[$i+1]);
                }

                # is first
                elsif ($length == CORE::length($char[$i-1])) {
                    push @regexp, _range_regexp($length, $char[$i-1], "\xFF" x $length);
                }

                # is inside in first and last
                elsif ((CORE::length($char[$i-1]) < $length) and ($length < CORE::length($char[$i+1]))) {
                    push @regexp, _range_regexp($length, "\x00" x $length, "\xFF" x $length);
                }

                # is last
                elsif ($length == CORE::length($char[$i+1])) {
                    push @regexp, _range_regexp($length, "\x00" x $length, $char[$i+1]);
                }

                else {
                    die __FILE__, ": function make_regexp panic.";
                }

                if ($length == 1) {
                    push @singleoctet, @regexp;
                }
                else {
                    push @multipleoctet, @regexp;
                }
            }

            $i += 2;
        }

        # with /i modifier
        elsif ($char[$i] =~ /\A [\x00-\xFF] \z/oxms) {
            if ($modifier =~ /i/oxms) {
                my $uc = Char::Eeucjp::uc($char[$i]);
                my $fc = Char::Eeucjp::fc($char[$i]);
                if ($uc ne $fc) {
                    if (CORE::length($fc) == 1) {
                        push @singleoctet, $uc, $fc;
                    }
                    else {
                        push @singleoctet,   $uc;
                        push @multipleoctet, $fc;
                    }
                }
                else {
                    push @singleoctet, $char[$i];
                }
            }
            else {
                push @singleoctet, $char[$i];
            }
            $i += 1;
        }

        # single character of single octet code
        elsif ($char[$i] =~ /\A (?: \\h ) \z/oxms) {
            push @singleoctet, "\t", "\x20";
            $i += 1;
        }
        elsif ($char[$i] =~ /\A (?: \\v ) \z/oxms) {
            push @singleoctet, "\x0A", "\x0B", "\x0C", "\x0D";
            $i += 1;
        }
        elsif ($char[$i] =~ /\A (?: \\d | \\s | \\w ) \z/oxms) {
            push @singleoctet, $char[$i];
            $i += 1;
        }

        # single character of multiple-octet code
        else {
            push @multipleoctet, $char[$i];
            $i += 1;
        }
    }

    # quote metachar
    for (@singleoctet) {
        if ($_ eq '...') {
            $_ = '-';
        }
        elsif (/\A \n \z/oxms) {
            $_ = '\n';
        }
        elsif (/\A \r \z/oxms) {
            $_ = '\r';
        }
        elsif (/\A ([\x00-\x20\x7F-\xFF]) \z/oxms) {
            $_ = sprintf('\x%02X', CORE::ord $1);
        }
        elsif (/\A [\x00-\xFF] \z/oxms) {
            $_ = quotemeta $_;
        }
    }

    # return character list
    return \@singleoctet, \@multipleoctet;
}

#
# EUC-JP octal escape sequence
#
sub octchr {
    my($octdigit) = @_;

    my @binary = ();
    for my $octal (split(//,$octdigit)) {
        push @binary, {
            '0' => '000',
            '1' => '001',
            '2' => '010',
            '3' => '011',
            '4' => '100',
            '5' => '101',
            '6' => '110',
            '7' => '111',
        }->{$octal};
    }
    my $binary = join '', @binary;

    my $octchr = {
        #                1234567
        1 => pack('B*', "0000000$binary"),
        2 => pack('B*', "000000$binary"),
        3 => pack('B*', "00000$binary"),
        4 => pack('B*', "0000$binary"),
        5 => pack('B*', "000$binary"),
        6 => pack('B*', "00$binary"),
        7 => pack('B*', "0$binary"),
        0 => pack('B*', "$binary"),

    }->{CORE::length($binary) % 8};

    return $octchr;
}

#
# EUC-JP hexadecimal escape sequence
#
sub hexchr {
    my($hexdigit) = @_;

    my $hexchr = {
        1 => pack('H*', "0$hexdigit"),
        0 => pack('H*', "$hexdigit"),

    }->{CORE::length($_[0]) % 2};

    return $hexchr;
}

#
# EUC-JP open character list for qr
#
sub charlist_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $multipleoctet) = _charlist(@char, $modifier);
    my @singleoctet   = @$singleoctet;
    my @multipleoctet = @$multipleoctet;

    # return character list
    if (scalar(@singleoctet) >= 1) {

        # with /i modifier
        if ($modifier =~ m/i/oxms) {
            my %singleoctet_ignorecase = ();
            for (@singleoctet) {
                while (s/ \A \\x(..) - \\x(..) //oxms or s/ \A \\x((..)) //oxms) {
                    for my $ord (hex($1) .. hex($2)) {
                        my $char = CORE::chr($ord);
                        my $uc = Char::Eeucjp::uc($char);
                        my $fc = Char::Eeucjp::fc($char);
                        if ($uc eq $fc) {
                            $singleoctet_ignorecase{unpack 'C*', $char} = 1;
                        }
                        else {
                            if (CORE::length($fc) == 1) {
                                $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                                $singleoctet_ignorecase{unpack 'C*', $fc} = 1;
                            }
                            else {
                                $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                                push @multipleoctet, join '', map {sprintf('\x%02X',$_)} unpack 'C*', $fc;
                            }
                        }
                    }
                }
            }
            my $i = 0;
            my @singleoctet_ignorecase = ();
            for my $ord (0 .. 255) {
                if (exists $singleoctet_ignorecase{$ord}) {
                    push @{$singleoctet_ignorecase[$i]}, $ord;
                }
                else {
                    $i++;
                }
            }
            @singleoctet = ();
            for my $range (@singleoctet_ignorecase) {
                if (ref $range) {
                    if (scalar(@{$range}) == 1) {
                        push @singleoctet, sprintf('\x%02X', @{$range}[0]);
                    }
                    elsif (scalar(@{$range}) == 2) {
                        push @singleoctet, sprintf('\x%02X\x%02X', @{$range}[0], @{$range}[-1]);
                    }
                    else {
                        push @singleoctet, sprintf('\x%02X-\x%02X', @{$range}[0], @{$range}[-1]);
                    }
                }
            }
        }

        my $not_anchor = '';
        $not_anchor = '(?!\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE])';

        push @multipleoctet, join('', $not_anchor, '[', @singleoctet, ']' );
    }
    if (scalar(@multipleoctet) >= 2) {
        return '(?:' . join('|', @multipleoctet) . ')';
    }
    else {
        return $multipleoctet[0];
    }
}

#
# EUC-JP open character list for not qr
#
sub charlist_not_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $multipleoctet) = _charlist(@char, $modifier);
    my @singleoctet   = @$singleoctet;
    my @multipleoctet = @$multipleoctet;

    # with /i modifier
    if ($modifier =~ m/i/oxms) {
        my %singleoctet_ignorecase = ();
        for (@singleoctet) {
            while (s/ \A \\x(..) - \\x(..) //oxms or s/ \A \\x((..)) //oxms) {
                for my $ord (hex($1) .. hex($2)) {
                    my $char = CORE::chr($ord);
                    my $uc = Char::Eeucjp::uc($char);
                    my $fc = Char::Eeucjp::fc($char);
                    if ($uc eq $fc) {
                        $singleoctet_ignorecase{unpack 'C*', $char} = 1;
                    }
                    else {
                        if (CORE::length($fc) == 1) {
                            $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                            $singleoctet_ignorecase{unpack 'C*', $fc} = 1;
                        }
                        else {
                            $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                            push @multipleoctet, join '', map {sprintf('\x%02X',$_)} unpack 'C*', $fc;
                        }
                    }
                }
            }
        }
        my $i = 0;
        my @singleoctet_ignorecase = ();
        for my $ord (0 .. 255) {
            if (exists $singleoctet_ignorecase{$ord}) {
                push @{$singleoctet_ignorecase[$i]}, $ord;
            }
            else {
                $i++;
            }
        }
        @singleoctet = ();
        for my $range (@singleoctet_ignorecase) {
            if (ref $range) {
                if (scalar(@{$range}) == 1) {
                    push @singleoctet, sprintf('\x%02X', @{$range}[0]);
                }
                elsif (scalar(@{$range}) == 2) {
                    push @singleoctet, sprintf('\x%02X\x%02X', @{$range}[0], @{$range}[-1]);
                }
                else {
                    push @singleoctet, sprintf('\x%02X-\x%02X', @{$range}[0], @{$range}[-1]);
                }
            }
        }
    }

    # return character list
    if (scalar(@multipleoctet) >= 1) {
        if (scalar(@singleoctet) >= 1) {

            # any character other than multiple-octet and single octet character class
            return '(?!' . join('|', @multipleoctet) . ')(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE' . join('', @singleoctet) . '])';
        }
        else {

            # any character other than multiple-octet character class
            return '(?!' . join('|', @multipleoctet) . ")(?:$your_char)";
        }
    }
    else {
        if (scalar(@singleoctet) >= 1) {

            # any character other than single octet character class
            return                                      '(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE' . join('', @singleoctet) . '])';
        }
        else {

            # any character
            return                                      "(?:$your_char)";
        }
    }
}

#
# EUC-JP order to character (with parameter)
#
sub Char::Eeucjp::chr(;$) {

    my $c = @_ ? $_[0] : $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# EUC-JP order to character (without parameter)
#
sub Char::Eeucjp::chr_() {

    my $c = $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# EUC-JP path globbing (with parameter)
#
sub Char::Eeucjp::glob($) {

    if (wantarray) {
        my @glob = _dosglob(@_);
        for my $glob (@glob) {
            $glob =~ s{ \A (?:\./)+ }{}oxms;
        }
        return @glob;
    }
    else {
        my $glob = _dosglob(@_);
        $glob =~ s{ \A (?:\./)+ }{}oxms;
        return $glob;
    }
}

#
# EUC-JP path globbing (without parameter)
#
sub Char::Eeucjp::glob_() {

    if (wantarray) {
        my @glob = _dosglob();
        for my $glob (@glob) {
            $glob =~ s{ \A (?:\./)+ }{}oxms;
        }
        return @glob;
    }
    else {
        my $glob = _dosglob();
        $glob =~ s{ \A (?:\./)+ }{}oxms;
        return $glob;
    }
}

#
# EUC-JP path globbing from File::DosGlob module
#
my %iter;
my %entries;
sub _dosglob {

    # context (keyed by second cxix argument provided by core)
    my($expr,$cxix) = @_;

    # glob without args defaults to $_
    $expr = $_ if not defined $expr;

    # represents the current user's home directory
    #
    # 7.3. Expanding Tildes in Filenames
    # in Chapter 7. File Access
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
    #
    # and File::HomeDir, File::HomeDir::Windows module

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $expr =~ s{ \A ~ (?= [^/\\] ) }
                  { $ENV{'HOME'} || $ENV{'USERPROFILE'} || "$ENV{'HOMEDRIVE'}$ENV{'HOMEPATH'}" }oxmse;
    }

    # UNIX-like system
    else {
        $expr =~ s{ \A ~ ( (?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE/])* ) }
                  { $1 ? (getpwnam($1))[7] : ($ENV{'HOME'} || $ENV{'LOGDIR'} || (getpwuid($<))[7]) }oxmse;
    }

    # assume global context if not provided one
    $cxix = '_G_' if not defined $cxix;
    $iter{$cxix} = 0 if not exists $iter{$cxix};

    # if we're just beginning, do it all first
    if ($iter{$cxix} == 0) {
            $entries{$cxix} = [ _do_glob(1, _parse_line($expr)) ];
    }

    # chuck it all out, quick or slow
    if (wantarray) {
        delete $iter{$cxix};
        return @{delete $entries{$cxix}};
    }
    else {
        if ($iter{$cxix} = scalar @{$entries{$cxix}}) {
            return shift @{$entries{$cxix}};
        }
        else {
            # return undef for EOL
            delete $iter{$cxix};
            delete $entries{$cxix};
            return undef;
        }
    }
}

#
# EUC-JP path globbing subroutine
#
sub _do_glob {

    my($cond,@expr) = @_;
    my @glob = ();
    my $fix_drive_relative_paths = 0;

OUTER:
    for my $expr (@expr) {
        next OUTER if not defined $expr;
        next OUTER if $expr eq '';

        my @matched = ();
        my @globdir = ();
        my $head    = '.';
        my $pathsep = '/';
        my $tail;

        # if argument is within quotes strip em and do no globbing
        if ($expr =~ /\A " ((?:$q_char)*) " \z/oxms) {
            $expr = $1;
            if ($cond eq 'd') {
                if (-d $expr) {
                    push @glob, $expr;
                }
            }
            else {
                if (-e $expr) {
                    push @glob, $expr;
                }
            }
            next OUTER;
        }

        # wildcards with a drive prefix such as h:*.pm must be changed
        # to h:./*.pm to expand correctly
        if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
            if ($expr =~ s# \A ((?:[A-Za-z]:)?) (\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE/\\]) #$1./$2#oxms) {
                $fix_drive_relative_paths = 1;
            }
        }

        if (($head, $tail) = _parse_path($expr,$pathsep)) {
            if ($tail eq '') {
                push @glob, $expr;
                next OUTER;
            }
            if ($head =~ / \A (?:$q_char)*? [*?] /oxms) {
                if (@globdir = _do_glob('d', $head)) {
                    push @glob, _do_glob($cond, map {"$_$pathsep$tail"} @globdir);
                    next OUTER;
                }
            }
            if ($head eq '' or $head =~ /\A [A-Za-z]: \z/oxms) {
                $head .= $pathsep;
            }
            $expr = $tail;
        }

        # If file component has no wildcards, we can avoid opendir
        if ($expr !~ / \A (?:$q_char)*? [*?] /oxms) {
            if ($head eq '.') {
                $head = '';
            }
            if ($head ne '' and ($head =~ / \G ($q_char) /oxmsg)[-1] ne $pathsep) {
                $head .= $pathsep;
            }
            $head .= $expr;
            if ($cond eq 'd') {
                if (-d $head) {
                    push @glob, $head;
                }
            }
            else {
                if (-e $head) {
                    push @glob, $head;
                }
            }
            next OUTER;
        }
        opendir(*DIR, $head) or next OUTER;
        my @leaf = readdir DIR;
        closedir DIR;

        if ($head eq '.') {
            $head = '';
        }
        if ($head ne '' and ($head =~ / \G ($q_char) /oxmsg)[-1] ne $pathsep) {
            $head .= $pathsep;
        }

        my $pattern = '';
        while ($expr =~ / \G ($q_char) /oxgc) {
            my $char = $1;
            if ($char eq '*') {
                $pattern .= "(?:$your_char)*",
            }
            elsif ($char eq '?') {
                $pattern .= "(?:$your_char)?",  # DOS style
#               $pattern .= "(?:$your_char)",   # UNIX style
            }
            elsif ((my $fc = Char::Eeucjp::fc($char)) ne $char) {
                $pattern .= $fc;
            }
            else {
                $pattern .= quotemeta $char;
            }
        }
        my $matchsub = sub { Char::Eeucjp::fc($_[0]) =~ /\A $pattern \z/xms };

#       if ($@) {
#           print STDERR "$0: $@\n";
#           next OUTER;
#       }

INNER:
        for my $leaf (@leaf) {
            if ($leaf eq '.' or $leaf eq '..') {
                next INNER;
            }
            if ($cond eq 'd' and not -d "$head$leaf") {
                next INNER;
            }

            if (&$matchsub($leaf)) {
                push @matched, "$head$leaf";
                next INNER;
            }

            # [DOS compatibility special case]
            # Failed, add a trailing dot and try again, but only...

            if (Char::Eeucjp::index($leaf,'.') == -1 and   # if name does not have a dot in it *and*
                CORE::length($leaf) <= 8 and        # name is shorter than or equal to 8 chars *and*
                Char::Eeucjp::index($pattern,'\\.') != -1  # pattern has a dot.
            ) {
                if (&$matchsub("$leaf.")) {
                    push @matched, "$head$leaf";
                    next INNER;
                }
            }
        }
        if (@matched) {
            push @glob, @matched;
        }
    }
    if ($fix_drive_relative_paths) {
        for my $glob (@glob) {
            $glob =~ s# \A ([A-Za-z]:) \./ #$1#oxms;
        }
    }
    return @glob;
}

#
# EUC-JP parse line
#
sub _parse_line {

    my($line) = @_;

    $line .= ' ';
    my @piece = ();
    while ($line =~ /
        " ( (?: \x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE"]   )*  ) " \s+ |
          ( (?: \x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE"\s] )*  )   \s+
        /oxmsg
    ) {
        push @piece, defined($1) ? $1 : $2;
    }
    return @piece;
}

#
# EUC-JP parse path
#
sub _parse_path {

    my($path,$pathsep) = @_;

    $path .= '/';
    my @subpath = ();
    while ($path =~ /
        ((?: \x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x8E\x8F\xA1-\xFE\/\\] )+?) [\/\\]
        /oxmsg
    ) {
        push @subpath, $1;
    }

    my $tail = pop @subpath;
    my $head = join $pathsep, @subpath;
    return $head, $tail;
}

#
# ${^PREMATCH}, $PREMATCH, $` the string preceding what was matched
#
sub Char::Eeucjp::PREMATCH {
    if (defined($&)) {
        if (defined($1) and (CORE::substr($&,-CORE::length($1),CORE::length($1)) eq $1)) {
            return CORE::substr($&,0,CORE::length($&)-CORE::length($1));
        }
        else {
            croak 'Use of "$`", $PREMATCH and ${^PREMATCH} need to /( capture all )/ in regexp';
        }
    }
    else {
        return '';
    }
    return $`;
}

#
# ${^MATCH}, $MATCH, $& the string that matched
#
sub Char::Eeucjp::MATCH {
    if (defined($&)) {
        if (defined($1)) {
            return $1;
        }
        else {
            croak 'Use of "$&", $MATCH and ${^MATCH} need to /( capture all )/ in regexp';
        }
    }
    else {
        return '';
    }
    return $&;
}

#
# ${^POSTMATCH}, $POSTMATCH, $' the string following what was matched
#
sub Char::Eeucjp::POSTMATCH {
    return $';
}

#
# EUC-JP character to order (with parameter)
#
sub Char::EUCJP::ord(;$) {

    local $_ = shift if @_;

    if (/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# EUC-JP character to order (without parameter)
#
sub Char::EUCJP::ord_() {

    if (/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# EUC-JP reverse
#
sub Char::EUCJP::reverse(@) {

    if (wantarray) {
        return CORE::reverse @_;
    }
    else {
        return join '', CORE::reverse(join('',@_) =~ /\G ($q_char) /oxmsg);
    }
}

#
# EUC-JP length by character
#
sub Char::EUCJP::length(;$) {

    local $_ = shift if @_;

    local @_ = /\G ($q_char) /oxmsg;
    return scalar @_;
}

#
# EUC-JP substr by character
#
sub Char::EUCJP::substr($$;$$) {

    my @char = $_[0] =~ /\G ($q_char) /oxmsg;

    # substr($string,$offset,$length,$replacement)
    if (@_ == 4) {
        my(undef,$offset,$length,$replacement) = @_;
        my $substr = join '', splice(@char, $offset, $length, $replacement);
        $_[0] = join '', @char;
        return $substr;
    }

    # substr($string,$offset,$length)
    elsif (@_ == 3) {
        my(undef,$offset,$length) = @_;
        if ($length == 0) {
            return '';
        }
        if ($offset >= 0) {
            return join '', (@char[$offset            .. $#char])[0 .. $length-1];
        }
        else {
            return join '', (@char[($#char+$offset+1) .. $#char])[0 .. $length-1];
        }
    }

    # substr($string,$offset)
    else {
        my(undef,$offset) = @_;
        if ($offset >= 0) {
            return join '', @char[$offset            .. $#char];
        }
        else {
            return join '', @char[($#char+$offset+1) .. $#char];
        }
    }
}

#
# EUC-JP index by character
#
sub Char::EUCJP::index($$;$) {

    my $index;
    if (@_ == 3) {
        $index = Char::Eeucjp::index($_[0], $_[1], CORE::length(Char::EUCJP::substr($_[0], 0, $_[2])));
    }
    else {
        $index = Char::Eeucjp::index($_[0], $_[1]);
    }

    if ($index == -1) {
        return -1;
    }
    else {
        return Char::EUCJP::length(CORE::substr $_[0], 0, $index);
    }
}

#
# EUC-JP rindex by character
#
sub Char::EUCJP::rindex($$;$) {

    my $rindex;
    if (@_ == 3) {
        $rindex = Char::Eeucjp::rindex($_[0], $_[1], CORE::length(Char::EUCJP::substr($_[0], 0, $_[2])));
    }
    else {
        $rindex = Char::Eeucjp::rindex($_[0], $_[1]);
    }

    if ($rindex == -1) {
        return -1;
    }
    else {
        return Char::EUCJP::length(CORE::substr $_[0], 0, $rindex);
    }
}

#
# instead of Carp::carp
#
sub carp(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
}

#
# instead of Carp::croak
#
sub croak(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
    die "\n";
}

#
# instead of Carp::cluck
#
sub cluck(@) {
    my $i = 0;
    my @cluck = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @cluck, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @cluck;
    print STDERR "\n";
    carp @_;
}

#
# instead of Carp::confess
#
sub confess(@) {
    my $i = 0;
    my @confess = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @confess, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @confess;
    print STDERR "\n";
    croak @_;
}

1;

__END__

=pod

=head1 NAME

Char::Eeucjp - Run-time routines for Char/EUCJP.pm

=head1 SYNOPSIS

  use Char::Eeucjp;

    Char::Eeucjp::split(...);
    Char::Eeucjp::tr(...);
    Char::Eeucjp::chop(...);
    Char::Eeucjp::index(...);
    Char::Eeucjp::rindex(...);
    Char::Eeucjp::lc(...);
    Char::Eeucjp::lc_;
    Char::Eeucjp::lcfirst(...);
    Char::Eeucjp::lcfirst_;
    Char::Eeucjp::uc(...);
    Char::Eeucjp::uc_;
    Char::Eeucjp::ucfirst(...);
    Char::Eeucjp::ucfirst_;
    Char::Eeucjp::fc(...);
    Char::Eeucjp::fc_;
    Char::Eeucjp::ignorecase(...);
    Char::Eeucjp::capture(...);
    Char::Eeucjp::chr(...);
    Char::Eeucjp::chr_;
    Char::Eeucjp::glob(...);
    Char::Eeucjp::glob_;

  # "no Char::Eeucjp;" not supported

=head1 ABSTRACT

This module is a run-time routines of the Char/EUCJP.pm.
Because the Char/EUCJP.pm automatically uses this module, you need not use directly.

=head1 BUGS AND LIMITATIONS

I have tested and verified this software using the best of my ability.
However, a software containing much regular expression is bound to contain
some bugs. Thus, if you happen to find a bug that's in Char::EUCJP software and not
your own program, you can try to reduce it to a minimal test case and then
report it to the following author's address. If you have an idea that could
make this a more useful tool, please let everyone share it.

=head1 HISTORY

This Char::Eeucjp module first appeared in ActivePerl Build 522 Built under
MSWin32 Compiled at Nov 2 1999 09:52:28

=head1 AUTHOR

INABA Hitoshi E<lt>ina@cpan.orgE<gt>

This project was originated by INABA Hitoshi.
For any questions, use E<lt>ina@cpan.orgE<gt> so we can share
this file.

=head1 LICENSE AND COPYRIGHT

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 EXAMPLES

=over 2

=item Split string

  @split = Char::Eeucjp::split(/pattern/,$string,$limit);
  @split = Char::Eeucjp::split(/pattern/,$string);
  @split = Char::Eeucjp::split(/pattern/);
  @split = Char::Eeucjp::split('',$string,$limit);
  @split = Char::Eeucjp::split('',$string);
  @split = Char::Eeucjp::split('');
  @split = Char::Eeucjp::split();
  @split = Char::Eeucjp::split;

  This function scans a string given by $string for separators, and splits the
  string into a list of substring, returning the resulting list value in list
  context or the count of substring in scalar context. Scalar context also causes
  split to write its result to @_, but this usage is deprecated. The separators
  are determined by repeated pattern matching, using the regular expression given
  in /pattern/, so the separators may be of any size and need not be the same
  string on every match. (The separators are not ordinarily returned; exceptions
  are discussed later in this section.) If the /pattern/ doesn't match the string
  at all, Char::Eeucjp::split returns the original string as a single substring, If it
  matches once, you get two substrings, and so on. You may supply regular
  expression modifiers to the /pattern/, like /pattern/i, /pattern/x, etc. The
  //m modifier is assumed when you split on the pattern /^/.

  If $limit is specified and positive, the function splits into no more than that
  many fields (though it may split into fewer if it runs out of separators). If
  $limit is negative, it is treated as if an arbitrarily large $limit has been
  specified If $limit is omitted or zero, trailing null fields are stripped from
  the result (which potential users of pop would do wel to remember). If $string
  is omitted, the function splits the $_ string. If /pattern/ is also omitted or
  is the literal space, " ", the function split on whitespace, /\s+/, after
  skipping any leading whitespace.

  A /pattern/ of /^/ is secretly treated if it it were /^/m, since it isn't much
  use otherwise.

  String of any length can be split:

  @chars  = Char::Eeucjp::split(//,  $word);
  @fields = Char::Eeucjp::split(/:/, $line);
  @words  = Char::Eeucjp::split(" ", $paragraph);
  @lines  = Char::Eeucjp::split(/^/, $buffer);

  A pattern capable of matching either the null string or something longer than
  the null string (for instance, a pattern consisting of any single character
  modified by a * or ?) will split the value of $string into separate characters
  wherever it matches the null string between characters; nonnull matches will
  skip over the matched separator characters in the usual fashion. (In other words,
  a pattern won't match in one spot more than once, even if it matched with a zero
  width.) For example:

  print join(":" => Char::Eeucjp::split(/ */, "hi there"));

  produces the output "h:i:t:h:e:r:e". The space disappers because it matches
  as part of the separator. As a trivial case, the null pattern // simply splits
  into separate characters, and spaces do not disappear. (For normal pattern
  matches, a // pattern would repeat the last successfully matched pattern, but
  Char::Eeucjp::split's pattern is exempt from that wrinkle.)

  The $limit parameter splits only part of a string:

  my ($login, $passwd, $remainder) = Char::Eeucjp::split(/:/, $_, 3);

  We encourage you to split to lists of names like this to make your code
  self-documenting. (For purposes of error checking, note that $remainder would
  be undefined if there were fewer than three fields.) When assigning to a list,
  if $limit is omitted, Perl supplies a $limit one larger than the number of
  variables in the list, to avoid unneccessary work. For the split above, $limit
  would have been 4 by default, and $remainder would have received only the third
  field, not all the rest of the fields. In time-critical applications, it behooves
  you not to split into more fields than you really need. (The trouble with
  powerful languages it that they let you be powerfully stupid at times.)

  We said earlier that the separators are not returned, but if the /pattern/
  contains parentheses, then the substring matched by each pair of parentheses is
  included in the resulting list, interspersed with the fields that are ordinarily
  returned. Here's a simple example:

  Char::Eeucjp::split(/([-,])/, "1-10,20");

  which produces the list value:

  (1, "-", 10, ",", 20)

  With more parentheses, a field is returned for each pair, even if some pairs
  don't match, in which case undefined values are returned in those positions. So
  if you say:

  Char::Eeucjp::split(/(-)|(,)/, "1-10,20");

  you get the value:

  (1, "-", undef, 10, undef, ",", 20)

  The /pattern/ argument may be replaced with an expression to specify patterns
  that vary at runtime. As with ordinary patterns, to do run-time compilation only
  once, use /$variable/o.

  As a special case, if the expression is a single space (" "), the function
  splits on whitespace just as Char::Eeucjp::split with no arguments does. Thus,
  Char::Eeucjp::split(" ") can be used to emulate awk's default behavior. In contrast,
  Char::Eeucjp::split(/ /) will give you as many null initial fields as there are
  leading spaces. (Other than this special case, if you supply a string instead
  of a regular expression, it'll be interpreted as a regular expression anyway.)
  You can use this property to remove leading and trailing whitespace from a
  string and to collapse intervaning stretches of whitespace into a single
  space:

  $string = join(" ", Char::Eeucjp::split(" ", $string));

  The following example splits an RFC822 message header into a hash containing
  $head{'Date'}, $head{'Subject'}, and so on. It uses the trick of assigning a
  list of pairs to a hash, because separators altinate with separated fields, It
  users parentheses to return part of each separator as part of the returned list
  value. Since the split pattern is guaranteed to return things in pairs by virtue
  of containing one set of parentheses, the hash assignment is guaranteed to
  receive a list consisting of key/value pairs, where each key is the name of a
  header field. (Unfortunately, this technique loses information for multiple lines
  with the same key field, such as Received-By lines. Ah well)

  $header =~ s/\n\s+/ /g; # Merge continuation lines.
  %head = ("FRONTSTUFF", Char::Eeucjp::split(/^(\S*?):\s*/m, $header));

  The following example processes the entries in a Unix passwd(5) file. You could
  leave out the chomp, in which case $shell would have a newline on the end of it.

  open(PASSWD, "/etc/passwd");
  while (<PASSWD>) {
      chomp; # remove trailing newline.
      ($login, $passwd, $uid, $gid, $gcos, $home, $shell) =
          Char::Eeucjp::split(/:/);
      ...
  }

  Here's how process each word of each line of each file of input to create a
  word-frequency hash.

  while (<>) {
      for my $word (Char::Eeucjp::split()) {
          $count{$word}++;
      }
  }

  The inverse of Char::Eeucjp::split is join, except that join can only join with the
  same separator between all fields. To break apart a string with fixed-position
  fields, use unpack.

=item Transliteration

  $tr = Char::Eeucjp::tr($variable,$bind_operator,$searchlist,$replacementlist,$modifier);
  $tr = Char::Eeucjp::tr($variable,$bind_operator,$searchlist,$replacementlist);

  This is the transliteration (sometimes erroneously called translation) operator,
  which is like the y/// operator in the Unix sed program, only better, in
  everybody's humble opinion.

  This function scans a EUC-JP string character by character and replaces all
  occurrences of the characters found in $searchlist with the corresponding character
  in $replacementlist. It returns the number of characters replaced or deleted.
  If no EUC-JP string is specified via =~ operator, the $_ variable is translated.
  $modifier are:

  ---------------------------------------------------------------------------
  Modifier   Meaning
  ---------------------------------------------------------------------------
  c          Complement $searchlist.
  d          Delete found but unreplaced characters.
  s          Squash duplicate replaced characters.
  r          Return transliteration and leave the original string untouched.
  ---------------------------------------------------------------------------

  To use with a read-only value without raising an exception, use the /r modifier.

  print Char::Eeucjp::tr('bookkeeper','=~','boep','peob','r'); # prints 'peekkoobor'

=item Chop string

  $chop = Char::Eeucjp::chop(@list);
  $chop = Char::Eeucjp::chop();
  $chop = Char::Eeucjp::chop;

  This fubction chops off the last character of a string variable and returns the
  character chopped. The Char::Eeucjp::chop function is used primary to remove the newline
  from the end of an input recoed, and it is more efficient than using a
  substitution. If that's all you're doing, then it would be safer to use chomp,
  since Char::Eeucjp::chop always shortens the string no matter what's there, and chomp
  is more selective. If no argument is given, the function chops the $_ variable.

  You cannot Char::Eeucjp::chop a literal, only a variable. If you Char::Eeucjp::chop a list of
  variables, each string in the list is chopped:

  @lines = `cat myfile`;
  Char::Eeucjp::chop(@lines);

  You can Char::Eeucjp::chop anything that is an lvalue, including an assignment:

  Char::Eeucjp::chop($cwd = `pwd`);
  Char::Eeucjp::chop($answer = <STDIN>);

  This is different from:

  $answer = Char::Eeucjp::chop($tmp = <STDIN>); # WRONG

  which puts a newline into $answer because Char::Eeucjp::chop returns the character
  chopped, not the remaining string (which is in $tmp). One way to get the result
  intended here is with substr:

  $answer = substr <STDIN>, 0, -1;

  But this is more commonly written as:

  Char::Eeucjp::chop($answer = <STDIN>);

  In the most general case, Char::Eeucjp::chop can be expressed using substr:

  $last_code = Char::Eeucjp::chop($var);
  $last_code = substr($var, -1, 1, ""); # same thing

  Once you understand this equivalence, you can use it to do bigger chops. To
  Char::Eeucjp::chop more than one character, use substr as an lvalue, assigning a null
  string. The following removes the last five characters of $caravan:

  substr($caravan, -5) = '';

  The negative subscript causes substr to count from the end of the string instead
  of the beginning. To save the removed characters, you could use the four-argument
  form of substr, creating something of a quintuple Char::Eeucjp::chop;

  $tail = substr($caravan, -5, 5, '');

  This is all dangerous business dealing with characters instead of graphemes. Perl
  doesn't really have a grapheme mode, so you have to deal with them yourself.

=item Index string

  $byte_pos = Char::Eeucjp::index($string,$substr,$byte_offset);
  $byte_pos = Char::Eeucjp::index($string,$substr);

  This function searches for one string within another. It returns the byte position
  of the first occurrence of $substring in $string. The $byte_offset, if specified,
  says how many bytes from the start to skip before beginning to look. Positions are
  based at 0. If the substring is not found, the function returns one less than the
  base, ordinarily -1. To work your way through a string, you might say:

  $byte_pos = -1;
  while (($byte_pos = Char::Eeucjp::index($string, $lookfor, $byte_pos)) > -1) {
      print "Found at $byte_pos\n";
      $byte_pos++;
  }

=item Reverse index string

  $byte_pos = Char::Eeucjp::rindex($string,$substr,$byte_offset);
  $byte_pos = Char::Eeucjp::rindex($string,$substr);

  This function works just like Char::Eeucjp::index except that it returns the byte
  position of the last occurrence of $substring in $string (a reverse Char::Eeucjp::index).
  The function returns -1 if $substring is not found. $byte_offset, if specified,
  is the rightmost byte position that may be returned. To work your way through a
  string backward, say:

  $byte_pos = length($string);
  while (($byte_pos = Char::EUCJP::rindex($string, $lookfor, $byte_pos)) >= 0) {
      print "Found at $byte_pos\n";
      $byte_pos--;
  }

=item Lower case string

  $lc = Char::Eeucjp::lc($string);
  $lc = Char::Eeucjp::lc_;

  This function returns a lowercased version of EUC-JP $string (or $_, if
  $string is omitted). This is the internal function implementing the \L escape
  in double-quoted strings.

  You can use the Char::Eeucjp::fc function for case-insensitive comparisons via Char::EUCJP
  software.

=item Lower case first character of string

  $lcfirst = Char::Eeucjp::lcfirst($string);
  $lcfirst = Char::Eeucjp::lcfirst_;

  This function returns a version of EUC-JP $string with the first character
  lowercased (or $_, if $string is omitted). This is the internal function
  implementing the \l escape in double-quoted strings.

=item Upper case string

  $uc = Char::Eeucjp::uc($string);
  $uc = Char::Eeucjp::uc_;

  This function returns an uppercased version of EUC-JP $string (or $_, if
  $string is omitted). This is the internal function implementing the \U escape
  in interpolated strings. For titlecase, use Char::Eeucjp::ucfirst instead.

  You can use the Char::Eeucjp::fc function for case-insensitive comparisons via Char::EUCJP
  software.

=item Upper case first character of string

  $ucfirst = Char::Eeucjp::ucfirst($string);
  $ucfirst = Char::Eeucjp::ucfirst_;

  This function returns a version of EUC-JP $string with the first character
  titlecased and other characters left alone (or $_, if $string is omitted).
  Titlecase is "Camel" for an initial capital that has (or expects to have)
  lowercase characters following it, not uppercase ones. Exsamples are the first
  letter of a sentence, of a person's name, of a newspaper headline, or of most
  words in a title. Characters with no titlecase mapping return the uppercase
  mapping instead. This is the internal function implementing the \u escape in
  double-quoted strings.

  To capitalize a string by mapping its first character to titlecase and the rest
  to lowercase, use:

  $titlecase = Char::Eeucjp::ucfirst(substr($word,0,1)) . Char::Eeucjp::lc(substr($word,1));

  or

  $string =~ s/(\w)(\w*)/\u$1\L$2/g;

  Do not use:

  $do_not_use = Char::Eeucjp::ucfirst(Char::Eeucjp::lc($word));

  or "\u\L$word", because that can produce a different and incorrect answer with
  certain characters. The titlecase of something that's been lowercased doesn't
  always produce the same thing titlecasing the original produces.

  Because titlecasing only makes sense at the start of a string that's followed
  by lowercase characters, we can't think of any reason you might want to titlecase
  every character in a string.

  See also P.287 A Case of Mistaken Identity
  in Chapter 6: Unicode
  of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

=item Fold case string

  P.860 fc
  in Chapter 27: Functions
  of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

  $fc = Char::Eeucjp::fc($string);
  $fc = Char::Eeucjp::fc_;

  New to Char::EUCJP software, this function returns the full Unicode-like casefold of
  EUC-JP $string (or $_, if omitted). This is the internal function implementing
  the \F escape in double-quoted strings.

  Just as title-case is based on uppercase but different, foldcase is based on
  lowercase but different. In ASCII there is a one-to-one mapping between only
  two cases, but in other encoding there is a one-to-many mapping and between three
  cases. Because that's too many combinations to check manually each time, a fourth
  casemap called foldcase was invented as a common intermediary for the other three.
  It is not a case itself, but it is a casemap.

  To compare whether two strings are the same without regard to case, do this:

  Char::Eeucjp::fc($a) eq Char::Eeucjp::fc($b)

  The reliable way to compare string case-insensitively was with the /i pattern
  modifier, because Char::EUCJP software has always used casefolding semantics for
  case-insensitive pattern matches. Knowing this, you can emulate equality
  comparisons like this:

  sub fc_eq ($$) {
      my($a,$b) = @_;
      return $a =~ /\A\Q$b\E\z/i;
  }

=item Make ignore case string

  @ignorecase = Char::Eeucjp::ignorecase(@string);

  This function is internal use to m/ /i, s/ / /i, split / /i and qr/ /i.

=item Make capture number

  $capturenumber = Char::Eeucjp::capture($string);

  This function is internal use to m/ /, s/ / /, split / / and qr/ /.

=item Make character

  $chr = Char::Eeucjp::chr($code);
  $chr = Char::Eeucjp::chr_;

  This function returns a programmer-visible character, character represented by
  that $code in the character set. For example, Char::Eeucjp::chr(65) is "A" in either
  ASCII or EUC-JP, not Unicode. For the reverse of Char::Eeucjp::chr, use Char::EUCJP::ord.

=item Filename expansion (globbing)

  @glob = Char::Eeucjp::glob($string);
  @glob = Char::Eeucjp::glob_;

  This function returns the value of $string with filename expansions the way a
  shell would expand them, returning the next successive name on each call.
  If $string is omitted, $_ is globbed instead. This is the internal function
  implementing the <*> operator.
  This function function when the pathname ends with chr(0x5C) on MSWin32.

  For economic reasons, the algorithm matches the command.com or cmd.exe's style
  of expansion, not the UNIX-like shell's. An asterisk ("*") matches any sequence
  of any character (including none). A question mark ("?") matches any one
  character or none. A tilde ("~") expands to a home directory, as in "~/.*rc"
  for all the current user's "rc" files, or "~jane/Mail/*" for all of Jane's mail
  files.

  For example, C<<..\\l*b\\file/*glob.p?>> on MSWin32 or UNIX will work as
  expected (in that it will find something like '..\lib\File/DosGlob.pm' alright).

  Note that all path components are case-insensitive, and that backslashes and
  forward slashes are both accepted, and preserved. You may have to double the
  backslashes if you are putting them in literally, due to double-quotish parsing
  of the pattern by perl.

  The Char::Eeucjp::glob function grandfathers the use of whitespace to separate multiple
  patterns such as <*.c *.h>. If you want to glob filenames that might contain
  whitespace, you'll have to use extra quotes around the spacy filename to protect
  it. For example, to glob filenames that have an "e" followed by a space followed
  by an "f", use either of:

  @spacies = <"*e f*">;
  @spacies = Char::Eeucjp::glob('"*e f*"');
  @spacies = Char::Eeucjp::glob(q("*e f*"));

  If you had to get a variable through, you could do this:

  @spacies = Char::Eeucjp::glob("'*${var}e f*'");
  @spacies = Char::Eeucjp::glob(qq("*${var}e f*"));

  Hint: Programmer Efficiency

  "When I'm on Windows, I use split(/\n/,`dir /s /b *.* 2>NUL`) instead of glob('*.*')"
  -- ina

=cut

