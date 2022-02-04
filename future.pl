#!/usr/bin/perl -w

use feature qw(say);
use subs qw(mkcps);

if (@ARGV) {
  mkcps(@ARGV);
} else {
  die "provide some files!";
}

sub mkcps {
  foreach my $in (@_) {
    if (-f $in) {
      say ":: reading $in...";
      open(my $inh, '<', $in) or die $!;
        while ($_ = <$inh>) {
          unless ($_ =~ /^\s*$/ or $_ =~ /^\s*#/) {
            $_ =~ /^\s*(\S+)\s*(\S*)$/;
            my ($src, $dst) = ($1, $2 eq "" ? "." : $2);
              ...
          }
        }
      close($inh);
    } else {
      say ":: $in does not exist...";
    }
  }
}
