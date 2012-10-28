#!/usr/bin/perl
# The function integral calculates the value of the cumulative
# distribution function of the Gaussian distribution. Due to
# rounding errors, we abort the computation for extreme values.
#
# Frank Recker, 2012

use warnings;
use strict;
use Math::Trig;

foreach my $x (1.96,5) {
    foreach my $n (1,2,10,30,50,100,200) {
	my $v=integral($x,$n);
	my $e=error_term($x,$n);
	print "x: $x, n: $n, value: $v, error bound: $e\n";
    }
}

# x: Argument for the Gaussian integral
# n: Number of steps for the approximation
# return value: approximation of Phi(x)
sub integral() {
    my ($x,$n)=@_;

    die "n is to small: $n" if $n<1;
    die "x is out of the range: $x" if (($x<=-6.2) or ($x>=6.2));

    my $d=0;
    while($n>1) {
	--$n;
	$d=-$x*$x*($d+1/(2*$n+1))/(2*$n);
    }
    return (0.5+$x*($d+1)/sqrt(2*pi));
}

# x: Argument for the Gaussian integral
# n: Number of steps for the approximation
# An upper bound for the absolute difference of the result and Phi(x)
sub error_term() {
    my ($x,$n)=@_;

    return "no bound" if 2*$n<$x*$x;
    my $erg=1;
    for(my $j=1;$j<=$n;++$j) {
	$erg*=$x*$x/2/$j;
    }
    return $x*$erg/(2*$n+1)/sqrt(2*pi);
}
