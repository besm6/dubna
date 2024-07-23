#!/usr/bin/perl -w
print q@<pre style='line-height: 1.2; letter-spacing: 0.1em; font-family: monospace, fixed, Courier;'>@;
$ff = chr(12);
$nextover = 0;

sub doline {
        my ($l) = @_;
	$over = $nextover;
	$nextover = 0;
	$nextover = 1 if $l =~ s/\\//;
	if ($over) {
                chop $l;
                print "<div style='margin-top:-1.2em;margin-bottom:-1.2em'>$l</div>\n";
        }
	else { print $l; }
}

while (<>) {
	s/</&lt;/g;
        my $l = $_;
        if (/^(.*)$ff(.*)$/) {
                $l1 = $1; $l2 = $2;
                doline($l1 . "\n");
                print STDERR "Cannot overstrike after form feed <$l1>\n" if $nextover;
                $nextover = 0;
                print '<hr>';
                doline($l2);
        } else {
                doline($l);
        }
}
print '</pre>';
