# HTML_Element_Extended

$NAME    = 'HTML_Element_Extended';
$VERSION = '1.03';

$| = 1;
$missing_modules = 0;

print "Checking for HTML::Element...";
eval {
  require HTML::Element;
};
if ($@) {
  print " failed\n";
  $missing_modules++;
  print <<EOT;

$@
HTML::Element is part of the HTML-Tree bundle.

EOT
    sleep(2);  # Don't hurry too much
} else {
    print " ok\n";
}

print <<EOT if $missing_modules;
The missing modules can be obtained from CPAN.  Visit
http://www.perl.com/CPAN/ to find a CPAN site near you.

EOT

use ExtUtils::MakeMaker;

WriteMakefile(
	      'NAME'    => $NAME,
	      'VERSION' => $VERSION,
	      dist      => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
	     );
