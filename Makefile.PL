use ExtUtils::MakeMaker;

$NAME     = 'HTML';
$DISTNAME = "HTML_Element_Extended";
$VERSION  = "1.01";

# Check for non-standard modules used by this module set.

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
HTML::Element is part of the HTML::Tree bundle. You can obtain this
from CPAN/modules/by-module/HTML/HTML-Tree-x.x.tar.gz

EOT
    sleep(2);  # Don't hurry too much
} else {
    print " ok\n";
}

print "Checking for Data::Dumper...";
eval {
    require Data::Dumper;
};
if ($@) {
    print " failed\n";
    $missing_modules++;
    print <<EOT;
$@
Data::Dumper is part of the standard perl distribution these days.
You should probably upgrade or fix your version.

EOT
    sleep(2);  # Don't hurry too much
} else {
    print " ok\n";
}

print <<EOT if $missing_modules;
The missing modules can be obtained from CPAN.  Visit
<URL:http://www.perl.com/CPAN/> to find a CPAN site near you.

EOT

# Time to make a makefile

WriteMakefile(
	      VERSION   => $VERSION,
	      DISTNAME  => $DISTNAME,
	      NAME      => $NAME,                       
	      'dist'    => {
			    COMPRESS => 'gzip -9f',
			    SUFFIX   => 'gz',
			   },
	     );

package MY;

# Pass HTML_Element_Table version number to pod2man
sub manifypods
{
  my $self = shift;
  my $ver = $self->{VERSION} || "";
  local($_) = $self->SUPER::manifypods(@_);
  $_;
}
