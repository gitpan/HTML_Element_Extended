package HTML::ElementGlob;

use strict;
use vars qw($VERSION $AUTOLOAD);

use HTML::ElementSuper;

$VERSION = '1.00';

####################################################
# glob_* methods do the HTML::Element type methods #
# on the glob structure itself, rather than muxing #
# the methods to its children.                     #
####################################################

sub glob_push_content {
  # This negates the default adoption behavior of HTML::Element
  # style objects, so that the child element does not believe
  # the glob is its parent.
  my $self = shift;
  my $cp;
  foreach (@_) {
    if (ref) {
      if (ref eq ref $self) {
	$cp = $_->glob_parent;
	$self->{_element}->push_content($_);
	$_->glob_parent($cp);
      } else {
	$cp = $_->parent;
	$self->{_element}->push_content($_);
	$_->parent($cp);
      }
    } else {
      $self->{_element}->push_content($_);
    }
  }
  $self;
}

sub glob_insert_element {
  # Negate adoption policy, again.
  my $self = shift;
  my $e = shift;
  if (ref $e) {
    my $cp = $e->parent;
    $self->{_element}->insert_element($e,@_);
    $e->parent($cp);
  } else {
    $self->{_element}->insert_element($e,@_);
  }
  $self;
}

sub glob_delete_content {
  # Do not propogate delete_content to children
  @{$_[0]->glob_content} = () unless $_[0]->glob_is_empty;
  $_[0];
}

sub glob_delete {
  # Do not propogate delete to children
  $_[0]->glob_delete_content;
  %{$_[0]} = ();
}

######################################################
# MUXed methods (pass invocation to children)        #
# Some methods do not really make sense in a globbed #
# context, so we try to 'do the right thing' here.   #
######################################################

sub push_content {
  # Push clones of elements
  my $self = shift;
  my @content = $self->{_element}->is_empty ? () :
    @{$self->{_element}->content()};
  print STDERR "Glob here.  Should be pushing ",join(',',@content),"\n";
  # Deal with the tail elements first
  foreach (1..$#content) {
    next unless ref $content[$_];
    $content[$_]->push_content($self->{_element}->clone(@_));
  }
  # First element can have the real copy
  $content[0]->push_content(@_);
}

sub insert_content {
  # Insert clones of elements
  my $self = shift;
  my @content = $self->{_element}->is_empty ? () :
    @{$self->{_element}->content()};
  my $element = shift;
  my @content = @{$self->{_element}->content()};
  # Deal with tail elements first
  foreach (1..$#content) {
    next unless ref $content[$_];
    $content[$_]->insert_content($self->{_element}->clone($element));
  }
  # First element gets the real copy
  $content[0]->insert_content($element) if ref $content[0];
}

sub wrap_content {
  # Wrap with clones of elements
  my $self = shift;
  my @content = $self->{_element}->is_empty ? () :
    @{$self->{_element}->content()};
  my @content = @{$self->{_element}->content()};
  # Deal with the tail elements first
  foreach (1..$#content) {
    next unless ref $content[$_];
    $content[$_]->wrap_content($self->{_element}->clone(@_));
  }
  # First element can have the real copy
  $content[0]->wrap_content(@_) if ref $content[0];
}

# Constructor

sub new {
  my $that = shift;
  my $class = ref($that) || $that;
  my $self = {};
  bless $self,$class;
  $self->{_element} = new HTML::ElementSuper @_;
  $self;
}

sub AUTOLOAD {
  # Methods starting with glob deal with glob management,
  # otherwise they get passed blindly to all children unless
  # they have been overridden above.
  my $self = shift;
  my $name = $AUTOLOAD;
  $name =~ s/.*:://;
  return if $name =~ /^DESTROY/;
  if ($name =~ s/^glob_//) {
    return $self->{_element}->$name(@_);
  }
  if (!$self->{_element}->is_empty) {
    my @results;
    foreach (@{$self->{_element}->content()}) {
      next unless ref;
      push(@results, $_->$name(@_));
    }
    return @results;
  }
}

1;
__END__

=head1 NAME

HTML::ElementGlob - Perl extension for managing HTML::Element based objects as a single object.

=head1 SYNOPSIS

  use HTML::ElementGlob;
  $element_a = new HTML::Element 'font', color => 'red';
  $element_b = new HTML::Element 'font', color => 'blue';
  $element_a->push_content('red');
  $element_b->push_content('blue');

  $p = new HTML::Element 'p';
  $p->push_content($element_a, ' and ', $element_b, ' boo hoo hoo');

  # Tag type of the glob is not really relevant unless
  # you plan on seeing the glob as_HTML()
  $eglob = new HTML::ElementGlob 'p';
  $eglob->glob_push_content($element_a, $element_b);
  # Alter both elements at once
  $eglob->attr(size => 5);

  # They still belong to their original parent
  print $p->as_HTML;

=head1 DESCRIPTION

HTML::ElementGlob is a managing object for multiple
HTML::Element(3) style elements.  The children of the glob
element retain their original parental elements and have
no knowledge of the glob that manipulates them.  All methods
that do not start with 'glob_' will be passed, sequentially, to
all elements contained within the glob element.  Methods
starting with 'glob_' will operate on the glob itself, rather
than being passed to its foster children.

For example, $eglob->attr(size => 3) will invoke attr(size => 3) on
all children contained by $eglob.  $eglob->glob_attr(size => 3), on
the other hand, will set the attr attribute on the glob itself.

The tag type passed to HTML::Element::Glob is largely
irrrelevant as far as how methods are passed to children.  However,
if you choose to invoke $eglob->as_HTML(), you might want to pick
a tag that would sensibly contain the globbed children for debugging
or display purposes.

The 'glob_*' methods that operate on the glob itself are limited
to those available in an HTML::Element(3).  All other methods get
passed blindly to the globbed children, which can be enhanced elements
with arbitrary methods, such as HTML::ElementSuper(3).

Element globs can contain other element globs.  In such cases, the
plain methods will cascade down to the leaf children.  'glob_*' methods,
of course, will not be propogated to children globs.  You will
have to rely on glob_content() to access those glob children and
access their 'glob_*' methods directly.

=head1 REQUIRES

HTML::ElementSuper(3)

=head1 AUTHOR

Matthew P. Sisk, E<lt>F<sisk@mojotoad.com>E<gt>

=head1 COPYRIGHT

Copyright (c) 1998 Matthew P. Sisk.
All rights reserved. All wrongs revenged. This program is free
software; you can redistribute it and/or modify it under the
same terms as Perl itself.

=head1 SEE ALSO

HTML::Element(3), HTML::ElementSuper, HTML::ElementRaw, HTML::Element::Table(3), perl(1).

=cut
