package HTML::ElementSuper;

# Extend the HTML::Element class to allow the following:
#    positional reporting
#    content replacement
#    masking (i.e., in the structure but invisible to traverse)
#    content wrapping
#    cloning of self and arbitrary elements

use strict;
use vars qw($VERSION @ISA);
use Carp;
use Data::Dumper;

use HTML::Element;

@ISA = qw(HTML::Element);

$VERSION = '1.00';

### Begin Positional extension ###

sub addr {
  my $self = shift;
  return undef unless $self->parent;
  my $sibs = $self->parent->content;
  my $addr;
  foreach (0..$#{$sibs}) {
    $addr = $_;
    last if $sibs->[$_] eq $self;
  }
  $addr;
}

sub position {
  # Report coordinates by chasing addr's up the
  # HTML::ElementSuper tree.  We know we've reached
  # the top when a) there is no parent, or b) the
  # parent is some HTML::Element unable to report
  # it's position.
  my $self = shift;
  my($addr, @pp);
  # Only grab addr if we know what it means and have one.
  eval { $addr = $self->addr; };
  defined $addr || return ();
  # Only grab parent position if parent is able to report.
  eval { @pp = $self->parent->position; };
  @pp ? (@pp,$addr) : $addr;
}

sub depth {
  my $self = shift;
  my $depth = 0;
  my $p = $self;
  while ($p = $p->parent) {
    ++$depth;
  }
  $depth;
}

sub delete_attr {
  my $self = shift;
  my $attr = lc shift;
  my $old = $self->{$attr};
  delete $self->{$attr};
  $old;
}

# Handy debugging tools

sub push_position {
  # Push positional coordinates into own content
  my $self = shift;
  $self->push_content(' (' . join(',', $self->position) . ')');
}

sub push_depth {
  # Push HTML tree depth into own content
  my $self = shift;
  $self->push_content('(' . $self->depth . ')');
}

### End Positional extension ###


### Begin Cloner extension ###

sub clone {
  # Clone HTML::Element style trees.
  # Clone self unless told otherwise.
  # Cloning comes in handy when distributing methods
  # such as push_content - you don't want the same
  # HTML::Element tree across multiple nodes, just
  # a copy of it - since HTML::Element nodes only
  # recognize one parent.
  my $self = shift;
  my @args = @_;
  my($clone, $node, @clones);
  @args || push(@args, $self);
  my($VAR1, $VAR2, $VAR3);
  $Data::Dumper::Purity  = 1;
  foreach $node (@args) {
    eval(Dumper($node));
    carp("$@ $node") if $@;
    $clone = $VAR1;
    push(@clones, $clone);
  }
  $#clones ? @clones : $clones[0];
}

### End Cloner extension ###

### Begin Maskable extension ###

sub mask {
  # Set/Return masking.  If masked, do not appear in a traverse()
  my $self = shift;
  my $mode = shift;
  if (defined $mode) {
    # Unfortunately can have multiple maskings, so we keep count.
    if ($mode == 0) {
      --$self->{_mask} unless $self->{_mask} <= 0;
    } else {
      ++$self->{_mask};
    }
  }
  $self->{_mask};
}

# Routines that use traverse, such as as_HTML, are not called
# on a per-element basis.  as_HTML always belongs to the top level
# element that initiated the call.  A maskable element should not
# be seen, though.  Overriding as_HTML will not do the trick since
# we cannot guarantee that the top level element is a maskable-aware
# element with the overridden method.  Therefore, for maskable
# elements, we override traverse itself, which does get called on a
# per-element basis. If this element is masked, simply return from
# traverse, making this element truly invisible to parents.  This
# means that traverse is no longer guranteed to actually visit all
# elements in the tree. For that, you must rely on the actual
# contents of each element.
sub traverse {
  my $self = shift;
  return if $self->mask;
  $self->SUPER::traverse(@_);
}

sub super_traverse {
  # Saftey net for catching wayward masked elements.
  my $self = shift;
  $self->SUPER::traverse(@_);
}

### End Maskable extension ###

### Begin Replacer extension ###

sub replace_content {
  my $self = shift;
  $self->delete_content;
  $self->push_content(@_);
}

### End Replacer extension ###

### Begin Wrapper extension ###

sub wrap_content {
  my $self = shift;
  my $wrap = shift;
  my $content = $self->content;
  if (ref $content) {
    $wrap->push_content(@$content);
    @$content = ($wrap);
  } else {
    $self->push_content($wrap);
  }
  $wrap;
}

### End Wrapper extension ###

sub new {
  my $that = shift;
  my $class = ref($that) || $that;
  my $self = new HTML::Element @_;
  bless $self,$class;
  $self;
}

1;
__END__

=head1 NAME

HTML::ElementSuper - Perl extension for HTML::Element(3)

=head1 SYNOPSIS

  use HTML::ElementSuper;

  ### Positional extension
  $e = new HTML::ElementSuper 'font';
  $sibling_number = $e->addr();
  $e2 = new HTML::ElementSuper 'p';
  $e2->push_content($e);
  # 
  @coords = $e->position();
  $depth_in_pos_tree = $e->depth();

  ### Replacer extension
  $er = new HTML::ElementSuper 'font';
  # Tree beneath $er, if present, is dropped.
  $er->replace_content(new HTML::Element 'p');

  ### Wrapper extension
  $ew = new HTML::ElementSuper;
  $ew->push_content("Tickle me, baby");
  $ew->wrap_content(new HTML::Element 'font', color => 'pink');
  print $ew->as_HTML();

  ### Maskable extension
  $em = new HTML::ElementSuper 'td';
  $em->mask(1);
  print $em->as_HTML; # nada
  $em->mask(0);
  print $em->as_HTML; # $e and its children are visible

  ### Cloning of own tree or another element's tree
  ### (is this the correct clomenature?  :-)
  $a = new HTML::ElementSuper 'font', size => 2;
  $b = new HTML::ElementSuper 'font', color => 'red';
  $a_clone  = $a->clone;
  $b_clone = $a->clone($b);
  # Multiple elements can be cloned
  @clone_clones = $a_clone->clone($a_clone, $b_clone);


=head1 DESCRIPTION

HTML::ElementSuper is an extension for HTML::Element(3) that provides
several new methods to assist in element manipulation.  An HTML::ElementSuper
has the following additional properties:

   * report is coordinate position in a tree of its peers
   * replace its contents
   * wrap its contents in a new element
   * mask itself so that it and its descendants are invisible
     to traverse()
   * clone itself and other HTML::Element based object trees

Note that these extensions were originally developed to assist in
implementing the HTML::ElementTable(3) class, but were thought to be
of general enough utility to warrant their own package.

=head1 METHODS

=over

=item new('tag', attr => 'value', ...)

Return a new HTML::ElementSuper object.  Exactly like the constructor
for HTML::Element(3), takes a tag type and optional attributes.

=item addr()

Returns the position of this element in relation to its siblings
based on the content of the parent, starting with 0.  Returns
undef if this element has no parent. In other words, this returns
the index of this element in the content array of the parent.

=item position()

Returns the coordinates of this element in the tree it inhabits.
This is accomplished by succesively calling addr() on ancestor
elements until either a) an element that does not support these
methods is found, or b) there are no more parents.  The resulting
list is the n-dimensional coordinates of the element in the tree.

=item replace_content(@new_content)

Simple shortcut method that deletes the current contents of the
element before adding the new.

=item wrap_content($wrapper_element)

Wraps the existing content in the provided element.  If the
provided element happens to be a non-element, a push_content
is performed instead.

=item mask

=item mask(mode)

Toggles whether or not this element is visible to parental
methods that visit the element tree using traverse(), such as
as_HTML().  Valid arguments for mask() are 0 and 1.  Returns
the current setting without an argument.

This might seem like a strange method to have, but it helps
in managing dynamic tree structures.  For example, in HTML::ElementTable(3),
when you expand a table cell you simply mask what it covers
rather than destroy it.  Shrinking the table cell reveals that
content to as_HTML() once again.

=item super_traverse()

Traverses all elements in the tree, even if they are masked.

=item clone

=item clone(@elements)

Returns a clone of elements and all of their descendants.  Without
arguments, the element clones itself, otherwise it clones the
elements provided as arguments. Any element can be cloned as long
as it is HTML::Element(3) based.  This method is very handy for
duplicating tree structures since an HTML::Element cannot have
more than one parent at any given time...hence "tree".

=back

=head1 REQUIRES

HTML::Element(3), Data::Dumper(3)

=head1 AUTHOR

Matthew P. Sisk, E<lt>F<sisk@mojotoad.com>E<gt>

=head1 COPYRIGHT

Copyright (c) 1998 Matthew P. Sisk.
All rights reserved. All wrongs revenged. This program is free
software; you can redistribute it and/or modify it under the
same terms as Perl itself.

=head1 SEE ALSO

HTML::Element(3), HTML::ElementGlob(3), HTML::ElementRaw(3), HTML::ElementTable(3), perl(1).
