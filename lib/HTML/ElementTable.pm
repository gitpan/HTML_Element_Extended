package HTML::ElementTable;

use strict;
use vars qw($VERSION @ISA $AUTOLOAD);
use Carp;

use HTML::ElementSuper;
use HTML::ElementGlob;

@ISA = qw(HTML::ElementSuper);

$VERSION = '1.01';

# Enforced adoption policy such that positional coords are untainted.
my %Valid_Children;
++$Valid_Children{'HTML::ElementTable::RowElement'};
++$Valid_Children{'HTML::ElementTable::HeaderElement'};
my $VC = join(', ',sort keys %Valid_Children);

# In order to maintain the integrity of the table, we have to
# police the content methods in the <TABLE> and <TR> elements.
# This is because the table expects grid coordinates - since
# each element reports it's own coordinates, superfluous
# elements will screw that process up.
#
# Any new content related methods in the superclass should
# be reflected here.

my @Content_Methods = qw(
			 push_content
			 insert_element
			 replace_content
			 wrap_content
			 );
my $CM = join('|', @Content_Methods);

##################
# Native Methods #
##################

sub extent {
  my $self = shift;
  @_ || return ($self->maxrow,$self->maxcol);
  my($maxrow, $maxcol) = @_;
  defined $maxrow && defined $maxcol
    or croak "Max row and col dimensions required";
#  print "current r,c: ",$self->maxrow, ',', $self->maxcol,"\n";
#  print "attempt r,c: $maxrow,$maxcol\n";

  # Hit rows
  $self->_adjust_content($self, $maxrow, $self->maxrow)
    if $maxrow != $self->maxrow;
  
  # Hit columns
  my @rows = $self->is_empty ? () : @{$self->content};
  if ($maxcol != $self->maxcol) {
    grep($self->_adjust_content($_, $maxcol, $self->maxcol), @rows);
  }

  # New data cells caused by new rows will be
  # automatically taken care of within _adjust_content

#  print "interim r,c: ",$self->maxrow, ',', $self->maxcol,"\n";

  # Re-glob
  $self->refresh;
}

sub refresh {
  my $self = shift;
  my($row,$col,$p_row,$p_col);

  # Reconstruct globs. There are two main
  # globs - the row and column collections - plus the
  # globs representing each row and each column of cells.
  
  # Clear old row and column globs
  grep($_->glob_delete_content,@{$self->_rows->glob_content})
    unless $self->_rows->glob_is_empty;
  grep($_->glob_delete_content,@{$self->_cols->glob_content})
    unless $self->_cols->glob_is_empty;
  $self->_rows->glob_delete_content;
  $self->_cols->glob_delete_content;

  my $colcnt;
  my $maxcol = -1;
  foreach $row (@{$self->content}) {
    # New glob for each row, added to rows glob
    $p_row = $self->_rowglob;
    $p_row->alias($row);
    $self->_rows->glob_push_content($p_row);
    $colcnt = 0;
    foreach $col ($row->is_empty ? () : @{$row->content}) {
      # Add each cell to the individual row glob
      $p_row->glob_push_content($col);
      if ($colcnt > $maxcol) {
	# If a new column, make column glob
	$p_col = $self->_colglob;
	$self->_cols->glob_push_content($p_col);
	++$maxcol;
      } else {
	# Otherwise use the existing column glob
	$p_col = $self->_cols->glob_content->[$colcnt];
      }
      # Add the cell to the column glob
      $p_col->glob_push_content($col);
      ++$colcnt;
    }
  }
  $self;
}

sub _adjust_content {
  my $self = shift;
  my($e,$limit,$old) = @_;
  ref $e or croak "Element required";
  defined $limit or croak "Index limit required";
  if (!defined $old) {
    grep(++$old,@{$e->content});
  }
  if ($limit < $old) {
    # We are trimming
    my($i, $c, $found);
    $i = $c = -1;
    # We mess with $i like this to avoid having
    # non data elements throw off our grid count
    foreach (@{$e->content}) {
      ++$c;
      next unless ref;
      ++$i;
      if ($i == $limit) {
	$found = $c;
	next;
      }
      $_->delete if $found;           
    }
    @{$e->content} = @{$e->content}[0..$found];
  }
  elsif ($limit > $old) {
    # We are growing
    my($tag,$d,$r);
    foreach ($old+1..$limit) {
      if ($e->tag eq 'table') {
	$r = new HTML::ElementTable::RowElement;
	if ($self->maxcol != -1) {
	  # Brand new colums...use -1 as old to get 0
	  $self->_adjust_content($r,$self->maxcol,-1);
	}
	$e->push_content($r);
      }
      else {
	$d = new HTML::ElementTable::DataElement;
	$d->blank_fill($self->blank_fill);
	$e->push_content($d);
      }
    }
  }
  $e;
}

sub maxrow {
  my($self, $maxrow) = @_;
  $self->extent($maxrow,$self->maxcol) if defined $maxrow;
  $self->_rows->glob_is_empty ? -1 : $#{$self->_rows->glob_content};
}

sub maxcol {
  my($self, $maxcol) = @_;
  $self->extent($self->maxrow, $maxcol) if defined $maxcol;
  $self->_cols->glob_is_empty ? -1 : $#{$self->_cols->glob_content};
}

# Index and glob hooks
sub cell {
  my $self = shift;
  my ($r,$c,@elements);
  while (@_) {
    ($r,$c) = (shift,shift);
    defined $r && defined $c || croak "Missing coordinate";
    my $r = $self->row($r);
    croak "Row $r is empty" if $r->glob_is_empty;
    $#{$r->glob_content} >= $c && $c >= 0 || next;
    push(@elements,$r->glob_content->[$c]);
  }
  return undef unless @elements;
  @elements > 1 ? $self->_cellglob(@elements) : $elements[0];
}
sub row {
  my $self = shift;
  @_ || croak "Index required";
  my @out = grep($_ > $self->maxrow,@_);
  croak "Rows(@out) out of range" if @out;
  @_ > 1 ? $self->_rowglob(@{$self->_rows->glob_content}[@_])
    : $self->_rows->glob_content->[$_[0]];
}
sub col {
  my $self = shift;
  @_ || croak "Index required";
  my @out = grep($_ > $self->maxcol,@_);
  croak "Columns(@out) out of range" if @out;
  @_ > 1 ? $self->_colglob(@{$self->_cols->glob_content}[@_])
    : $self->_cols->glob_content->[$_[0]];
}
sub box {
  my $self = shift;
  my($r1,$c1,$r2,$c2) = @_;
  defined $r1 && defined $c1 && defined $r2 && defined $c2 ||
    croak "Two coordinate pairs required";
  # Normalize for ascending counts
  ($r1, $r2) = ($r2, $r1) if $r2 < $r1;
  ($c1, $c2) = ($c2, $c1) if $c2 < $c1;
  # Optimize on rows if we can
  if ($c1 == 0 && $c2 == $self->maxcol) {
    return $self->row($r1 .. $r2);
  }
  # Otherwise glob the box
  my(@coords,$r,$c);
  foreach $r ($r1 .. $r2) {
    foreach $c ($c1 .. $c2) {
      push(@coords,$r,$c);
    }
  }
  $self->cell(@coords);
}
sub table {
  my $self = shift;
  # Both _rows and _cols are effectively globs of the
  # whole table.  We return row here so that valid
  # TR attrs can be captured.
  $self->_rows;
}

sub mask_mode {
  # Should span antics of children push/pull or mask/reveal
  # siblings?
  my($self,$mode) = @_;
  $self->{_maskmode} = $mode if defined $mode;
  $self->{_maskmode};
}

# Main glob hooks
sub _rows {
  my $self = shift;
  return $self->{_rows};
}
sub _cols {
  my $self = shift;
  return $self->{_cols};
}

sub _glob {
  my $self = shift;
  my $tag = shift || croak "No tag";
  my $g = new HTML::ElementGlob $tag;
  $g->glob_push_content(@_) if @_;
  $g;
}

sub _colglob {
  my $self = shift;
  $self->_glob('tr',@_);
}

sub _rowglob {
  my $self = shift;
  my $g = new HTML::ElementTable::RowGlob;
  $g->glob_push_content(@_) if @_;
  $g;
}

sub _cellglob {
  my $self = shift;
  $self->_glob('tr',@_);
}

sub colspan_dispatch {
  # Dispatch for children to send notice of colspan changes
  my($self,$row,$col,$span,$oldspan) = @_;
  defined $row && defined $col || croak "Cell row and column required";
  defined $span || croak "Span setting required";
  $span = 1 unless $span;
  $oldspan = 1 unless $oldspan;
  return if $span == $oldspan;
  my $rspan = $self->cell($row,$col)->attr('rowspan');
  $rspan = 1 unless $rspan;
  # We are either masking or revealing
  my $mask = $span > $oldspan ? 1 : 0;
  my($r,$c,$tmp);
  if ($oldspan > $span) {
    $tmp = $span; $span = $oldspan; $oldspan = $tmp;
  }
  foreach $c ($col + $oldspan .. $col + $span - 1) {
    foreach $r ($row .. $row + $rspan - 1) {
      next if $c == $col && $r == $row;
      next unless $self->cell($r,$c);
      $self->cell($r,$c)->mask($mask & $self->mask_mode);
    }
  }
}

sub rowspan_dispatch {
  # Dispatch for children to send notice of rowspan changes
  my($self,$row,$col,$span,$oldspan) = @_;
  defined $row && defined $col || croak "Cell row and column required";
  defined $span || croak "Span setting required";
  $span = 1 unless $span;
  $oldspan = 1 unless $oldspan;
  return if $span == $oldspan;
  my $cspan = $self->cell($row,$col)->attr('colspan');
  $cspan = 1 unless $cspan;
  my $mask = $span > $oldspan ? 1 : 0;
  my($r,$c,$tmp);
  if ($oldspan > $span) {
    $tmp = $span; $span = $oldspan; $oldspan = $tmp;
  }
  foreach $r ($row + $oldspan .. $row + $span - 1) {
    foreach $c ($col .. $col + $cspan - 1) {
      next if $r == $row && $c == $col;
      $self->cell($r,$c)->mask($mask & $self->mask_mode);
    }
  }
}

sub _orphan {
  # Who's ugly and unwanted?
  # Note that our pickiness excludes comment tags...ugh.
  my $self = shift;
  scalar grep(!$Valid_Children{ref $_},@_);
}

sub blank_fill {
  # Should blank cells be populated with "&nbsp;" in order
  # for BGCOLOR to show up?
  my $self = shift;
  my $mode = shift;
  if (defined $mode) {
    $self->{_blank_fill} = $mode;
    $self->table->blank_fill($mode);
  }
  $self->{_blank_fill};
}

sub beautify {
  # Set mode for making as_HTML output human readable.
  # Broadcasts to component elements.
  my $self = shift;
  my $mode = shift;
  if (defined $mode) {
    $self->{_beautify} = $mode;
    # Broadcast to row elements as well as data elements
    $self->row(0..$self->maxrow)->beautify($mode);
    $self->col(0..$self->maxcol)->beautify($mode);
  }
  $self->{_beautify};
}

sub new {
  my $that = shift;
  my $class = ref($that) || $that;

  # Extract complex attributes
  my($attr,$val,$maxrow,$maxcol,%e_attrs);
  while ($attr = shift) {
    $val = shift;
    if ($attr =~ /^maxrow/) {
      $maxrow = $val;
    } elsif ($attr =~ /^maxcol/) {
      $maxcol = $val;
    } else {
      $e_attrs{$attr} = $val;
    }
  }
  my $self = new HTML::ElementSuper 'table', %e_attrs;
  bless $self,$class;

  # The tag choices for globs are arbitrary, but
  # these should at least make some sort of since
  # if the globs are rendered as_HTML.
  $self->{_rows} = $self->_rowglob;
  $self->{_rows}->tag('table');
  $self->{_cols} = $self->_colglob;

  $self->mask_mode(1);
  $self->blank_fill(0);

  $self->extent($maxrow, $maxcol) if defined $maxrow || defined $maxcol;

  $self;
}

sub AUTOLOAD {
  # Do a check for content control, pass along to super.
  my $self = shift;
  my $name = $AUTOLOAD;
  $name =~ s/.*://;
  return if $name =~ /^DESTROY/;
  if ($name =~ /$CM/o) {
    # Picky adoption
    croak "Attempted adoption of something other than $VC"
      if $self->_orphan($_[0]);
  }
  my $sname = "SUPER::$name";
  $self->$sname(@_);
}

################
# Sub packages #
################

{ package HTML::ElementTable::Element;

  use strict;
  use vars qw( @ISA );
  use HTML::ElementSuper;

  @ISA = qw(HTML::ElementSuper);

  # "Beautify" mode
  # Primarily intended for as_HTML, this mode affects
  # how the source HTML appears.  When beautified, the
  # starttag and endtags are modified to include indentation.
  sub beautify {
    my $self = shift;
    defined $_[0] ? $self->{_beautify} = shift : $self->{_beautify};
  }

  sub starttag {
    my $self = shift;
    my $spc;
    if ($self->beautify) {
      my @position = $self->position;
      $spc = ' ' x @position;
      $spc = "\n$spc";
    }
    $spc .  $self->SUPER::starttag;
  }

  sub new {
    my $that = shift;
    my $class = ref($that) || $that;
    my $self = new HTML::ElementSuper @_;
    bless $self, $class;
    $self;
  }

  # End HTML::ElementTable::Element
}

{ package HTML::ElementTable::DataElement;

  use strict;
  use vars qw( @ISA );

  @ISA = qw(HTML::ElementTable::Element);

  ####################
  # Override Methods #
  ####################

  sub attr {
    # Keep tabs on COLSPAN and ROWSPAN
    my $self = shift;
    my($attr, $val) = map(lc $_, @_);
    if (defined $val) {
      if ($attr eq 'colspan') {
	$self->parent->colspan_dispatch($self->addr, $val, $self->attr($attr));
      } elsif ($attr eq 'rowspan') {
	$self->parent->rowspan_dispatch($self->addr, $val, $self->attr($attr));
      }
    }
    $self->SUPER::attr(@_);
  }

  sub delete_attr {
    # Keep tabs on spans, again.
    my $self = shift;
    my $attr = lc $_[0];
    if ($attr eq 'colspan' || $attr eq 'rowspan') {
      $self->attr($_[0], 0);
    }
    $self->SUPER::delete_attr(@_);
  }

  sub blank_fill {
    # Set/return mode for populating empty cells with "&nbsp;" so
    # that BGCOLOR will show up.
    my $self = shift;
    @_ ? $self->{_blank_fill} = shift : $self->{_blank_fill};
  }

  ####################
  # Codus horribilus #
  ####################

  # This bit of unfortunate code is necessary because of the
  # shortcomings of the as_HTML method in HTML::Element.  as_HTML
  # uses HTML::Entity::encode_entities to process nodes that
  # are not elements.  For some reason, "<>&" is passed to the
  # encode_entities method, which effectively makes it impossible
  # to pass a literal "&" into your HTML output.  Specifically,
  # in order for the BGCOLOR to show up in an empty table cell,
  # you must include a "&nbsp;".  However, you cannot pass a literal
  # "&", for it always gets translated to "&amp;", thus placing
  # "&nbsp;" as literal text in your cells.  Nor can you pass the
  # code for a non-breaking space - it remains unchanged since the
  # encode list is limited.
  #
  # So we cheat.  We override the starttag method, including the "&nbsp;"
  # along with the starttag if the cell is empty.  This could be
  # avoided if HTML::Element relaxed a little and laid off the hand
  # holding.
  #
  # Oh - we can't just override as_HTML() and do it correctly, because
  # as_HTML() is only invoked from the top level element - which could be
  # a plain jane HTML::Element and know nothing of HTML::Element::Table
  # elements.
  #
  # Ooo-glay!
  sub starttag {
    my $self = shift;
    my $c = $self->content;
    (!ref $c || !@$c) && $self->blank_fill ?
      $self->SUPER::starttag . "&nbsp; " : $self->SUPER::starttag;
  }

  # Constructor

  sub new {
    my $that = shift;
    my $class = ref($that) || $that;
    my $self = new HTML::ElementTable::Element 'td';
    bless $self, $class;
    $self->blank_fill(0);
    $self;
  }

  # End HTML::ElementTable::DataElement
}

{ package HTML::ElementTable::HeaderElement;

  use strict;
  use vars qw( @ISA );
  use Carp;

  @ISA = qw(HTML::ElementTable::DataElement);

  sub new {
    my $that = shift;
    my $class = ref($that) || $that;
    my $self = new HTML::Element::Table::DataElement;
    bless $self, $class;
    $self->tag('th');
    $self;
  }

  # End HTML::ElementTable::HeaderElement
}

{ package HTML::ElementTable::RowElement;

  use strict;
  use vars qw( @ISA $AUTOLOAD );
  use Carp;

  @ISA = qw(HTML::ElementTable::Element);


  # Restrict children so that Table coordinate system is untainted.
  my %Valid_Children;
  ++$Valid_Children{'HTML::ElementTable::DataElement'};
  ++$Valid_Children{'HTML::ElementTable::HeaderElement'};
  my $VC = join(',', sort keys %Valid_Children);

  ##################
  # Native Methods #
  ##################

  sub colspan_dispatch {
    # Dispatch for children to send notice of colspan changes
    my $self = shift;
    $self->parent->colspan_dispatch($self->addr, @_);
  }

  sub rowspan_dispatch {
    # Dispatch for children to send notice of rowspan changes
    my $self = shift;
    $self->parent->rowspan_dispatch($self->addr, @_);
  }

  sub _orphan {
    # Who's ugly and unwanted?
    my $self = shift;
    scalar grep(!$Valid_Children{ref $_}, @_);
  }

  sub new {
    my $that = shift;
    my $class = ref($that) || $that;
    my $self = new HTML::ElementTable::Element 'tr';
    bless $self,$class;
    $self;
  }

  sub AUTOLOAD {
    # Do a check for content control, pass along to super.
    my $self = shift;
    my $name = $AUTOLOAD;
    $name =~ s/.*://;
    return if $name =~ /^DESTROY/;
    if ($name =~ /$HTML::ElementTable::CM/o) {
      # Picky adoption
      croak "Attempted adoption of something other than $VC"
	if $self->_orphan($_[0]);
    }
    my $sname = "SUPER::$name";
    $self->$sname(@_);
  }

  # End HTML::ElementTable::RowElement
}

{ package HTML::ElementTable::RowGlob;

  use strict;
  use vars qw( @ISA );

  use HTML::ElementGlob;

  @ISA = qw(HTML::ElementGlob);

  # Designate attributes that are valid for <TR> tags.
  my %TR_ATTRS;
  grep(++$TR_ATTRS{$_},qw( align valign bgcolor ));

  sub alias {
    # alias() allows us to designate an actual row element
    # that contains our data/header elements.  If we can optimize
    # an attribute on the <TR> tag rather than each <TD> or <TH>
    # tag, then we do so.
    my $self = shift;
    my $alias = shift;
    if (ref $alias) {
      $self->{_alias} = $alias;
    }
    $self->{_alias};
  }

  sub attr {
    # Catch the optimization opportunities.
    my $self = shift;
    if ($self->alias && $TR_ATTRS{lc $_[0]}) {
      $self->SUPER::delete_attr(@_);
      return $self->alias->attr(@_);
    }
    $self->SUPER::attr(@_);
  }

  sub mask {
    # In addition to masking all children <TD> and <TH> tags,
    # we have to mask the row itself - accessible via the alias().
    my $self = shift;
    if ($self->alias) {
      return $self->alias->mask(@_);
    }
    $self->SUPER::mask(@_);
  }

  sub beautify {
    # Broadcast beautify to alias
    my $self = shift;
    if ($self->alias) {
      return $self->alias->beautify(@_);
    }
    $self->SUPER::beautify(@_);
  }

  sub new {
    my $that = shift;
    my $class = ref($that) || $that;
    my $self = new HTML::ElementGlob 'table';
    bless $self, $class;
    $self;
  }

  # End HTML::ElementTable::RowGlob
}

1;
__END__

=head1 NAME

HTML::ElementTable - Perl extension for manipulating a table composed of HTML::Element style components.

=head1 SYNOPSIS

  use HTML::ElementTable;
  # Create a table 0..10 x 0..12
  $t = new HTML::ElementTable maxrow => 10, maxcol => 12;

  # Populate cells with coordinates
  $t->table->push_position;

  # Manipulate <TABLE> tag
  $t->attr('cellspacing',0);
  $t->attr('border',1);
  $t->attr('bgcolor','#DDBB00');

  # Manipulate entire table - optimize on <TR> or pass to all <TD>
  $t->table->attr('align','left');
  $t->table->attr('valign','top');

  # Manipulate rows (optimizes on <TR> if possible)
  $t->row(0,2,4,6)->attr('bgcolor','#9999FF');

  # Manipulate columns (all go to <TD> tags within column)
  $t->col(0,4,8,12)->attr('bgcolor','#BBFFBB');

  # Manipulate boxes (all go to <TD> elements
  # unless it contains full rows, then <TR>)
  $t->box(7,1 => 10,3)->attr('bgcolor','magenta');
  $t->box(7,7 => 10,5)->attr('bgcolor','magenta');
  $t->box(8,9 => 9,11)->attr('bgcolor','magenta');
  $t->box(7,10 => 10,10)->attr('bgcolor','magenta');

  # individual <TD> or <TH> attributes
  $t->cell(8,6)->attr('bgcolor','#FFAAAA');
  $t->cell(9,6)->attr('bgcolor','#FFAAAA');
  $t->cell(7,9, 10,9, 7,11, 10,11)->attr('bgcolor','#FFAAAA');

  # Take a look
  print $t->as_HTML;

=head1 DESCRIPTION

HTML::ElementTable provides a highly enhanced HTML::ElementSuper structure
with methods designed to easily manipulate table elements by
using coordinates.  Elements can be manipulated in bulk by individual
cells, arbitrary groupings of cells, boxes, columns, rows, or the
entire table.

=head1 PUBLIC METHODS

Table coordinates start at 0,0 in the upper left cell.

CONSTRUCTOR

=over 4

=item new()

=item new(maxrow => row, maxcol => col)

Return a new HTML::ElementTable object.  If the number of rows
and columns were provided, all elements required for the rows
and columns will be initialized as well.  See extent().

=back

TABLE CONFIGURATION

=over 4

=item extent()

=item extent(maxrow, maxcolumn)

Set or return the extent of the current table.  The I<maxrow> and
I<maxcolumn> parameters indicate the maximum row and column coordinates
you desire in the table.  These are the coordinates of the lower
right cell in the table, starting from (0,0) at the upper left. Providing
a smaller extent than the current one will shrink the table with no 
ill effect, provided you do not mind losing the information in the
clipped cells.

=item maxrow()

Set or return the coordinate of the last row.

=item maxcol()

Set or return the coordinate of the last column.

=back

ELEMENT ACCESS

Unless accessing a single element, most table element access is accomplished
through I<globs>, which are collections of elements that behave as if they
were a single element object.

Whenever possible, globbed operations are optimized into the most appropriate
element.  For example, if you set an attribute for a row glob, the attribute
will be set either on the <TR> element or the collected <TD> elements,
whichever is appropriate.

See L<HTML::ElementGlob(3)> for more information on element globs.

=over

=item cell(row,col,[row2,col2],[...])

Access an individual cell or collection of cells by their coordinates.

=item row(row,[row2,...])

Access the contents of a row or collection of rows by row coordinate.

=item col(col,[col2,...])

Access the contents of a column or collection of columns by column coordinate.

=item box(row_a1,col_a1,row_a2,col_a2,[row_b1,col_b1,row_b2,col_b2],[...])

Access the contents of a span of cells, specified as a box consisting of
two sets of coordinates.  Multiple boxes can be specified.

=item table()

Access all cells in the table.  This is different from manipulating the
table object itself, which is reserved for such things as CELLSPACING
and other attributes specific to the <TABLE> tag.  However, since table()
returns a glob of cells, if the attribute is more appropriate for the
top level <TABLE> tag, it will be placed there rather than in each
<TR> tag or every <TD> tag.

=back

ELEMENT/GLOB METHODS

The interfaces to a single table element or a glob of elements are
identical.  All methods available from the HTML::ElementSuper class
are also available to a table element or glob of elements.
See L<HTML::ElementSuper(3)> for details on these methods.

Briefly, here are some of the more useful methods provided by HTML::ElementSuper:

=over

=item attr()

=item push_content()

=item replace_content()

=item wrap_content()

=item clone([element])

=item mask([mode])

=back

TABLE SPECIFIC EXTENSIONS

=over

=item blank_fill([mode])

Set or return the current fill mode for blank cells. The default
is 0 for HTML::Element::Table elements.  When most browsers
render tables, if they are empty you will get a box the color
of your browser background color rather than the BGCOLOR of that
cell.  When enabled, empty cells are provided with an '&nbsp;',
or invisible content, which will trigger the rendering of the
BGCOLOR for that cell.

=back

=head1 NOTES ON GLOBS

Globbing was a convenient way to treat arbitrary collections of
table cells as if they were a single HTML element.  Methods are
generally passed blindly and sequentially to the elements they
contain.

Most of the time, this is fairly intuitive, such as when you are
setting the attributes of the cells.

Other times, it might be problematic, such as with push_content().
Do you push the same object to all of the cells? HTML::Element based
classes only support one parent, so this breaks if you try to push the
same element into multiple parental hopefuls.  In the specific
case of push_content() on globs, the elements that eventually
get pushed are clones of the originally provided content.  It
works, but it is not necessarily what you expect.  An incestuous
HTML element tree is probably not what you want anyway.

See L<HTML::ElementGlob(3)> for more details on how globs work.

=head1 REQUIRES

HTML::ElementSuper, HTML::ElementGlob

=head1 AUTHOR

Matthew P. Sisk, E<lt>F<sisk@mojotoad.com>E<gt>

=head1 ACKNOWLEDGEMENTS

Thanks to William R. Ward for some conceptual nudging.

=head1 COPYRIGHT

Copyright (c) 1998 Matthew P. Sisk.
All rights reserved. All wrongs revenged. This program is free
software; you can redistribute it and/or modify it under the
same terms as Perl itself.

=head1 SEE ALSO

HTML::ElementSuper(3), HTML::ElementGlob(3), HTML::Element(3), perl(1).

=cut
