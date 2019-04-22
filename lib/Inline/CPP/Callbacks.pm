use strict; use warnings; use v5.10;
package Inline::CPP::Callbacks;

use Exporter 'import';
our @EXPORT = qw(get_callback_filter);
our $VERSION = "0.001";

use Carp;
use open IO => ':encoding(UTF-8)';
use utf8;
use File::Basename qw(basename dirname fileparse);

## Use our updated or patched modules...
#my $srctop;
#BEGIN { $srctop = dirname(dirname(dirname(($INC{"Inline/CPP/Callbacks.pm"}//die)))) }
#use lib $srctop;
#
#use Inline::CPP ();
#use Inline::CPP::Parser::RecDescent (); # for fixkey()

sub get_callback_filter(@) {
  my $obj = __PACKAGE__->new(@_);
  return sub{ $obj->filter($_[0]) }
}

sub new {
  my $class = shift;
  bless { 
          perlprefix => "perl_",
          @_
        }, $class;
}

# copy a string, replacing specified substrings with specified texts.
# $edits = [ [charoffset, length, repl_string], ...] # in any order
# TODO: Pass $input by ref (first test if faster than Perl's automatic COW alias)
# RETURNS: The constructed string.
sub edit_string($$) {
  my ($input, $editsarg) = @_;
  my @edits = sort { $a->[0] <=> $b->[0] } @$editsarg;
  #use Vis; say dvis '### @edits\n';
  my $output = q{};
  my $eindex = 0;
  my $ipos = 0;
  while ($ipos < length($input) || $eindex <= $#edits) {
    if ($eindex > $#edits) {
      $output .= substr($input, $ipos);
      last;
    }
    my ($offset, $length, $repl) = @{ $edits[$eindex] };
    if ($ipos < $offset) {
      confess "bug" if $offset >= length($input);
      $output .= substr($input, $ipos, $offset-$ipos);
      $ipos = $offset + $length;
    }
    $output .= $repl;
    $eindex++;
  }
  $output
}

sub _gen_newSV($$;$) {
  my ($type, $cppexpr, $datalen) = @_;
  return "newSViv($cppexpr)" if $type =~ /^(?:int|long|short)\b/;
  return "newSVuv($cppexpr)" if $type =~ /^unsigned/;
  return "newSVnv($cppexpr)" if $type eq "double";
  return "newSVpv($cppexpr,0)" if $type =~ /char\s*\*/ && ! defined($datalen);
  confess "Unhandled argument type '$type'"
}
sub _gen_POP($) {
  my $rtype = shift;
  return "POPn" if $rtype eq "double";
  return "POPi" if $rtype eq "int";
  return "POPu" if $rtype eq "unsigned" or $rtype eq "unsigned int";
  return "POPl" if $rtype eq "long";
  return "POPul" if $rtype eq "unsigned long";
  confess "Unhandled return type '$rtype'"
}
sub _gen_perlmethcall($$$) {
  my ($classname, $part, $perlprefix) = @_;
  my $s = "\tdSP;\n";
  $s .= "\tSV *err_tmp;\n";
  $s .= "\t\tint _perl_retcount;\n";
  $s .= "\t\t".$part->{rtype}." retval;\n" if $part->{rtype} ne "void";
  $s .= "\t\tENTER;\n";
  $s .= "\t\tSAVETMPS;\n";
  $s .= "\t\tPUSHMARK(SP);\n";
  $s .= "\t\tEXTEND(SP,".(1+@{$part->{args}}).");\n";
  $s .= "\t\t// TODO: Figure out how to initialize m__PerlObjPtr to NULL\n";
  $s .= "\t\tif(m__PerlObjPtr == NULL) { __uninit_PerlObjPtr(); }\n";
  $s .= "\t\tPUSHs(sv_mortalcopy(m__PerlObjPtr));\n";
  foreach my $arg (@{ $part->{args} }) {
    $s .= "\t\tPUSHs(sv_2mortal("._gen_newSV($arg->{type},$arg->{name})."));\n";
  }
  my $numret = ($part->{rtype} ne "void" ? 1 : 0);

  $s .= "\t\tPUTBACK;\n";
  $s .= "\t\t_perl_retcount = call_method(\"${perlprefix}".$part->{name}."\", "
        .($numret ? "G_SCALAR" : "G_DISCARD")."|G_EVAL"
        .");\n";
  $s .= "\t\tSPAGAIN;\n";

  $s .= "\t\terr_tmp = ERRSV;\n";
  $s .= "\t\tif (SvTRUE(err_tmp)) {\n";
  $s .= "\t\t\tcroak(\"%s\", SvPV_nolen(err_tmp));\n";
  $s .= "\t\t}\n";


  $s .= "\t\tif (_perl_retcount != $numret) { cout << \"ERROR in generated proxy method: Callback to perl ${classname}::".$part->{name}." unexpectedly returned \" << _perl_retcount << \"items!\" << endl; }\n";
  if ($part->{rtype} ne "void") {
    $s .= "\t\tretval = "._gen_POP($part->{rtype}).";\n";
    $s .= "\t\tPUTBACK;\n";
  }
  $s .= "\t\tFREETMPS;\n";
  $s .= "\t\tLEAVE;\n";
  $s .= "\t\treturn retval;\n" if $part->{rtype} ne "void";
  $s
}
sub _gen_xtra_member($$) {
  my ($classname, $curr_scope) = @_;
  "\n"
  .($curr_scope ne "private" ? "  private:\n" : "")
  ."\tSV * m__PerlObjPtr;  // FIXME: patch ctor to initialze to undef\n"
  ."\tvoid __uninit_PerlObjPtr() {\n"
  ."\t    fprintf(stderr,\"ERROR: set_perl_object() not called on $classname before attempted call-back\\n\");\n"
  ."\t    exit(123);\n"
  ."\t}\n"
  ."\t/* TEMP DEBUGGING... */\n"
  ."\tvoid printSV(const char *label, SV * sv) {
         printf(\"%-32s= %#10lx = %lu (REFCNT=%d)\\n\", 
                label, (unsigned long)sv, (unsigned long)sv,
                (int)SvREFCNT(sv));
      }\n"
  ."  public:\n"

  ."\tSV *set_perl_object(SV *obj) {
        static bool first = 1;
        if (first) {
          setvbuf(stdout, NULL, _IONBF, 0);
          setvbuf(stderr, NULL, _IONBF, 0);
          first = 0;
        }

        //printSV(\"## set_perl_object: obj\",obj);
        if (SvREADONLY(obj)) { croak(\"set: READONLY obj\"); }

        m__PerlObjPtr = obj;

        SV *ret = SvREFCNT_inc(m__PerlObjPtr); // RETVAL will be _dec'd
        //printSV(\"## set_perl_object: ret\",ret);
        return ret;
   }\n"

  ."\tSV * get_perl_obj() { 
        //printSV(\"##             : m__PerlObjPtr\", m__PerlObjPtr);
        if (SvREADONLY(m__PerlObjPtr)) { croak(\"get: READONLY\"); }
        if (m__PerlObjPtr == NULL) return &PL_sv_undef;
        if (!SvOK(m__PerlObjPtr)) {
          croak(\"### ERROR: get_perl_obj: !SvOK.\");
        }
        SV *ret = SvREFCNT_inc(m__PerlObjPtr); // RETVAL will be _dec'd
        printSV(\"## get_perl_obj: ret\",ret);
        return ret;
   }\n"

  .($curr_scope ne "public" ? "  ${curr_scope}:\n" : "")
}
sub _engulf_tag($$) { # returns (start, length) to cover tag and adjacent spaces
  my $part = $_[1];
  #use Vis; Carp::cluck(dvis '## $part');
  my $length = length($part->{filtercmd});
  my $start = $part->{offset} - $length;
  # Extend replaced segement to eat surrounding spaces
  while (substr($_[0],$start-1,1) eq " ") { --$start; ++$length; }
  while (substr($_[0],$start+$length,1) eq " ") { ++$length; }
  return ($start, $length);
}
sub filter {
  my $o = shift;
  $o->{input} = $_[0] if @_;
  croak "Missing or empty 'input'" unless $o->{input};
  my $parser = $o->get_parser;

  local $SIG{__DIE__} = sub{ confess @_ };
  # "code" is the top-level production
  my $start_lineno = 1; # TODO: Figure out how to set this
  $parser->code($o->{input}, $start_lineno);

  #use Vis; warn dvis '################# $parser->{data}';
  my $xtra_member_generated;
  my @edits;  # list of [offset, rmchars, repl_string]
  my %callback_classnames;
  while (my ($classname, $parts) = each %{ $parser->{data}{class} }) {
    my %seen_methnames;
    foreach my $part (@$parts) {
      my $repl;
      if ($part->{thing} eq "method") {
        $seen_methnames{$part->{name}}++;
        if (my $macro = $part->{filtercmd}) {
          if ($macro eq "FILTER_CALLPERL") {
            $repl = "\n"._gen_perlmethcall($classname, $part, $o->{perlprefix})."    ";
            $callback_classnames{$classname} = 1;
            push @edits, [ (index($o->{input},"}",$part->{offset})+1)||die,
                           0,
                           _gen_xtra_member($classname, $part->{scope}) 
                         ]
              unless $xtra_member_generated++;
          }
          else {
            die "Invalid class-impl '${macro}' at line $part->{line}\n";
          }
        }
      }
      push @edits, [ _engulf_tag($o->{input}, $part), $repl ]
        if defined $repl;
    }
    foreach my $part (@$parts) {
      my $repl;
      if ($part->{thing} eq "filtercmd") {
        my $macro = $part->{filtercmd};
        if ($macro eq "FILTER_IMPL_PUREVIRTS") {
          # Generate complete methods for all unimplemented base-class virtuals
          my $curr_scope = $part->{scope};
          $repl = "";
          $repl .= _gen_xtra_member($classname, $curr_scope) unless $xtra_member_generated++;
          foreach my $ip (grep{ $_->{thing} eq "inherits" } @$parts) {
            my $bclassname = $ip->{name};
            my $bparts = $parser->{data}{class}{$bclassname};
            croak "base class '${bclassname}' not defined" unless defined $bparts;
            foreach my $bp (@$bparts) {
              next if $bp->{thing} ne "method";
              next unless $bp->{abstract};
              next if $bp->{scope} eq "private";
              next if $seen_methnames{$bp->{name}};
              $repl .= "\n";
              $repl .= "  ".($curr_scope=$bp->{scope}).":\n" if $bp->{scope} ne $curr_scope;
              $repl .= "\tvirtual ".$bp->{rtype}." ".$bp->{name}."("
                       .join(", ", map{$_->{type}." ".$_->{name}} @{ $bp->{args} })
                       .") { /* pure in ${bclassname} */\n";
              $repl .= _gen_perlmethcall($classname, $bp, $o->{perlprefix});
              $repl .= "\t}";
            }
            $callback_classnames{$classname} = 1;
          }
        }
        else {
          die "Invalid class-body '${macro}' at line $part->{line}\n";
        }
      }
      push @edits, [ _engulf_tag($o->{input}, $part), $repl ]
        if defined $repl;
    }
  }
  my $prefix = <<'END_PREFIX';
#ifdef __INLINE_CPP_NAMESPACE_STD
using namespace std;
#endif

END_PREFIX
  unshift @edits, [0,0,$prefix];

  my $result = edit_string($o->{input}, \@edits);

  ###TEMP
  warn "### saving code in /tmp/j4";
  open my $fh, ">", "/tmp/j4" or die $!;
  print $fh $result;
  close $fh or die;

  return $result;
}

sub get_parser {
  my $o = shift;
  #Inline::CPP::get_parser(bless {}, __PACKAGE__."::FakeClass");

  no warnings qw/ once /;    ## no critic (warnings)
  $::RD_ERRORS = 1;  
  $::RD_WARN = 1;  
  $::RD_HINT = 1;    # Turns on Parse::RecDescent's warnings/diagnostics.
  my $parser = Parse::RecDescent->new(grammar());
  # Provide fake data so we can just use grammar as-is
  #  from Inline::CPP::Parser::RecDescent
  $parser->{data}{typeconv}{type_kind} = {};
  $parser->{data}{typeconv}{input_expr} =  {};
  $parser->{data}{typeconv}{output_expr} = {};
  $parser->{data}{typeconv}{valid_types} = {};
  $parser->{data}{typeconv}{valid_rtypes} = {};
  $parser->{data}{AUTOWRAP} = 0;
  return $parser;
}

# Started with a copy of grammar() in the patched version of Inline/CPP/Parser/RecDescent.pm
sub grammar {
  return <<'END';

{ use Data::Dumper; }

{ 
  sub fixkey { &$Inline::CPP::Parser::RecDescent::fixkey }
}

{
    sub handle_class_def {
        my ($thisparser, $def) = @_;
#         print "Found a class: $def->[0]\n";
        my $class = $def->[0];
        my @parts;
        for my $part (@{$def->[1]}) { push @parts, @$_ for @$part }
        push @{$thisparser->{data}{classes}}, $class
            unless defined $thisparser->{data}{class}{$class};
        $thisparser->{data}{class}{$class} = \@parts;
#   print "Class $class:\n", Dumper \@parts;
        Inline::CPP::Parser::RecDescent::typemap($thisparser, $class);
        [$class, \@parts];
    }
    sub handle_typedef {
        my ($thisparser, $t) = @_;
        my ($name, $type) = @{$t}{qw(name type)};
#   print "found a typedef: $name => $type\n";

        # XXX: this doesn't handle non-class typedefs that we could handle,
        # e.g. "typedef int my_int_t"

        if ($thisparser->{data}{class}{$type}
            && !exists($thisparser->{data}{class}{$name})) {
            push @{$thisparser->{data}{classes}}, $name;
            $thisparser->{data}{class}{$name} = $thisparser->{data}{class}{$type};
            Inline::CPP::Parser::RecDescent::typemap($thisparser, $name);
        }
        $t;
    }
    sub handle_enum {
        my ($thisparser, $t) = @_;
        $t;
    }
}

code: part(s) {1}

part: comment
    | typedef
      {
        handle_typedef($thisparser, $item[1]);
        1;
      }
    | enum
      {
        my $t = handle_enum($thisparser, $item[1]);
        push @{$thisparser->{data}{enums}}, $t;
        1;
      }
    | class_def
      {
         handle_class_def($thisparser, $item[1]);
     1;
      }
    | function_def
      {
#         print "found a function: $item[1]->{name}\n";
         my $name = $item[1]->{name};
     my $i=0;
     for my $arg (@{$item[1]->{args}}) {
        $arg->{name} = 'dummy' . ++$i unless defined $arg->{name};
     }
     Inline::CPP::Parser::RecDescent::strip_ellipsis($thisparser,
                          $item[1]->{args});
     push @{$thisparser->{data}{functions}}, $name
           unless defined $thisparser->{data}{function}{$name};
     $thisparser->{data}{function}{$name} = $item[1];
#    print Dumper $item[1];
     1;
      }
    | all

typedef: 'typedef' class IDENTIFIER(?) '{' <commit> class_part(s?) '}' IDENTIFIER ';'
       {
     my ($class, $parts);
         $class = $item[3][0] || 'anon_class'.($thisparser->{data}{anonclass}++);
         ($class, $parts)= handle_class_def($thisparser, [$class, $item{fixkey('class_part(s?)')}]);
     { thing => 'typedef', name => $item[8], type => $class, body => $parts }
       }
       | 'typedef' IDENTIFIER IDENTIFIER ';'
       { { thing => 'typedef', name => $item[3], type => $item[2] } }
       | 'typedef' /[^;]*/ ';'
       {
#         dprint "Typedef $item{__DIRECTIVE1__} is too heinous\n";
         { thing => 'comment'}
       }

enum: 'enum' IDENTIFIER(?) '{' <leftop: enum_item ',' enum_item> '}' ';'
       {
    { thing => 'enum', name => $item{fixkey('IDENTIFIER(?)')}[0],
          body => $item{__DIRECTIVE1__} }
       }

enum_item: IDENTIFIER '=' <commit> /[0-9]+/
         { [$item{IDENTIFIER}, $item{__PATTERN1__}] }
         | IDENTIFIER
         { [$item{IDENTIFIER}, undef] }

class_def: class IDENTIFIER '{' <commit> class_part(s?) '}' ';'
           {
              [@item{'IDENTIFIER',fixkey('class_part(s?)')}]
       }
     | class IDENTIFIER ':' <commit> <leftop: inherit ',' inherit>
            '{' class_part(s?) '}' ';'
       {
          push @{$item{fixkey('class_part(s?)')}}, [$item{__DIRECTIVE2__}];
          [@item{'IDENTIFIER',fixkey('class_part(s?)')}]
       }

inherit: scope IDENTIFIER
    { {thing => 'inherits', name => $item[2], scope => $item[1]} }

class_part: comment { [ {thing => 'comment'} ] }
      | scope ':' <commit> class_decl(s?)
            {
          for my $part (@{$item{fixkey('class_decl(s?)')}}) {
                  $_->{scope} = $item[1] for @$part;
          }
          $item{fixkey('class_decl(s?)')}
        }
      | class_decl(s)
            {
          for my $part (@{$item[1]}) {
                  $_->{scope} = $thisparser->{data}{defaultscope}
            for @$part;
          }
          $item[1]
        }

class_decl: comment { [{thing => 'comment'}] }
          | typedef { [ handle_typedef($thisparser, $item[1]) ] }
          | enum { [ handle_enum($thisparser, $item[1]) ] }
          | class_def
            {
               my ($class, $parts) = handle_class_def($thisparser, $item[1]);
               [{ thing => 'class', name => $class, body => $parts }];
            }
          | FILTERCMD
            {
               [{ thing => 'filtercmd', @{ $item[1] } }];
            }
          | method_def
        {
              $item[1]->{thing} = 'method';
#         print "class_decl found a method: $item[1]->{name}\n";
          my $i=0;
          for my $arg (@{$item[1]->{args}}) {
        $arg->{name} = 'dummy' . ++$i unless defined $arg->{name};
          }
          Inline::CPP::Parser::RecDescent::strip_ellipsis($thisparser,
                           $item[1]->{args});
          [$item[1]];
        }
          | member_def
        {
#         print "class_decl found one or more members:\n", Dumper(\@item);
              $_->{thing} = 'member' for @{$item[1]};
          $item[1];
        }

function_def: operator <commit> ';'
              {
                   $item[1]
              }
            | operator <commit> smod(?) code_block
              {
                  $item[1]
              }
            | IDENTIFIER '(' <commit> <leftop: arg ',' arg>(s?) ')' smod(?) code_block
              {
                {name => $item{IDENTIFIER}, args => $item{__DIRECTIVE2__}, rtype => '' }
              }
            | rtype IDENTIFIER '(' <leftop: arg ',' arg>(s?) ')' ';'
              {
                {rtype => $item[1], name => $item[2], args => $item{__DIRECTIVE1__} }
              }
            | rtype IDENTIFIER '(' <leftop: arg ',' arg>(s?) ')' smod(?) code_block
              {
                {rtype => $item{rtype}, name => $item[2], args => $item{__DIRECTIVE1__} }
              }

method_def: operator <commit> method_imp
            {
#               print "method operator:\n", Dumper $item[1];
               $item[1];
            }

          | IDENTIFIER '(' <commit> <leftop: arg ',' arg>(s?) ')' method_imp
            {
#         print "con-/de-structor found: $item[1]\n";
              {name => $item[1], args => $item{__DIRECTIVE2__}, 
               @{ $item{method_imp} },  # abstract, etc.
              };
            }
          | rtype IDENTIFIER '(' <leftop: arg ',' arg>(s?) ')' method_imp
            {
#         print "method found: $item[2]\n";
          $return =
                {name => $item[2], rtype => $item[1], args => $item[4],
                 @{ $item{method_imp} },  # abstract, etc.
                 rconst => $thisparser->{data}{smod}{const},
                };
          $thisparser->{data}{smod}{const} = 0;
            }

operator: rtype(?) 'operator' /\(\)|[^()]+/ '(' <leftop: arg ',' arg>(s?) ')'
          {
#            print "Found operator: $item[1][0] operator $item[3]\n";
            {name=> "operator $item[3]", args => $item[5], ret => $item[1][0]}
          }

# By adding smod, we allow 'const' member functions. This would also bind to
# incorrect C++ with the word 'static' after the argument list, but we don't
# care at all because such code would never be compiled successfully.

# By adding init, we allow constructors to initialize references. Again, we'll
# allow them anywhere, but our goal is not to enforce c++ standards -- that's
# the compiler's job.
method_imp: smod(?) ';'                    { [] }  
          | smod(?) '=' <commit> '0' ';'   { [abstract => 1] }
          | smod(?) initlist(?) '{' (comment)(s?) FILTERCMD '}' 
              { [has_impl => 'filtercmd', @{ $item{FILTERCMD} }] }
          | smod(?) initlist(?) code_block { [has_impl => 1] }
          | smod(?) '=' '0' code_block     { [abstract => 1, has_impl => 1] } # MS VC

initlist: ':' <leftop: subexpr ',' subexpr>

member_def: anytype <leftop: var ',' var> ';'
            {
          my @retval;
          for my $def (@{$item[2]}) {
              my $type = join '', $item[1], @{$def->[0]};
          my $name = $def->[1];
#             print "member found: type=$type, name=$name\n";
          push @retval, { name => $name, type => $type };
          }
          \@retval;
            }

var: star(s?) IDENTIFIER '=' expr { [@item[1,2]] }
   | star(s?) IDENTIFIER '[' expr ']' { [@item[1,2]] }
   | star(s?) IDENTIFIER          { [@item[1,2]] }

arg: type IDENTIFIER '=' expr
     {
#       print "argument $item{IDENTIFIER} found\n";
#       print "expression: $item{expr}\n";
    {type => $item[1], name => $item{IDENTIFIER}, optional => 1,
     offset => $thisoffset}
     }
   | type IDENTIFIER
     {
#       print "argument $item{IDENTIFIER} found\n";
       {type => $item[1], name => $item{IDENTIFIER}, offset => $thisoffset}
     }
   | type { {type => $item[1]} }
   | '...'
     { {name => '...', type => '...', offset => $thisoffset} }

ident_part: /[~_a-z]\w*/i '<' <commit> <leftop: IDENTIFIER ',' IDENTIFIER>(s?) '>'
        {
       $item[1].'<'.join('', @{$item[4]}).'>'
        }

      | /[~_a-z]\w*/i
        {
           $item[1]
        }

IDENTIFIER: <leftop: ident_part '::' ident_part>
        {
              my $x = join '::', @{$item[1]};
#              print "IDENTIFIER: $x\n";
              $x
        }

# Special keywords of the form FILTER...
# e.g. 'FILTER_CALLPERL' used by a preprocessor to insert code
FILTERCMD: /FILTER[^\s;:]*/
  { [filtercmd => $item[1], offset => $thisoffset, line => $thisline] }

# Parse::RecDescent is retarded in this one case: if a subrule fails, it
# gives up the entire rule. This is a stupid way to get around that.
rtype: rtype2 | rtype1
rtype1: TYPE star(s?)
        {
         $return = $item[1];
         $return .= join '',' ',@{$item[2]} if @{$item[2]};
#    print "rtype1: $return\n";
#          return undef
#            unless(defined$thisparser->{data}{typeconv}{valid_rtypes}{$return});
        }
rtype2: modifier(s) TYPE star(s?)
    {
         $return = $item[2];
         $return = join ' ',grep{$_}@{$item[1]},$return
           if @{$item[1]};
         $return .= join '',' ',@{$item[3]} if @{$item[3]};
#    print "rtype2: $return\n";
#          return undef
#            unless(defined$thisparser->{data}{typeconv}{valid_rtypes}{$return});
     $return = 'static ' . $return
       if $thisparser->{data}{smod}{static};
         $thisparser->{data}{smod}{static} = 0;
    }

type: type2 | type1
type1: TYPE star(s?)
        {
         $return = $item[1];
         $return .= join '',' ',@{$item{fixkey('star(s?)')}} if @{$item{fixkey('star(s?)')}};
#    print "type1: $return\n";
#          return undef
#            unless(defined$thisparser->{data}{typeconv}{valid_types}{$return});
        }
type2: modifier(s) TYPE star(s?)
    {
         $return = $item{TYPE};
         $return = join ' ',grep{$_}@{$item[1]},$return if @{$item[1]};
         $return .= join '',' ',@{$item{fixkey('star(s?)')}} if @{$item{fixkey('star(s?)')}};
#    print "type2: $return\n";
#          return undef
#            unless(defined$thisparser->{data}{typeconv}{valid_types}{$return});
    }

anytype: anytype2 | anytype1
anytype1: TYPE star(s?)
         {
           $return = $item[1];
           $return .= join '',' ',@{$item[2]} if @{$item[2]};
         }
anytype2: modifier(s) TYPE star(s?)
         {
           $return = $item[2];
           $return = join ' ',grep{$_}@{$item[1]},$return if @{$item[1]};
           $return .= join '',' ',@{$item[3]} if @{$item[3]};
         }

comment: m{\s* // [^\n]* \n }x
       | m{\s* /\* (?:[^*]+|\*(?!/))* \*/  ([ \t]*)? }x

# long and short aren't recognized as modifiers because they break when used
# as regular types. Another Parse::RecDescent problem is greedy matching; I
# need tmodifier to "give back" long or short in cases where keeping them would
# cause the modifier rule to fail. One side-effect is 'long long' can never
# be parsed correctly here.
modifier: tmod
        | smod { ++$thisparser->{data}{smod}{$item[1]}; ''}
    | nmod { '' }
tmod: 'unsigned' # | 'long' | 'short'
smod: 'const' | 'static'
nmod: 'extern' | 'virtual' | 'mutable' | 'volatile' | 'inline'

scope: 'public' | 'private' | 'protected'

class: 'class' { $thisparser->{data}{defaultscope} = 'private'; $item[1] }
     | 'struct' { $thisparser->{data}{defaultscope} = 'public'; $item[1] }

star: '*' | '&'

code_block: /$Inline::CPP::Parser::RecDescent::code_block/

# Consume expressions
expr: <leftop: subexpr OP subexpr> {
    my $o = join '', @{$item[1]};
#   print "expr: $o\n";
    $o;
}
subexpr: /$Inline::CPP::Parser::RecDescent::funccall/ # Matches a macro, too
       | /$Inline::CPP::Parser::RecDescent::string/
       | /$Inline::CPP::Parser::RecDescent::number/
       | UOP subexpr
OP: '+' | '-' | '*' | '/' | '^' | '&' | '|' | '%' | '||' | '&&'
UOP: '~' | '!' | '-' | '*' | '&'

TYPE: IDENTIFIER

all: /.*/

END
}

1;

__END__

=pod

=encoding utf8

=head1 NAME

Inline::CPP::Callbacks - Write methods of C++ classes in Perl 

=head1 SYNOPSIS

    use Inline::CPP::Callbacks 'get_callback_filter';
    use Inline CPP => <<'CPPEND' FILTERS => [get_callback_filter()] ;
    
    class MyClass {
      public:
        void methodA() { ... } /* implemented here in C++ */
        void methodB(int a) { FILTER_CALLPERL } /* forward to Perl */
    }
    
    class Abstract {
      public:
        void dispatch(int n, int arg) {
          switch (n) {
            case 0: method0(arg); break;
            case 1: method1(arg); break;
            case 2: method2(arg); break;
            case 3: method3(arg); break;
            default: throw("invalid dispatch arg");
          }
        }
        virtual int    method0(int) = 0;
        virtual double method1(int) = 0;
        virtual void   method2(int) = 0;
        virtual void   method3(int) = 0;
    };
    class Derived : public Abstract {
        /* A pure-virtual implemented normally in C++ */
        virtual int method0(int arg) { return 12345; }

        /* Auto-generate implementations of all other pure virtuals */
        FILTER_IMPL_PUREVIRTS 
    };
    CPPEND
    
    my $obj = Derived->new;
    $obj->set_perl_object($obj);

    $obj->dispatch(2, 42);  

    exit;
    
    package MyClass;

    sub perl_methodB { ... }
    
    package Derived;

    sub perl_method1 { ... }
    sub perl_method2 { ... }
    sub perl_method3 { ... }
    
=head1 DESCRIPTION

This is an EXPERIMENTAL PROOF-OF-CONCEPT.  The API and name of this module may change.

L<Inline::CPP::Callbacks> is an extension for L<Inline::CPP> which lets 
you write methods of C++ classes in Perl.

This is a "filter" which parses your C++ code and
inserts proxy implementations of the methods which you want 
forwarded back to Perl.   You indicate
which methods to forward to Perl with the following "macros":

=over

=item B<FILTER_CALLPERL>

This may be used as the I<only> thing in a method body.
It expands to code which calls the corresponding method in Perl, 
passing the C++ method's parameters as scalar arguments, and uses
the return value from Perl as the return value of the C++ method.  

C++ integer and floating parameters are passed as such, 
and pointers are passed as object references (if the pointed-to 
thing is a C++ class registered by C<Inline::CPP> then your Perl 
code can call methods on it).

The Perl method called is named "perl_XXX" where XXX is the C++ method name
(the "perl_" prefix can be changed with a config option).

=item B<FILTER_IMPL_PUREVIRTS>

This may appear as the last thing in a C<class> definition.  It expands
to complete implementations of I<all> pure-virtual methods in a base class
which have not otherwise been implemented.

This is exactly equivalent to manually coding C++ implementations 
for missing pure-virtual methods, with each method body
containing C<FILTER_CALLPERL>.

=back

=head1 A BRIEF PRIMER ON C++ PURE VIRTUALS

In C++, a "virtual" method (whether pure or not) is implicitly called
through a hidden pointer to the most-derived implementation for the 
particular object 
(objects might be of different derived types, and therefore
have different implementations of a particular method).  
The purpose of virtual methods is so code which knows only the base class API
can invoke derived-class methods indirectly, i.e., invoking methods
on a pointer-to-base-class will actually call derived-class methods
(if a method is not "virtual" then doing this just calls the base class's implementation).
Virtual methods support "polymorphism" where different flavors
of something share a common API (virtual methods in a common base class)
with different implementations (methods in derived classes).

A virtual method declared with C<= 0> as the implementation
is a B<pure virtual> method and represents only an interface, with no implementation at
all (not even a default implementation).  A class containing pure virtuals
can not be instantiated directly; it can only be used as a base class for 
derived classes which *do* implement those methods.

Such a base class is sometimes called an "abstract" class.


=head1 REGISTERING THE PERL OBJECT WITH C++ OBJECT

Each C++ object which forwards method calls to Perl must know the
corresponding Perl object to use as the invocant, i.e. the Perl
object through which forwarded-to methods are to be found.  

Currently this is not automated; you must call a special C<set_perl_object> method 
to register the Perl object with the C++ object -- see example above.  

This method (and an associated data member) are automatically inserted into 
the class.

=head1 AUTHOR

Jim Avera <jim.avera@gmail.com>

The C++ parser was derived from the one in
L<Inline::CPP> by David Oswald.

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2019 - Jim Avera

Portions Copyright (c) 2011 - 2019 David Oswald.

All Rights Reserved. This module is free software, licensed under
the Artistic license, version 2.0.

See http://www.perlfoundation.org/artistic_license_2_0

=cut
