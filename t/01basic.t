use strict; use warnings;
use Test::More;

use Inline Config => force_build => 1, clean_after_build => 0;

use Inline::CPP::Callbacks 'get_callback_filter';
use Inline CPP => <<'CPPEND' => FILTERS => [get_callback_filter()] ;

 class Abstract {
   public:
     int dispatch(int n, int arg) {
       switch (n) {
         case 0: return pure0(arg);
         case 1: return pure1(arg);
         case 2: return pure2(arg);
         case 3: return pure3(arg);
         case 4: return pure4(arg);
         case 5: return nonpure5(arg);
         case 6: return nonpure6(arg);
         case 7: return nonpure7unimp(arg);
         case 8: return pure8unimp(arg);
         default: 
           cerr << "dispatch called with invalid n=" << n << endl;
           exit(1);
       }
     }
     virtual int pure0(int) = 0;
  protected:
     virtual int pure1(int) = 0;
  public: 
     virtual int pure2(int) = 0;
  protected:
     virtual int pure3(int) = 0;
  public: 
     virtual int pure4(int) = 0;
     virtual int nonpure5(int arg) { return -1; }
     virtual int nonpure6(int arg) { return 6000+arg; }

     virtual int nonpure7unimp(int arg) { return -1; }
     virtual int pure8unimp(int) = 0;
 };

 class Derived : public Abstract {
  public:     
     // Not a pure virtual, so FILTER_IMPL_PUREVIRTS would not
     // generate anything for this method
     virtual int nonpure5 (int arg) { FILTER_CALLPERL }

     /* Note: nonpure6 is not overridden */

     virtual int nonpure7unimp (int arg) { FILTER_CALLPERL }
     virtual int pure8unimp (int arg) { FILTER_CALLPERL }

     virtual int pure0(int arg) {
       // A pure virtual, but implemented explicitly and so
       // FILTER_IMPL_PUREVIRTS should not generate another implementation.
       return arg; 
     }

     virtual int pure2(int arg) {
       // Another pure virtual implemented explictly, but this 
       // one forwards to Perl.
       // Note--we only allow comments before FILTER_CALLPERL, no real code!
       FILTER_CALLPERL
     }

     // Generate all otherwise-unimplemented pure virtuals
     FILTER_IMPL_PUREVIRTS
 };

CPPEND

### ??? Is there a Perl analogue of an "Abstract *" pointer?
my $obj = Derived->new();
$obj->set_perl_object($obj);

is(
  $obj->pure0(999), 999,
  "Directly call pure C++ impl of abstract method"
);

ok( ! defined(eval{ $obj->pure1(42) }), 
    "Protected method not callable"
);

is(
  $obj->pure2(43), 2043,
  "Directly call mixed C++/Perl impl of abstract method"
);

for (my $n=0; $n <= 6; ++$n) {
  my $expected = ($n*1000) + (42+$n);
  my $result = $obj->dispatch($n, 42+$n);
  ok($expected == $result, "Indirect call via base-class method (n=$n)");
}

ok( !defined(do{
                  my $r = eval{ $obj->dispatch(7, 999); };
                  print "CAUGHT: $@\n" if $@;
                  $r
               }), "Missing perl call-back method trapped (non-pure)"
);

ok( !defined(do{
                  my $r = eval{ $obj->dispatch(8, 999); };
                  print "CAUGHT: $@\n" if $@;
                  $r
               }), "Missing perl call-back method trapped (pure v.)"
);

done_testing();

exit 0;

package Derived;
use Carp;

sub check_arg {
  my ($self, @args) = @_;
  confess "self is wrong: $self" unless ref($self) eq __PACKAGE__;
  confess "Wrong arg count" unless @args == 1;
  return $args[0];
}

sub perl_pure0 { die "Should never be called" }
sub perl_pure1 { return 1000 + &check_arg }
sub perl_pure2 { return 2000 + &check_arg }
sub perl_pure3 { return 3000 + &check_arg }
sub perl_pure4 { return 4000 + &check_arg }
sub perl_nonpure5 { return 5000 + &check_arg }
sub perl_nonpure6 { return 6000 + &check_arg }

1;
