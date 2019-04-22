use ExtUtils::MakeMaker;
use ExtUtils::CppGuess;
use strict;

my $DIST_VERSION    = '0.001';

my $CPP_Config_path = 'lib/Inline/CPP/Config.pm';

my %PREREQ_PM = (
  'Inline'            => '0.82',
  'Inline::CPP'       => '0.79', 
  'Parse::RecDescent' => '0',
  'Carp'              => '0',
  'Config'            => '0',
);

check_prereqs(\%PREREQ_PM) or warn "!!! PREREQUISITES NOT MET !!!";

WriteMakefile(
  NAME           => 'Inline::CPP::Callbacks',
  AUTHOR         => 'Jim Avera <jim.avera@gmail.com>',
  VERSION_FROM   => 'lib/Inline/CPP/Callbacks.pm',
  ABSTRACT_FROM  => 'lib/Inline/CPP/Callbacks.pm',
  LICENSE        => 'artistic_2',
  PREREQ_PM      => \%PREREQ_PM,
  TEST_REQUIRES => {
    'Test::More' => '1.001009',    # Core. Minimum version.
    'File::Temp' => '0',           # Core.
    'FindBin'    => '0',           # Core.
  },
  CONFIGURE_REQUIRES => {
    'ExtUtils::CppGuess'  => '0.15',
    'ExtUtils::MakeMaker' => '7.04',    # Core.
  },
  META_MERGE => {
    'meta-spec' => {
      version => 2,
      url     => 'http://search.cpan.org/perldoc?CPAN::Meta::Spec',
    },
    no_index   => { directory => [ 'inc' ], },
    resources   => {
      license    => 'http://dev.perl.org/licenses/artistic.html',
      bugtracker => {web=>'http://github.com/jimav/Inline-CPP-Callbacks/issues'},
      homepage   => 'http://github.com/jimav/Inline-CPP-Callbacks',
      repository => {
        type => 'git',
        url  => 'http://github.com/jimav/Inline-CPP-Callbacks.git',
        web  => 'http://github.com/jimav/Inline-CPP-Callbacks',
      },
    },
    provides   => {
      'Inline::CPP::Callbacks' => {
        file    => 'lib/Inline/CPP/Callbacks.pm',
        version => $DIST_VERSION,
      },
    },
    prereqs => {
      develop => {
        requires => {
          'Test::Pod' => '1.00',
          'Test::Perl::Critic' => 0,
          'Test::Pod::Coverage' => '1.00',
          'Test::CPAN::Changes' => 0,
          'English' => '0',
        },
      },
    },
  },
  MIN_PERL_VERSION => '5.008001',
  test  => {RECURSIVE_TEST_FILES => 1},
  clean => {FILES => join q{ }, qw{
    _Inline/            t/_Inline
    Inline-CPP-Callback*/
  }}, # Paths can be space delimited.
);

#============================================================================
# We'll do our own prerequisite checking, since MakeMaker does it
# in a way that always fails: 'use Inline::C 0.33' will never work.
#============================================================================

sub check_prereqs {
  my $prereq_pm_href = shift;
  my $prereqs_ok = 1;

  for(sort keys %$prereq_pm_href) {
    ## no critic (eval);
    eval "require $_";

    # We eval version numbers to normalize _xxx dev numbering.
    my $have = eval 'no strict q/refs/; ${"${_}::VERSION"}';

    use strict q/refs/;

    my $want = eval $prereq_pm_href->{$_};

    if($@ || $have < $want) {
      warn "Warning: prerequisite $_ version $prereq_pm_href->{$_} not found.";
      $prereqs_ok = 0;
    }
  }
  return $prereqs_ok;
}


sub to_file {
  my ($file, @data) = @_;
  open my $fh, '>', $file
    or die "Makefile.PL: Couldn't open $file for output: $!\n";
  print $fh @data or die "Makefile.PL: Couldn't write $file: $!\n";
  close $fh or die "Makefile.PL: Couldn't close $file: $!\n";
}

sub from_file {
  my ($file) = @_;
  local $/;
  open my $fh, '<', $file or die "Makefile.PL: Can't read from $file: $!";
  <$fh>;
}

