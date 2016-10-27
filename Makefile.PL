use 5.020000;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'PreEq::EqData',
    VERSION_FROM      => 'lib/PreEq/EqData.pm', # finds $VERSION, requires EU::MM from perl >= 5.5
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    ABSTRACT_FROM     => 'lib/PreEq/EqData.pm', # retrieve abstract from module
    AUTHOR            => 'Mariusz Jadczak <mariuszj@toya.com.pl>',
    LICENSE           => 'bsd',
    #Value must be from legacy list of licenses here
    #http://search.cpan.org/perldoc?Module%3A%3ABuild%3A%3AAPI
);
