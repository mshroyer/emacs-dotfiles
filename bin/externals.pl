#!/usr/bin/perl

# Checkout or update remote dependencies.  This script uses relative paths, so
# only run this from directly within the .emacs.d directory!

use warnings;
use strict;

use File::Basename;
use Cwd;

our @externals = (
    {
        path   => 'elisp/clojure-mode',
        vcs    => 'git',
        repo   => 'git://github.com/jochu/clojure-mode.git',
        branch => 'master',
    },
    {
        path   => 'elisp/swank-clojure',
        vcs    => 'git',
        repo   => 'git://github.com/jochu/swank-clojure.git',
        branch => 'master',
    },
    {
        path   => 'elisp/haskellmode-emacs',
        vcs    => 'darcs',
        repo   => 'http://code.haskell.org/haskellmode-emacs',
    },
    {
        path   => 'elisp/org-mode',
        vcs    => 'git',
        repo   => 'git://repo.or.cz/org-mode.git',
        branch => 'maint',
    },
    {
        path   => 'elisp/slime',
        vcs    => 'cvs',
        repo   => ':pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot slime',
        branch => 'trunk',
    },
    {
        path   => 'elisp/cperl-mode',
        vcs    => 'git',
        repo   => 'git://github.com/jrockway/cperl-mode.git',
        branch => 'perl6-pugs',
    },
    {
        path   => 'elisp/emacs_chrome',
        vcs    => 'git',
        repo   => 'git://github.com/stsquad/emacs_chrome.git',
        branch => 'master',
    },
);

my @dirstack = ();

sub pushd {
    my ($newpath) = @_;
    push @dirstack, getcwd;
    chdir $newpath;
}

sub popd {
    my $oldpath = pop @dirstack;
    chdir $oldpath if ( $oldpath );
}

sub vcs_cvs_checkout {
    my ($path, $repo, $branch) = @_;

    my @repo_words = split(/ /, $repo);
    my $cvsroot = $repo_words[0];
    my $module = $repo_words[1];

    my $branchopt = ( $branch eq 'trunk' ) ? "" : "-r ${branch}";

    my $ppath = dirname($path);
    mkdir $ppath unless ( -d $ppath );
    pushd($ppath);
    `cvs -d ${cvsroot} checkout -P ${branchopt} ${module}`;
    popd();
}

sub vcs_cvs_update {
    my ($path, $repo, $branch) = @_;

    my @repo_words = split(/ /, $repo);
    my $cvsroot = $repo_words[0];
    my $module = $repo_words[1];

    pushd($path);
    `cvs update -Pd`;
    popd();
}

sub vcs_darcs_checkout {
    my ($path, $repo, $branch) = @_;

    `darcs get "${repo}" "${path}"`;
}

sub vcs_darcs_update {
    my ($path, $repo, $branch) = @_;

    pushd($path);
    `darcs pull`;
    popd();
}

sub vcs_git_checkout {
    my ($path, $repo, $branch) = @_;

    my $branchopt = $branch ? "-b ${branch}" : "";

    `git clone ${branchopt} "${repo}" "${path}"`;
}

sub vcs_git_update {
    my ($path, $repo, $branch) = @_;

    pushd($path);
    `git pull origin ${branch}`;
    popd();
}

sub get_vcs_cmd {
    my ($vcs, $cmd) = @_;

    my $fname = "vcs_${vcs}_${cmd}";
    if ( defined &$fname ) {
        return \&$fname;
    }
    else {
        return 0;
    }
}

if ( $^O eq 'MSWin32' ) {
    print <<"EOF";

WARNING: Checking out from remote CVS repositories using CVSNT can cause
things to blow up.  In particular, CVSNT mangles the line endings in
checkouts from the Slime repository, resulting in errors in the Emacs
startup script.

If possible, run this script from within Cygwin instead.
EOF
}

my $command = shift @ARGV || '';
if ( $command eq 'checkout' || $command eq 'co' ) {
    print <<"EOF";

### Optional external Emacs resources ###

Externals marked with [i] are alread installed.  Enter an external's number to
mark it for installation, then type x to checkout marked externals or q to
quit without applying any changes.
EOF
    do {
        # Show externals menu
        print "\n";
        for ( my $i = 0; $i <= $#externals; $i++ ) {
            my $ext = $externals[$i];
            my $flag = ( -d $ext->{path} ) ? 'i' :
                    ( exists $ext->{install} ? '+' : ' ' );
            printf "%2d. [%s] %s (%s)\n", $i+1, $flag, $ext->{path},
                    ( exists $ext->{branch} ? $ext->{vcs} . ' ' . $ext->{branch}
                              : $ext->{vcs} );
        }

        # Prompt for input
        print "\next> ";
        my $cmd = <STDIN>;
        chomp $cmd;

        # Process input
        if ( lc($cmd) eq 'q' ) {
            # Quit
            exit 0;
        } elsif ( lc($cmd) eq 'x' ) {
            # Install marked externals
            for my $ext ( @externals ) {
                if ( exists $ext->{install} ) {
                    print "\n### Checking out " . $ext->{path} . " ###\n";
                    my $fun = get_vcs_cmd($ext->{vcs}, 'checkout');
                    if ( $fun ) {
                        &$fun($ext->{path}, $ext->{repo}, $ext->{branch});
                    } else {
                        print "Error: No checkout function for "
                                . $ext->{vcs} . ".\n";
                    }
                }
            }
            exit 0;
        } elsif ( $cmd =~ m/^[1-9]\d*$/o && $cmd <= $#externals + 1 ) {
            # Toggle mark on non-installed external
            my $ext = $externals[$cmd-1];
            if ( -d $ext->{path} ) {
                print "\nThe external " . $ext->{path}
                        . " is already installed.\n";
            } elsif ( exists $ext->{install} ) {
                delete $ext->{install};
            } else {
                $ext->{install} = 1;
            }
        } else {
            print "\nEnter a number between 1 and "
                    . ( $#externals + 1 ) . ", x, or q.\n";
        }
    } while ( 1 );
}
elsif ( $command eq 'update' || $command eq 'up' ) {
    for my $ext ( @externals ) {
        if ( -d $ext->{path} ) {
            print "\n### Updating " . $ext->{path} . " ###\n";
            my $fun = get_vcs_cmd($ext->{vcs}, 'update');
            if ( $fun ) {
                &$fun($ext->{path}, $ext->{repo}, $ext->{branch});
            }
            else {
                print "Error: No update function for " . $ext->{vcs} . ".\n";
            }
        }
    }
}
else {
    print "\nUsage: `$0 checkout` or `$0 update`.\n";
    exit 1;
}
