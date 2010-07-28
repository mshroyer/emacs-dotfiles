#!/usr/bin/perl

# Clone or update remote dependencies.  This script uses relative paths, so
# only run this from directly within the .emacs.d directory!

use warnings;
use strict;

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
        path   => 'elisp/org-mode',
        vcs    => 'git',
        repo   => 'git://repo.or.cz/org-mode.git',
        branch => 'maint',
    },
    {
        path   => 'elisp/slime',
        vcs    => 'cvs',
        repo   => ':pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot',
        branch => 'slime',
    },
);

sub vcs_cvs_clone {
    my ($path, $repo, $branch) = @_;

    `cvs -d ${repo} checkout -P -d ${path} ${branch}`;
}

sub vcs_cvs_update {
    my ($path, $repo, $branch) = @_;

    my $oldcwd = getcwd;
    chdir $path;
    `cvs -d ${repo} update -Pd`;
    chdir $oldcwd;
}

sub vcs_git_clone {
    my ($path, $repo, $branch) = @_;

    `git clone "${repo}" "${path}"`;
    if ( $branch ne 'master' ) {
        my $oldcwd = getcwd;
        chdir $path;
        `git branch --track ${branch} origin/${branch}`;
        `git checkout ${branch}`;
        chdir $oldcwd;
    }
}

sub vcs_git_update {
    my ($path, $repo, $branch) = @_;

    my $oldcwd = getcwd;
    chdir $path;
    `git pull origin ${branch}`;
    chdir $oldcwd;
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
};

for my $ext ( @externals ) {
    my $fun = undef;

    if ( -d $ext->{path} ) {
        print "\nUpdating " . $ext->{path} . "\n";
        $fun = get_vcs_cmd($ext->{vcs}, 'update');
    }
    else {
        print "\nCloning " . $ext->{path} . "\n";
        $fun = get_vcs_cmd($ext->{vcs}, 'clone');
    }

    if ( $fun ) {
        &$fun($ext->{path}, $ext->{repo}, $ext->{branch});
    }
    else {
        print "No supporting VCS function for " . $ext->{path} . "\n";
    }
}
