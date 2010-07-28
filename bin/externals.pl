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
    }
);

sub vcs_git_clone {
    my ($path, $repo, $branch) = @_;

    my $oldcwd = getcwd;
    `git clone "${repo}" "${path}"`;
    chdir $path;
    `git checkout ${branch}`;
    chdir $oldcwd;
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
        $fun = get_vcs_cmd($ext->{vcs}, 'update');
    }
    else {
        $fun = get_vcs_cmd($ext->{vcs}, 'clone');
    }

    unless ( $fun ) {
        print "No supporting VCS function for " . $ext->{path} . "\n";
        exit 1;
    }
    &$fun($ext->{path}, $ext->{repo}, $ext->{branch});
}
