#! /usr/bin/perl -w
use Cwd 'abs_path';
#
## File: bootstrap.pl
## Author: Hao Ruan
## Purpose: Linux environment setup
## Created: 2013.06.12


my $HOME        = $ENV{HOME};
my $VIMDIR      = "$HOME/.vim";
my $VIMRC       = "$HOME/.vimrc";
my $REPO        = "https://github.com/ruanhao/microcebus.git";
my $GITCONFIG   = "$HOME/.vim/gitshorts";
my $SHELLCONFIG = "$HOME/.vim/shellconfig";

print_logo();
remove_orig($HOME);
backup_file($HOME);
git_clone($VIMDIR, $VIMRC);
initialize_repo($VIMDIR);
initialize_gitshorts($GITCONFIG);
initialize_shellconfig($SHELLCONFIG);
finalization();

sub initialize_shellconfig {
    my ($SHELLCONFIG) = @_;
    my $shell         = $ENV{SHELL};
    my $rcfile        = '';
    {
        $shell =~ /bash/ && do { 
            $rcfile = "$HOME/.bashrc";
            last
        };
        $shell =~ /zsh/  && do {
            $rcfile = "$HOME/.zshrc";
            last
        };
        last;
    }
    if ( $rcfile ) {
        print "initializing shell configuration "; 
        qx/ grep -q 'microcebus' $rcfile /;
        qx/ cat $SHELLCONFIG >> $rcfile / if $?;
        (not $?) ? do { print "--- ok \n" } : do { print "--- fail \n" };
    } else {
        print "only support bash and zsh now \n";
    }
}

sub initialize_gitshorts {
    my ($GITCONFIG) = @_;
    print "initializing git shorts "; 
    qx{ grep -q 'microcebus' $HOME/.gitconfig };
    qx{ cat $GITCONFIG >> $HOME/.gitconfig } if $?;
    (not $?) ? do { print "--- ok \n" } : do { print "--- fail \n" };
}

sub finalization {
    print "clean up $HOME/.vim \n";
    my $cleanupdir = "$HOME/.vim";
    my @cleanupds  = glob "$cleanupdir/.git $cleanupdir/bundle/*/.git";
    my @cleanupfs  = glob "$cleanupdir/shellconfig $cleanupdir/gitshorts $cleanupdir/*.md 
                           $cleanupdir/*.pl $cleanupdir/repo.config";
    for ( @cleanupds ) {
        qx/ rm -rf $_ /;
    }
    for ( @cleanupfs ) {
        qx/ rm -f $_ /;
    }
    print "microcebus installation done \n";
}

sub initialize_repo {
    my ($VIMDIR) = @_;
    qx{ mkdir $VIMDIR/bundle };
    ( -e "$VIMDIR/repo.config" ) ? my $total = `cat $VIMDIR/repo.config | egrep '^[[:space:]]*[^#]' | wc -l` : 
                                   do { print "$VIMDIR/repo.config not found, installation aborted \n"; exit 10 };
    $total    =~ s/^\s+|\s+$//g;
    my $i     = 1;
    open REPO, "$VIMDIR/repo.config";
    while ( <REPO> ) {
        chomp($_);
        if (/^\s*(https|git)[\@:](\/\/)?github\.com.*\/(.*)\.git/) {
           print "[ $i / $total ] initializing $3 "; 
           qx{ git clone $& $VIMDIR/bundle/$3 2> /dev/null };
           $? == 0 ? do { $i++; print "--- ok \n" } : do { print "--- fail \n" };
        }
    }
    close REPO;
}

sub git_clone {
    my ($VIMDIR, $VIMRC) = @_;
    if ( abs_path($0) =~ /(.*\/microcebus)\// ) {
        ## local mode   
        print "copying $1 to $VIMDIR ";
        qx/ cp -rf $1 $VIMDIR /;
        $? == 0 ? print "--- ok \n" : do { print "--- fail \n"; die "$!" };
    } else {
        ## remote mode
        print "cloning microcebus ";
        qx{ git clone git\@github.com:ruanhao/microcebus.git $VIMDIR 2> /dev/null };
        $? == 0 ? print "--- ok \n" : do { print "--- fail \n"; die "$!" };
    }
        qx{ ln -s $VIMDIR/vimrc $VIMRC };
}

sub print_logo {
    print "\n";
    print " =======================================================================================================\n";
    print "     _/       _/  _/_/_/   _/_/_/  _/_/_/_/   _/_/_/_/   _/_/_/   _/_/_/_/  _/_/_/_/  _/    _/    _/_/_/ \n";
    print "    _/_/   _/_/    _/   _/        _/    _/   _/    _/  _/        _/        _/    _/  _/    _/  _/        \n";
    print "   _/ _/ _/ _/    _/   _/        _/_/_/_/   _/    _/  _/        _/_/_/_/  _/_/_/_/  _/    _/    _/_/     \n";
    print "  _/   _/  _/    _/   _/        _/   _/    _/    _/  _/        _/        _/    _/  _/    _/        _/    \n";
    print " _/       _/  _/_/_/   _/_/_/  _/     _/  _/_/_/_/    _/_/_/  _/_/_/_/  _/_/_/_/  _/_/_/_/  _/_/_/       \n";
    print " =======================================================================================================\n";
    print "\n";
}

sub remove_orig {
    my ($HOME)  = @_;
    my @backups = `ls -a $HOME | egrep '\.vim(rc)?\.orig\$'`;
    for ( @backups ) {
        chomp($_);
        print "remove $HOME/$_ \n";
        qx{ rm -rf $HOME/$_ };
    }
}

sub backup_file {
    my ($HOME)  = @_;
    my @backups = `ls -a $HOME | egrep '\.vim(rc)?\$'`;
    for ( @backups ) {
        chomp($_);
        unless ( -l "$HOME/$_" ) {
            print "backup $HOME/$_ to $HOME/$_.orig \n";
            qx{ mv $HOME/$_ $HOME/$_.orig };
        } else { ## symbolic link is of no use
            print "remove $HOME/$_ \n";
            qx { rm -rf $HOME/$_ };
        }
    }
}


