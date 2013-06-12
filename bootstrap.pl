#! /usr/bin/perl -w
use Cwd 'abs_path';
#
## File: bootstrap.pl
## Author: Hao Ruan
## Purpose: Linux environment setup
## Created: 2013.06.12


my $HOME   = $ENV{HOME};
my $VIMDIR = "$HOME/.vim";
my $VIMRC  = "$HOME/.vimrc";
my $REPO   = "https://github.com/ruanhao/microcebus.git";

print_logo();
remove_orig($HOME);
backup_file($HOME);
git_clone($VIMDIR, $VIMRC);
initialize_repo($VIMDIR);
finalization();




sub finalization {
    print "microcebus installation done \n";
}

sub initialize_repo {
    my ($VIMDIR) = @_;
    qx{ mkdir $VIMDIR/bundle };
    my $total = `cat $VIMDIR/repo.config | egrep '^[[:space:]]*[^#]' | wc -l`;
    my $i     = 1;
    chomp($total);
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
    print "microcebus\n";
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


