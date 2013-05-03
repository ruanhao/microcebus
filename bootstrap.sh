#! /bin/bash

echo "WELCOME TO USE MICROCEBUS !!!"

VIMDIR="$HOME/.vim"
VIMRC="$HOME/.vimrc"
REPOCFG=$VIMDIR/repo.config

for i in $VIMDIR $VIMRC; do
    orig="${i}.orig"
    if [[ ( -e "$orig" ) || ( -h "$orig" ) ]]; then
        echo "removing $orig"
        rm -rf $orig
    fi
    
    if [[ -h "$i" ]]; then 
        if [[ -e "$( readlink $i )" ]]; then 
            echo "renaming $i ---> $orig"
            mv $i $orig
        else 
            echo "removing $i"
            rm -f $i
        fi
    else 
        echo "renaming $i ---> $orig"
        mv $i $orig
    fi
done

echo "Initializing MICROCEBUS..."
git clone  git@github.com:ruanhao/microcebus.git $VIMDIR 2> /dev/null
ln -s $VIMDIR/vimrc $VIMRC

echo "updating pathogen file..." 
TMPDIR=$( mktemp -d )
git clone git@github.com:tpope/vim-pathogen.git $TMPDIR > /dev/null 2>&1
cp -r $TMPDIR/autoload $VIMDIR

echo "updating VIM Scripts..."
mkdir -p $VIMDIR/bundle
cd $VIMDIR/bundle
TOTALREPO=$( grep -vE '(^#|^$)' $REPOCFG | wc -l )
cnt=1
while read repo; do
    echo -n "[$cnt/$TOTALREPO] "
    let "cnt++"
    [[ ( ${repo:0:1} != "#" ) && ( ${#repo} -ne 0 ) ]] && git clone $repo 2> /dev/null
done <$REPOCFG

echo "dos2unix ... "
find $VIMDIR/bundle \( -name ".git" -prune \) -o \( -type f -print \) | xargs dos2unix > /dev/null 2>&1 \
&& echo "dos2unix complete" || die "can't perform dos2unix"

cd $VIMDIR
rm -f *.sh *.md *.config

echo "It rocks, pls enjoy :)"
