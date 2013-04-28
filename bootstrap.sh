#! /bin/bash

echo "WELCOME TO USE MICROCEBUS !!!"

VIMDIR="$HOME/.vim"
VIMRC="$HOME/.vimrc"

for i in $VIMDIR $VIMRC; do
    orig="${i}.orig"
    [[ ( -e $orig ) || ( -h $orig ) ]] && echo "removing $orig"; rm -rf $orig
    [[ ( -e $i ) || ( -h $i ) ]] && echo "renaming $i ---> $orig"; mv $i $orig
done

git clone  git@github.com:ruanhao/Microcebus.git $VIMDIR
ln -s $VIMDIR/vimrc $VIMRC

echo "updating pathogen file" 
TMPDIR=$( mktemp -d )
git clone git@github.com:tpope/vim-pathogen.git $TMPDIR
cp -r $TMPDIR/autoload $VIMDIR

echo "updating VIM Scripts"
mkdir -p $VIMDIR/bundle
cd $VIMDIR/bundle
while read repo; do
    [[ "${repo:0:1}" != "#" ]] && git clone $repo
done <$VIMDIR/repo.config

echo "It rocks, pls enjoy :)"

