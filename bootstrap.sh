#! /bin/bash

origVimDir="$HOME/.vim.orig"
vimDir="$HOME/.vim"
[[ -d "$origVimDir" ]] && echo "removing $origVimDir"; \
                          rm -rf $origVimDir

if [[ -d "$vimDir" ]]; then
    echo "renaming $vimDir ---> $origVimDir"
    mv $vimDir $origVimDir
    git clone  git@github.com:ruanhao/Microcebus.git $vimDir
else
    echo "creating $vimDir"
    mkdir -p $vimDir
    git clone  git@github.com:ruanhao/Microcebus.git $vimDir
fi

echo "updating pathogen file" 
tmpDir=$( mktemp -d )
git clone git@github.com:tpope/vim-pathogen.git $tmpDir > /dev/null
cp -r $tmpDir/autoload $vimDir

echo "updating VIM Scripts"
mkdir -p $vimDir/bundle
cd $vimDir/bundle
while read repo; do
    git clone $repo
done <$vimDir/repo.config




    

