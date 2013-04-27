#! /bin/bash

tmpDir="$HOME/Microcebus"
cloneCmd="git clone git@github.com:ruanhao/Microcebus.git $tmpDir"
[[ -d "$tmpDir"  ]] && echo "rm -rf ${tmpDir} ..."; \
                       rm -rf $tmpDir; ( exec $cloneCmd ) || ( exec $cloneCmd ) 
cd $tmpDir
echo $( pwd )
