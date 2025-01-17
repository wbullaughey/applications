#!/bin/zsh
source ~/.zshrc
echo building on $OS_VERSION
pwd
alr build -- -j10 -s -k -gnatE 2>&1 | tee build.txt

