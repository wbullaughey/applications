#!/bin/zsh
source ~/.zshrc
export WHICH=$1
export NO_WARNINGS=$2
export TRACE=1


# WHICH values
#   all     - build everything (help_tests, driver unit tests, applications)
#   execute    - build application or library for subdirectory level
#   help_test  - builds help_test at level

export ALR_OPTIONS=-O0
echo build WHICH $WHICH ALR_OPTIONS $ALR_OPTIONS

../../../../global_build.sh $WHICH program $NO_WARNINGS $TRACE
