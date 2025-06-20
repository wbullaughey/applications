source ~/.zshrc
export WHICH=$1
#export ADAFLAGS=-gnatv
#export DEBUG_OPTIONS="-vv -d"

echo WHICH $WHICH BUILD_PROFILE $ADA_APPLICATION_PROFILE \
   ADA_OS_INCLUDE $ADA_OS_INCLUDE

../../../../global_build.sh $WHICH $ADA_APPLICATION_PROFILE program
