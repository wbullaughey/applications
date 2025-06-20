source ~/.zshrc
export WHICH=$1
export DIRECTORY=`pwd`
echo echo $0 WHICH $BUILD_MODE \
   BUILD_PROFILE $ADA_APPLICATION_PROFILE \
   ADA_OS_INCLUDE $ADA_OS_INCLUDE \
   DIRECTORY $DIRECTORY

../../../global_build.sh $BUILD_MODE camera_control $ADA_APPLICATION_PROFILE program
