source ~/.zshrc
export BUILD_MODE=$1
echo BUILD_MODE $BUILD_MODE

../../../global_build.sh $BUILD_MODE
