source ~/.zshrc
export CWD=`pwd`
export OUTPUT=$CWD/build_all.txt
echo output $OUTPUT 2>&1 | tee $OUTPUT
pushd ada_lib/ada_lib_tests
pwd 2>&1 | tee -a $OUTPUT
./build.sh 2>&1 | tee -a $OUTPUT
popd
pushd video/camera
pwd 2>&1 | tee -a $OUTPUT
./build.sh 2>&1 | tee -a $OUTPUT
pushd unit_test
pwd 2>&1 | tee -a $OUTPUT
./build.sh 2>&1 | tee -a $OUTPUT
popd
#pushd driver
#pwd 2>&1 | tee -a $OUTPUT
#./build.sh 2>&1 | tee -a $OUTPUT



