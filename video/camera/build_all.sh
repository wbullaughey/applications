source ~/.zshrc
alr build -- -v -s -k -gnatE --create-map-file
pushd unit_test
alr build -- -gnatE --create-map-file
popd
cd driver
alr build -- -gnatE --create-map-file
cd unit_test
alr build -- -gnatE --create-map-file
