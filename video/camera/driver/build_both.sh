#!/bin/zsh
./build.sh
if [[ $? -eq 0 ]]; then
   cd unit_test
   ./build.sh
else
    echo "build driver failed"
fi
