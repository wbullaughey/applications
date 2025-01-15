#!/bin/zsh
export SOURCE=/Users/wayne/Project/git/alr/applications
export DESTINATION=/Volumes/applications
export OPTIONS=-lptv
export OUTPUT=rsync.txt

function copy_directory {
   echo "rsync $OPTIONS $SOURCE/$1/src/* $DESTINATION/$1/src"
   rsync $OPTIONS $SOURCE/$1/src/* $DESTINATION/$1/src
}

function copy_file {
   echo "rsync $OPTIONS $SOURCE/$1 $DESTINATION/$1"
   rsync $OPTIONS $SOURCE/$1 $DESTINATION/$1
}

echo building 2>&1 | tee $OUTPUT

copy_directory "ada_lib" 2>&1 | tee -a $OUTPUT
copy_directory "ada_lib/ada_lib_gnoga" 2>&1 | tee -a $OUTPUT
copy_directory "ada_lib/ada_lib_tests" 2>&1 | tee -a $OUTPUT
copy_directory "aunit" 2>&1 | tee -a $OUTPUT
copy_directory "aunit/ada_lib" 2>&1 | tee -a $OUTPUT
copy_directory "." 2>&1 | tee -a $OUTPUT
copy_directory "video/camera" 2>&1 | tee -a $OUTPUT
copy_directory "video/camera/lib" 2>&1 | tee -a $OUTPUT
copy_directory "video/camera/test_lib" 2>&1 | tee -a $OUTPUT
copy_directory "video/camera/unit_test" 2>&1 | tee -a $OUTPUT
copy_directory "video/lib" 2>&1 | tee -a $OUTPUT
copy_directory "video/lib/video_aunit" 2>&1 | tee -a $OUTPUT

copy_file video/camera/unit_test/build.sh 2>&1 | tee -a $OUTPUT

