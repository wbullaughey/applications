#!/bin/zsh
export OUTPUT=list-camera_driver.txt
export PROGRAM=bin/camera_aunit
export DO_TRACE=1

source ../../../../routines.sh
export TARGET_SUBDIRECTORY=`realpath "$CURRENT_DIRECTORY/../unit_test"`
export OPTIONS="-=D $TARGET_SUBDIRECTORY"

output TRACE OPTIONS $OPTIONS CURRENT_DIRECTORY $CURRENT_DIRECTORY \
   TARGET_SUBDIRECTORY $TARGET_SUBDIRECTORY

case $PARAMETERS[1] in

   "help")
      echo "driver_test [<options>] <mode> <test>"
      echo "   <options> := -<driver option> | @<camera option>"
      echo "   <mode> := help | camera | test"
      echo "   <test> := [<suite> [<routine>]]"
      echo ""
      echo " mode"
      echo "   help  : displays driver help"
      echo "   camera : runs the camera driver"
      echo "   test   : runs the camera unit tests"
      echo ""
      echo " test"
      echo "   no suite or routien runs all"
      echo "   suite only runs all routines for suite"
      echo "   suite and routine runs just one routine"
      echo ""
      ../driver/bin/camera_driver -h
      exit
      ;;

   *)
      ;;

esac

export MODE="${PARAMETERS[1]}"
output TRACE MODE $MODE
remove_1
parameters  # process leading options
case "$MODE" in

   "help")
      bin/camera_driver -h | tee $OUTPUT
      exit
      ;;

   "camera")
      export APPLICATION=../bin/camera_driver
      ;;

   "test")
      export APPLICATION=../unit_test/bin/camera_aunit
      ;;

   "")
      output TRACE missing mode
      echo missing mode | tee -a $OUTPUT
      exit
      ;;

   *)
      output TRACE invalid mode $MODE
      exit
      ;;

esac
output TRACE APPLICATION $APPLICATION

export SUITE="${PARAMETERS[1]}"
output TRACE "suite $SUITE"
remove_1
parameters  # process leading options
output TRACE "suite: $SUITE"
case "$SUITE" in

   "")
      echo no suites specified, run all
      ;;

   "all")
      echo "test all suites"
      ;;

   *)
      export OPTIONS="$OPTIONS -u $SUITE"
      ;;

esac

export ROUTINE="${PARAMETERS[1]}"
output TRACE "routine $ROUTINE"

remove_1
parameters  # process leading options

export VERBOSE="-v -=X -v"
case "$ROUTINE" in

   all)
      ;;

   "")
      echo no routine specified, run all
      ;;

   *)
      export OPTIONS="$OPTIONS -R $ROUTINE"
       ;;

esac

parameters  # process leading options

output TRACE final OPTIONS $OPTIONS
export COMMAND="../driver/bin/camera_driver $OPTIONS $VERBOSE"
output TRACE "comand: $COMMAND" 2>&1
rm GNAT-*
echo run command
eval $COMMAND 2>&1 | tee -a $OUTPUT



