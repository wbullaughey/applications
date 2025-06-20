#!/bin/zsh
export OUTPUT=list-camera_driver.txt
export CURRENT_DIRECTORY=`pwd`
export TARGET_SUBDIRECTORY=`realpath "$CURRENT_DIRECTORY/../unit_test"`
export OPTIONS="-r -=D $TARGET_SUBDIRECTORY"
export PARAMETERS=("$@")
echo PARAMETERS: $PARAMETERS
export PROGRAM=bin/camera_aunit
echo OUPUT=$OUTPUT
echo test with driver 2>&1 | tee $OUTPUT
export DO_TRACE=1

function output() {
echo output parameters $* 2>&1 TRACE.txt
   TRACE=$1
   shift 1
   echo "output TRACE $TRACE DO_TRACE $DO_TRACE APPEND TRACE $APPEND_OUTPUT" \
         2>&1 | tee -a TRACE.txt
   case $TRACE in

      "LIST")
         ;;

      "TRACE")
         if [ "$DO_TRACE" -eq 0 ]; then
            return
         fi
   esac
   echo $* 2>&1 | tee $APPEND_OUTPUT $OUTPUT
   export APPEND_OUTPUT=-a  # append from now on
}

function run() {
   output TRACE run PROGRAM $PROGRAM OPTIONS $OPTIONS COMMAND $COMMAND

   export EXECUTE="$PROGRAM $OPTIONS $COMMAND"
   output TRACE EXECUTE $EXECUTE
   ${=EXECUTE} 2>&1 | tee $APPEND_OUTPUT $OUTPUT
   exit;
}

output TRACE camera_tests PARAMETERS $PARAMETERS

for PARAMETER in $*; do    # put all '-' options into OPTIONS
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     output TRACE option: $PARAMETER
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

output TRACE first parameter $1


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
trace MODE $MODE
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
      trace missing mode
      echo missing mode | tee -a $OUTPUT
      exit
      ;;

   *)
      trace invalid mode $MODE
      exit
      ;;

esac
trace APPLICATION $APPLICATION

export SUITE="${PARAMETERS[1]}"
echo "suite $SUITE"
remove_1
parameters  # process leading options
trace "suite: $SUITE"
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
trace "routine $ROUTINE"

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

trace final OPTIONS $OPTIONS
export COMMAND="../driver/bin/camera_driver $OPTIONS $VERBOSE"
trace "comand: $COMMAND" 2>&1
rm GNAT-*
echo run command
eval $COMMAND 2>&1 | tee -a $OUTPUT



