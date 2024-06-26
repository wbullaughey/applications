#!/bin/zsh
export OUTPUT=list-driver.txt
export CURRENT_DIRECTORY=`pwd`
export TARGET_SUBDIRECTORY=`realpath "$CURRENT_DIRECTORY/../unit_test"`
export OPTIONS="-D $TARGET_SUBDIRECTORY"
export PARAMETERS=$*
#echo PARAMETERS: $PARAMETERS
echo driver 2>&1 | tee $OUTPUT

for PARAMETER in $*; do
#  echo option $PARAMETER
   export FIRST=${PARAMETER:0:1}
#  echo FIRST $FIRST
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

export MODE=$1

echo MODE $MODE  | tee -a $OUTPUT
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
      echo missing mode
      exit
      ;;

esac
shift

for PARAMETER in $*; do
#  echo option $PARAMETER
   export FIRST=${PARAMETER:0:1}
#  echo FIRST $FIRST
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

export SUITE=$1
shift 1
export ROUTINE=$1   # all or test routine name
shift 1

echo "suite: $SUITE"  | tee -a $OUTPUT
case "$SUITE" in

   "")
      echo no suites specified, run all  | tee -a $OUTPUT
      ;;

   *)
      export OPTIONS="$OPTIONS -u $SUITE"
      ;;

esac

for PARAMETER in $*; do
#  echo option $PARAMETER
   export FIRST=${PARAMETER:0:1}
#  echo FIRST $FIRST
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

export VERBOSE="-v -X -v"
case "$ROUTINE" in

   all)
      ;;

   "")
      echo no routine specified, run all  | tee -a $OUTPUT
      ;;

   *)
      export OPTIONS="$OPTIONS -r $ROUTINE"
       ;;

esac

for PARAMETER in $*; do
#  echo option $PARAMETER
   export FIRST=${PARAMETER:0:1}
#  echo FIRST $FIRST
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

echo OPTIONS $OPTIONS | tee -a $OUTPUT
export COMMAND="./bin/camera_driver $OPTIONS $* $VERBOSE"
echo comand: $COMMAND | tee -a $OUTPUT
#pwd
rm GNAT-*
eval $COMMAND 2>&1 | tee -a $OUTPUT

