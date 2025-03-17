#!/bin/bash
export BIN_DIRECTORY=bin  # ../../../camera/unit_test/bin
export APPLICATION=./driver_unit_test
export CURRENT_DIRECTORY=`pwd`
export OUTPUT="$CURRENT_DIRECTORY/list-driver_unit.txt"
echo OUTPUT = $OUTPUT
echo command line $* | tee $OUTPUT

echo BIN_DIRECTORY = $BIN_DIRECTORY
echo APPLICATION = $APPLICATION
#echo bin directory
#ls $BIN_DIRECTORY

cd $BIN_DIRECTORY
echo pwd
pwd

for (( ;; ))
do
   if [[ "$1" == -x ]]; then
      export CAMERA_OPTIONS=$2
      shift 2
      echo "CAMERA_OPTIONS = -x $CAMERA_OPTIONS" | tee -a $OUTPUT
   elif [[ "$1" == -* ]]; then
     export OPTIONS="$OPTIONS $1"
     echo "options = $OPTIONS"
     shift 1
   else
      break
   fi
done

export SUITE=$1
shift 1

echo suite $SUITE rest: \"$1\" | tee -a $OUTPUT

case "$SUITE" in

   "help")
      export COMMAND="$APPLICATION $OPTIONS -h "
      echo "command: $COMMAND"  | tee $OUTPUT
      $COMMAND 2>&1 | tee -a $OUTPUT
      exit
      ;;

   "suites")
      export COMMAND="$APPLICATION $OPTIONS -@P "
      echo "command: $COMMAND"  | tee $OUTPUT
      $COMMAND 2>&1 | tee -a $OUTPUT
      exit
      ;;

   "")
      ;;

   *)
      export COMMAND="-s $SUITE "
      ;;
esac

export ROUTINE=$1   #  test routine name
shift 1

echo routine $ROUTINE rest $1 | tee -a $OUTPUT

case "$ROUTINE" in

   "")
      ;;

   *)
      echo add routine $ROUTINE
      export COMMAND="$COMMAND -e $ROUTINE $*"
      ;;

esac
export COMMAND="$APPLICATION $OPTIONS $CAMERA_OPTIONS $COMMAND"

echo "command: $COMMAND"  | tee -a $OUTPUT
$COMMAND 2>&1 | tee -a $OUTPUT

