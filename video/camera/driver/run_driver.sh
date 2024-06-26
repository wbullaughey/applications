#!/bin/bash
export DRIVER=$1
export OUTPUT=$2
shift 2

rm $OUTPUT
export TARGET=camera_aunit
export CURRENT_DIRECTORY=`pwd`
export OUTPUT="$CURRENT_DIRECTORY/$OUTPUT"
echo CURRENT_DIRECTORY $CURRENT_DIRECTORY | tee -a $OUTPUT
echo OUTPUT = $OUTPUT| tee -a $OUTPUT
echo start driver run tee $OUTPUT

for (( ;; ))
do
   echo next parameter $1  | tee -a $OUTPUT

   if [[ "$1" == -* ]]; then
     export DRIVER_OPTIONS="$DRIVER_OPTIONS $1"
     echo DRIVER_OPTIONS = "$DRIVER_OPTIONS"  | tee -a $OUTPUT
     shift 1
   else
      break
   fi
done

echo DRIVER_OPTIONS \'$DRIVER_OPTIONS\' | tee -a $OUTPUT

export TARGET_SUBDIRECTORY=`realpath "$CURRENT_DIRECTORY/../unit_test"`
export SUITE=$1
shift 1

if [[ /$SUITE/ == // ]]; then
   echo all tests  | tee -a $OUTPUT
else
   while [[ /$1/ != // ]]
   do
      export ROUTINES="$ROUTINES -r $1"   #  test routine name
      shift 1
      echo ROUTINES "$ROUTINES" | tee -a $OUTPUT
   done

   export TESTS="-u $SUITE $ROUTINES"
fi
case $DRIVER in

   "driver")
      export DRIVER_SUBDIRECTORY="$CURRENT_DIRECTORY/bin"
      export APPLICATION=camera_driver
      ;;

   "test_driver")
      export DRIVER_SUBDIRECTORY="$CURRENT_DIRECTORY/unit_test/bin"
      export APPLICATION=driver_unit_test
      ;;

   *)
      echo "bad DRIVER $DRIVER" | tee -a $OUTPUT
      exit;
      ;;

esac
echo TARGET_SUBDIRECTORY $TARGET_SUBDIRECTORY | tee -a $OUTPUT
echo DRIVER_SUBDIRECTORY $DRIVER_SUBDIRECTORY | tee -a $OUTPUT

export BIN_DIRECTORY=TARGET_SUBDIRECTORY/bin

echo BIN_DIRECTORY = $BIN_DIRECTORY  | tee -a $OUTPUT
echo APPLICATION = $APPLICATION  | tee -a $OUTPUT
ls -l $BIN_DIRECTORY

cd $BIN_DIRECTORY
echo pwd `pwd`  | tee -a $OUTPUT

export COMMAND="$DRIVER_SUBDIRECTORY/$APPLICATION $DRIVER_OPTIONS \
   -D $TARGET_SUBDIRECTORY $OPTIONS $OPTIONS $TESTS"

rm GNAT-*

echo "command: $COMMAND"  | tee -a $OUTPUT
cd $DRIVER_SUBDIRECTORY
pwd
$COMMAND 2>&1 | tee -a $OUTPUT

