#!/bin/zsh
export OUTPUT=list-camera_driver.txt
echo camera_driver  | tee $OUTPUT
export CURRENT_DIRECTORY=`pwd`
export TARGET_SUBDIRECTORY=`realpath "$CURRENT_DIRECTORY/../unit_test"`
export OPTIONS="-D $TARGET_SUBDIRECTORY"

function quote () {
   echo quote options $*
   case $OPTION_PREFIX in

      -X)
         export OPTIONS="$OPTIONS $OPTION_PREFIX \"$*\""
         ;;

      *)
         export OPTIONS="$OPTIONS $* "
         ;;
   esac
}

function option () {
   PARAMETER=$1
#  echo PARAMETER $PARAMETER  | tee -a $OUTPUT
   case ${PARAMETER:0:1} in

      -x)
         quote ${PARAMETER:1}
         ;;

      *)
         quote $PARAMETER
         ;;

   esac
}

for (( ; ; ))
do
   if [[ "$1" == -* ]]; then
#    echo options $1  | tee -a $OUTPUT
     option $1
     shift 1
   else
      break;
   fi
done
#echo options $OPTIONS  | tee -a $OUTPUT

#export MODE=$1
#shift

#echo MODE $MODE
#case "$MODE" in
#
#   "help")
#      bin/camera_driver -h | tee $OUTPUT
#      exit
#      ;;
#
#   "")
      export APPLICATION=../bin/camera_driver
      export OPTION_PREFIX=-X
#     echo test with the driver
#     ;;
#
#  "test")
#     export APPLICATION=../unit_test/bin/camera_aunit
#     ;;
#
#esac

for (( ; ; ))
do
   if [[ "$1" == -* ]]; then
#    echo options $1 | tee -a $OUTPUT
     option $1
     shift 1
   else
      break;
   fi
done
#echo options $OPTIONS | tee -a $OUTPUT

export SUITE=$1

echo "suite: $SUITE"


case "$SUITE" in

   "")
      echo no suites specified, run all | tee -a $OUTPUT
      ;;

   *)
      export SUITE_OPTION="-s $SUITE"
      shift 1
      ;;

esac

for (( ; ; ))
do
   if [[ "$1" == -* ]]; then
#    echo options $1 | tee -a $OUTPUT
     option $1
     shift 1
   else
      break;
   fi
done

export ROUTINE=$1   # all or test routine name
shift 1
export VERBOSE="-v -X -v"
case "$ROUTINE" in

   all)
      echo all routines
      ;;

   "")
      ;;

   *)
      quote -r $ROUTINE
      shift 1
       ;;

esac

echo options $OPTIONS | tee -a $OUTPUT
export COMMAND="./bin/camera_driver $OPTIONS $* $VERBOSE $SUITE_OPTION $ROUTINE_OPTION"
echo comand: $COMMAND  | tee -a $OUTPUT
pwd
eval $COMMAND 2>&1 | tee -a $OUTPUT

