#!/bin/zsh
export OUTPUT=list-camera_aunit.txt
#export CURRENT_DIRECTORY=`pwd`
#export TARGET_SUBDIRECTORY=`realpath "$CURRENT_DIRECTORY/../unit_test"`
#export OPTIONS="-D $TARGET_SUBDIRECTORY"
#export PARAMETERS=$*
#echo PARAMETERS: $PARAMETERS
echo camera aunit 2>&1 | tee $OUTPUT

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

case "$1" in

   help)
      bin/camera_aunit -h | tee -a $OUTPUT
      exit
      ;;

   suites)
      export Command="bin/camera_aunit $OPTIONS -@l"
      echo Command "$Command" | tee -a $OUTPUT
      eval $Command | tee -a $OUTPUT
      exit
      ;;
esac

export APPLICATION=bin/camera_aunit
echo run tests

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

export SUITE=$1
case "$SUITE" in

   "")
      echo no suites specified exiting  | tee -a $OUTPUT
      exit
      ;;

   *)
      export OPTIONS="$OPTIONS -s $SUITE"
      echo "suite: $SUITE"  | tee -a $OUTPUT
      shift 1
      ;;

esac

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

export ROUTINE=$1   # all or test routine name
case "$ROUTINE" in

   "")
      echo no routine specified exiting | tee -a $OUTPUT
      exit
      ;;

   *)
      export OPTIONS="$OPTIONS -e $ROUTINE"
      echo "routine: $ROUTINE"  | tee -a $OUTPUT
      shift 1
      ;;

esac

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

rm GNAT*
export COMMAND="$APPLICATION $OPTIONS $* $VERBOSE"
echo comand: $COMMAND  | tee -a $OUTPUT
eval $COMMAND 2>&1 | tee -a $OUTPUT

