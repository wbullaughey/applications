#!/bin/zsh
export BUILD_MODE=execute
export UNIT_TEST=TRUE
export OUTPUT=list-camera_aunit.txt
export PARAMETERS=$@
echo camera PARAMETERS $PARAMETERS 2>&1 | tee $OUTPUT

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER 2>&1 | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

echo first parameter $1 2>&1 | tee -a $OUTPUT

case $1 in

   help)
      bin/camera_aunit -h 2>&1 | tee -a $OUTPUT
      exit
      ;;

   help_test)
      bin/help_test -E -h -l -P -r -v -x -@c -@d -@i -@l -@m -@p -@P -@S -@t -@u -@x \
          2>&1 | tee -a $OUTPUT
      exit
      ;;

   suites)
      export Command="bin/camera_aunit $OPTIONS -@l"
      echo Command "$Command" 2>&1 | tee -a $OUTPUT
      eval $Command 2>&1 | tee -a $OUTPUT
      exit
      ;;
esac

export APPLICATION=bin/camera_aunit
echo run tests

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER 2>&1 | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

export SUITE=$1
case "$SUITE" in

   "")
      echo no suites specified exiting  2>&1 | tee -a $OUTPUT
      exit
      ;;

   *)
      export OPTIONS="$OPTIONS -s $SUITE"
      echo "suite: $SUITE"  2>&1 | tee -a $OUTPUT
      shift 1
      ;;

esac

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER 2>&1 | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

export ROUTINE=$1   # all or test routine name
case "$ROUTINE" in

   "")
      echo no routine specified exiting 2>&1 | tee -a $OUTPUT
      exit
      ;;

   *)
      export OPTIONS="$OPTIONS -e $ROUTINE"
      echo "routine: $ROUTINE"  2>&1 | tee -a $OUTPUT
      shift 1
      ;;

esac

for PARAMETER in $*; do
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     echo option: $PARAMETER 2>&1 | tee -a $OUTPUT
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

rm GNAT*
export COMMAND="$APPLICATION $OPTIONS $* $VERBOSE"
echo comand: $COMMAND  2>&1 | tee -a $OUTPUT
eval $COMMAND 2>&1 2>&1 | tee -a $OUTPUT

