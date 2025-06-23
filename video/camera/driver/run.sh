#!/bin/zsh
export APPLICATION=camera_driver
export OUTPUT=list-driver.txt

echo "run driver $*" 2>&1 | tee $OUTPUT
echo PARAMETERS: $PARAMETERS 2>&1 | tee -a $OUTPUT
echo OUPUT=$OUTPUT 2>&1 | tee -a $OUTPUT
export TRACE=1

trace() {
   if (( $TRACE > 0 )) then
      echo "TRACE: $*" 2>&1 | tee -a $OUTPUT
   fi
}

remove_1() {
   PARAMETERS=("${PARAMETERS[@]:1}")
   trace shifted $PARAMETERS
}

parameters() {
   trace parameters function /$PARAMETERS/
   for PARAMETER ("$PARAMETERS[@]"); do
      trace option $PARAMETER
      export FIRST=${PARAMETER:0:1}
      trace FIRST $FIRST
      case $FIRST in
         "-")
           trace option: $PARAMETER
           export OPTIONS="$OPTIONS $PARAMETER"
           trace OPTIONS $OPTIONS
           remove_1
           ;;

#        "@")
#          trace option: $PARAMETER
#          export OPTIONS="$OPTIONS -X${PARAMETER[@]:1}"
#          trace OPTIONS $OPTIONS
#          remove_1
#          ;;

         *)
            break;
           ;;
      esac
   done
}

parameters  # process leading options

trace PARAMETERS $PARAMETERS

case $PARAMETERS[1] in

   "help")
      echo "driver [<options>] [<mode>][suite][routine]"
      echo "   <options> := -<driver option> | @<camera option>"
      echo "   <mode> := help_test"
      echo ""
      echo " mode"
      echo "   help_test  : tests all of the options"
      echo "   run        : tests all of the options"
      echo ""
      bin/$APPLICATION -h 2>&1 | tee -a $OUTPUT
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

   "help_test")
      bin/$APPLICATION help_test -h -l -P -r -v -@c -@i -@p -@t -@x \
      -a abcCehiIlmMoOpPrRsStT@c@d@D@e@E@l@o@s@t \
      -t aopt \
      -=D subdirectory \
      -=R routine \
      -=u suite \
      -=X camera_options \
      | tee -a $OUTPUT
      exit
      ;;

   *)
      ;;

esac
trace APPLICATION $APPLICATION

export SUITE="${PARAMETERS[1]}"
echo "suite $SUITE" 2>&1 | tee -a $OUTPUT
remove_1
parameters  # process leading options
trace "suite: $SUITE"
case "$SUITE" in

   "")
      echo no suites specified, run all 2>&1 | tee -a $OUTPUT
      ;;

   "all")
      echo "test all suites" 2>&1 | tee -a $OUTPUT
      ;;

   *)
      export OPTIONS="$OPTIONS -=u $SUITE" 2>&1 | tee -a $OUTPUT
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
      echo no routine specified, run all 2>&1 | tee -a $OUTPUT
      ;;

   *)
      export OPTIONS="$OPTIONS -=R $ROUTINE"
       ;;

esac

parameters  # process leading options

trace final OPTIONS $OPTIONS
export COMMAND="../driver/bin/$APPLICATION $OPTIONS $VERBOSE"
trace "comand: $COMMAND"
rm GNAT-*
echo run command 2>&1 | tee -a $OUTPUT
eval $COMMAND 2>&1 | tee -a $OUTPUT



