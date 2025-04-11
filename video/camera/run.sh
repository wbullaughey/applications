#!/bin/zsh
export APPLICATION=camera_control
export OUTPUT=list-camera.txt
export CURRENT_DIRECTORY=`pwd`
export PARAMETERS=("$@")
echo PARAMETERS: $PARAMETERS 2>&1 | tee -a $OUTPUT
echo OUPUT=$OUTPUT 2>&1 | tee $OUTPUT
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

export MODE="${PARAMETERS[1]}"
trace MODE $MODE
remove_1
parameters  # process leading options
case "$MODE" in

   "help")
      echo "run.sh [<options>] [<mode>][suite][routine]"
      echo "   <options> := -<driver option> | @<camera option>"
      echo "   <mode> := help_test"
      echo ""
      echo " mode"
      echo "   help_test  : tests all of the options"
      echo "   <null>     : runs $APPLICATION"
      echo ""
      bin/$APPLICATION -h 2>&1 | tee -a $OUTPUT
      exit
      ;;

   "help_test")
      bin/$APPLICATION help_test -E -h -l -P -r -v -@c -@i -@p -@t -@x \
      -a abcCehiIlmMoOpPrRsStT@c@d@D@e@E@l@o@s@t \
      -c directory \
      -G amo \
      -T amr \
      -@T abcCglLmsSTVwawcwC \
      -V alo \
      -w 99 \
      2>&1 | tee -a $OUTPUT
      exit
      ;;

   *)
      ;;

esac
trace APPLICATION $APPLICATION

parameters  # process leading options

trace final OPTIONS $OPTIONS
export COMMAND="bin/$APPLICATION $OPTIONS $VERBOSE"
trace "comand: $COMMAND"
echo run $APPLICATION 2>&1 | tee -a $OUTPUT
eval $COMMAND 2>&1 | tee -a $OUTPUT



