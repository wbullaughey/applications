#!/bin/zsh
export OUTPUT=list-camera_driver.txt
export CURRENT_DIRECTORY=`pwd`
export TARGET_SUBDIRECTORY=`realpath "$CURRENT_DIRECTORY/../unit_test"`
export OPTIONS="-r -D $TARGET_SUBDIRECTORY"
export PARAMETERS=("$@")
echo PARAMETERS: $PARAMETERS
echo OUPUT=$OUTPUT
echo camera driver 2>&1 | tee $OUTPUT
export TRACE=1

trace() {
   if (( $TRACE > 0 )) then
      echo $* 2>&1 | tee -a $OUTPUT
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
           trace option: $PARAMETER | tee -a $OUTPUT
           export OPTIONS="$OPTIONS $PARAMETER"
           trace OPTIONS $OPTIONS
           remove_1
           ;;

         "@")
           trace option: $PARAMETER
           export OPTIONS="$OPTIONS -X${PARAMETER[@]:1}"
           trace OPTIONS $OPTIONS
           remove_1
           ;;

         *)
            break;
           ;;
      esac
   done
}

trace PARAMETERS $PARAMETERS

parameters  # process leading options

export MODE="${PARAMETERS[1]}"
trace MODE $MODE | tee -a $OUTPUT
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
      echo missing mode
      exit
      ;;

esac
trace APPLICATION $APPLICATION

export SUITE="${PARAMETERS[1]}"
remove_1
parameters  # process leading options
trace "suite: $SUITE"
case "$SUITE" in

   "")
      echo no suites specified, run all
      ;;

   *)
      export OPTIONS="$OPTIONS -u $SUITE"
      ;;

esac

export ROUTINE="${PARAMETERS[1]}"
remove_1
parameters  # process leading options

export VERBOSE="-v -X -v"
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
export COMMAND="./bin/camera_driver $OPTIONS $VERBOSE"
echo comand: $COMMAND 2>&1 | tee -a $OUTPUT
rm GNAT-*
eval $COMMAND 2>&1 | tee -a $OUTPUT



