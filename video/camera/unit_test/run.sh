#!/bin/zsh
export MODE=$1
if [[ -z "$MODE" ]]; then
  echo "var is null/empty"
else
   shift 1
fi
export OUTPUT=list-camera_aunit.txt
export PROGRAM=bin/camera_aunit
export DO_TRACE=1
export HELP_TEST=" \
   -E -h -P -r -v -x -@c -@d -@i -@l -@m -@p -@P -@S -@t -@u -@x"

export USE_DBDAEMON=FALSE

case $MODE in

   "help" | "suites")
      ;;

   "remote-camera" | "local-canera")
      MODE="-C single_camera.cfg $MODE"
      ;;

   "local-no-camera")
      ;;

   *)
      echo bad mode MODE
      echo valid: local-camera local-no-camera remote-camera
      exit
      ;;

esac

#echo MODE $MODE

source ../../../../global_run.sh $OUTPUT $PROGRAM $DO_TRACE $HELP_TEST $USE_DBDAEMON TRUE $MODE $*

