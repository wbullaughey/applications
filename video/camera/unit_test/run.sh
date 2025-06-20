#!/bin/zsh
export OUTPUT=list-camera_aunit.txt
export PROGRAM=bin/camera_aunit
export DO_TRACE=0
export HELP_TEST=" \
   -E -h -P -r -v -x -@c -@d -@i -@l -@m -@p -@P -@S -@t -@u -@x"

export USE_DBDAEMON=FALSE

source ../../../../global_run.sh $OUTPUT $PROGRAM $DO_TRACE $HELP_TEST $USE_DBDAEMON TRUE $*

