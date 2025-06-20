#!/bin/zsh
export OUTPUT=list-camera.txt
export PROGRAM=bin/camera_control
export DO_TRACE=1
export HELP_TEST=" \
      -E -h -P -r -v -@c -@i -@p -@t -@x \
      -a abcCehiIlmMoOpPrRsStT@c@d@D@e@E@l@o@s@t \
      -c directory \
      -G amo \
      -T amr \
      -@T abcCglLmsSTVwawcwC \
      -V alo \
      -w 99 "

export USE_DBDAEMON=FALSE

source ../../../global_run.sh $OUTPUT $PROGRAM $DO_TRACE $HELP_TEST $USE_DBDAEMON FALSE $*

