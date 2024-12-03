#!/bin/bash
export OUTPUT=list-driver.txt
#for (( ;; ))
#do
#   if [[ "$1" == -* ]]; then
#     export OPTIONS="$OPTIONS $1"
#     echo "options = $OPTIONS"
#     shift 1
#   else
#      break
#   fi
#done
#
#export FUNCTION=$1
#shift 1
#
#case "$FUNCTION" in
#
#   "help")
#      export OPTIONS="-h $OPTIONS"
#      ;;
#
#   *)
#      ;;
#
#esac
#
#echo OPTIONS $OPTIONS
bin/camera_driver $* 2>&1 | tee $OUTPUT
