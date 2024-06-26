#!/bin/bash
for (( ;; ))
do
   if [[ "$1" == -* ]]; then
     export OPTIONS="$OPTIONS $1"
     echo "options = $OPTIONS"
     shift 1
   else
      break
   fi
done

export FUNCTION=$1
shift 1

case "$FUNCTION" in

   "help")
      export OPTIONS="-h $OPTIONS"
      ;;

   *)
      ;;

esac

echo OPTIONS $OPTIONS
./run_camera.sh bin camera_control list-camera.txt $OPTIONS $*
