#!/bin/bash
export OUTPUT=list-alias_list.txt
export PROGRAM=bin/alias_list

function help {
   $PROGRAM -h $* | tee $OUTPUT
   exit
}

case "$1" in

   "help")
      help
      ;;

   *)
      ;;
esac

$PROGRAM $* 2>&1 | tee $OUTPUT
