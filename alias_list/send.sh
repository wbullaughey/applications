#!/bin/bash
export MESSAGE=$2
export PARAMETERS=$1
shift 2
export OUTPUT=list-alias_list.txt
export PROGRAM=build/bin/alias_list

function help {
   $PROGRAM -h $* | tee $OUTPUT
   exit
}

case "$PARAMETERS" in

   "help")
      help
      ;;

   *)
      ;;
esac

$PROGRAM $* -p $PARAMETERS -m $MESSAGE -u administrator@ucwc-pa.org -w xmexadcfwhgpvmtj 2>&1 | tee $OUTPUT
