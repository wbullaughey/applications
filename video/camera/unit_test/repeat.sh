#!/bin/zsh
export ALL_PARAMETERS=$@
export RUN_OUTPUT=list-camera_aunit.txt
export LOOP_OUTPUT=loop-camera_aunit.txt
echo "all parameters $ALL_PARAMETERS"
export COUNT=0
export MAX_LOOPS=25
rm -f $LOOP_OUTPUT

# Define the string to search for
search_string="- exception -"

while true
do
   rm -f $RUN_OUTPUT
   ./run.sh "$@"
   cat $RUN_OUTPUT >> $LOOP_OUTPUT
   let COUNT++
   echo "--------------- $COUNT --------" >> $LOOP_OUTPUT
#  echo $COUNT

   # Use grep to search for the string in the file
   if grep -q -- "$search_string" "$RUN_OUTPUT"; then
     echo "found exception"
     exit;
   else
     echo "no exceptions."
   fi

   if [[ $COUNT = $MAX_LOOPS ]]; then
      echo "quit loop."
      exit
   fi
done
