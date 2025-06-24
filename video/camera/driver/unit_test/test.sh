DO_TRACE=1
OUTPUT=test.txt
rm TRACE.txt test.txt

function output() {
echo output parameters $* 2>&1 TRACE.txt
   TRACE=$1
   shift 1
   echo "output TRACE $TRACE DO_TRACE $DO_TRACE \
         OUTPUT $OUTPUT $*" 2>&1 | tee -a TRACE.txt
   case $TRACE in

      "LIST")
         ;;

      "TRACE")
         if [ "$DO_TRACE" -eq 0 ]; then
            return
         fi
   esac
   echo $* 2>&1 | tee -a $OUTPUT
}

PARAMETERS=()
for PARAMETER in $*; do    # put all '-' options into OPTIONS
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     output TRACE option: $PARAMETER
     OPTIONS="$OPTIONS $PARAMETER"
   else
     PARAMETERS+=$PARAMETER
     output TRACE PARAMETER /$PARAMETER/ PARAMETERS: /$PARAMETERS/
   fi
done

export array=()
export array+=abc
export array+=xyz
echo array $array
