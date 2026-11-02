#!/bin/bash
# command=[options]{suite [routine]} | help | help_test

export OUTPUT=list-driver_unit.txt
export DO_TRACE=1
export PROGRAM=bin/driver_unit_test

source ../../../../../routines.sh

export MODE=PARAMETERS[1]
output TRACE MODE $MODE
case "$MODE" in

   "help")
         export OPTIONS="$OPTIONS -h "
         run
      ;;

   "help_test")
      export PROGRAM=bin/help_test
      export OPTIONS="-h -l -P -r -v -x -@c -@d -@i -@l -@m -@p -@P -@S -@t -@n2 -@u -@x \
         -1 aAbcCdmopqsSt \
         -a abcCehiIlmMoOpPrRsStT@c@d@D@e@E@l@o@s@t \
         -d aopt \
         -e xyz \
         -g aego \
         -G amo \
         -s suite \
         -S suites
         -t aopt \
         -T acCdhilmorRsStT@d@T@t \
         -U aAglprstT \
         -w 99 \
         =D subdirectory \
         -@R 123 \
         -=R routine \
         -u suite \
         -=X directory \
         -1 aAbcCdmopqsSt \
         -a abcCehiIlmMoOpPrRsStT@c@d@D@e@E@l@o@s@t \
         -c directory \
         -d aopt \
         -e \
         -g aego \
         -G amo \
         -s suite \
         -S adt \
         -t acCdhilmorRsStT@d@T@t \
         -T aAmot \
         -U aAglprstT \
         -V alo \
         -w port \
         -@D suites \
         -@n seeds \
         -@R routine \
         -@T abcCglLmsSTVwawcwC \
         -=D directory \
         -=u suite "
      run
      ;;

   "suites")
      export OPTIONS="$OPTIONS -@P"
      run
      ;;

   "")
      ;;

   *)
      export SUITE=$MODE
      export COMMAND="-s $SUITE "
      export ROUTINE=PARAMETERS[2]
      ;;
esac

