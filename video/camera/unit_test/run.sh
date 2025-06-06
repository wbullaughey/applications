#!/bin/zsh
export BUILD_MODE=execute
export UNIT_TEST=TRUE
export OUTPUT=list-camera_aunit.txt
export PROGRAM=bin/camera_aunit
export PARAMETERS=$@
export DO_TRACE=1
export APPEND_OUTPUT=""  # first time so output erased
rm -f TRACE.txt

function output() {
   TRACE=$1
   shift 1
   echo "output TRACE $TRACE DO_TRACE $DO_TRACE APPEND TRACE $APPEND_OUTPUT" \
         2>&1 | tee -a TRACE.txt
   case $TRACE in

      "LIST")
         ;;

      "TRACE")
         if [ "$DO_TRACE" -eq 0 ]; then
            return
         fi
   esac
   echo $* 2>&1 | tee $APPEND_OUTPUT $OUTPUT
   export APPEND_OUTPUT=-a  # append from now on
}

function run() {
#  output TRACE pwd `pwd`
#  output TRACE ls `ls bin`
   output TRACE run COMMAND $COMMAND DISPLAY $DISPLAY
#  output TRACE OPTIONS $OPTIONS
#  output TRACE PROGRAM $PROGRAM
#  output TRACE env `env`

   case "$DISPLAY" in

      true)
         $PROGRAM -h 2>&1 | tee $APPEND_OUTPUT $OUTPUT
#        $COMMAND 2>&1 | tee $APPEND_OUTPUT $OUTPUT
         ;;

      false)
         $COMMAND 2>&1| /dev/null
         ;;

      ignore)
         output LIST ignore $COMMAND
         ;;

   esac
   exit;
}

output TRACE camera_tests PARAMETERS $PARAMETERS

for PARAMETER in $*; do    # put all '-' options into OPTIONS
   export FIRST=${PARAMETER:0:1}
   if [[ "$FIRST" = "-" ]]; then
     output TRACE option: $PARAMETER
     export OPTIONS="$OPTIONS $PARAMETER"
     shift 1
   else
      break;
   fi
done

output TRACE first parameter $1

case $1 in

   "gdb")
      shift 1
      export GDB=gdb
      ;;

   *)
      ;;

esac
output TRACE 1st parameter $1

case $1 in

   "hide")
      shift 1
      export DISPLAY=false
      ;;

   "ignore")
      shift 1
      export DISPLAY=ignore
      ;;

   *)
      export DISPLAY=true
      ;;

esac

export DATABASE=$1  # local,remote,connect,none
export KILL=true
#export VERBOSE="-v"

output TRACE check database  "$DATABASE"
case "$DATABASE" in

   "help")
      shift 1
#     export COMMAND="$PROGRAM $OPTIONS -h"
      export COMMAND="bin/camera_aunit $OPTIONS -h"
      run
      ;;

   "help_test")
      output LIST Help Test
      export PROGRAM=bin/help_test
      export COMMAND="$PROGRAM $OPTIONS \
         -E -h -l -P -r -v -x -@c -@d -@i -@l -@m -@p -@P -@S -@t -@u -@x"
      export UNIT_TEST=FALSE
      export BUILD_MODE=help_test
      run
      ;;

   "connect")
      export DATABASE_OPTION="-l"
      export KILL=false
      ;;

   "local")
      export DATABASE_OPTION="-l -L /Users/wayne/bin/dbdaemon"
      ;;

   "remote")
      export DATABASE_OPTION="-r localhost -R /home/wayne/bin/dbdaemon -u wayne"
      ;;

   "none")
      ;;

   "suites")
      shift 1
      output  list suites
      export COMMAND="$PROGRAM -@l $*"
      echo "command: $COMMAND"  | tee -a $OUTPUT
      run
      ;;

   "")
      shift 1
      output LIST no database option provided
      exit
      ;;

   *)
      output LIST unrecognize database option \"$DATABASE\" allowed: local,remote,none
      exit
      ;;

esac

export SUITE=$2     # mwd - Main_Window_with_DBDaemon
                    # mwn - Main_Window_without_DBDaemon
                    # wrd - Widget_Root_With_DBDaemon
                    # wrn - Widget_Root_Without_DBDaemon
                    # all - all swites
export ROUTINE=$3   # all or test routine name
output LIST DATABASE $DATABASE
output LIST SUITE $SUITE
output LIST routine $ROUTINE

shift 3


case "$SUITE" in

   all)
      ;;

   "")
      output LIST "missing suite"
      exit;
      ;;

   *)
       export SUITE_OPTION="-s $SUITE"
       ;;

esac

case "$ROUTINE" in

   all)
      ;;

   -*)
      output LIST missing routine
      exit;
      ;;

   "")
      output LIST missing routine
      exit;
      ;;

   *)
      output LIST routine $ROUTINE
      export ROUTINE_OPTION="-e $ROUTINE"
      ;;

esac

ps ax | grep dbdaemon
case "$DATABASE" in
   true)
       killall -9 dbdaemon
       ;;

   false)
       ;;

   *)
       output LIST kill not set
       ;;

esac
#ps ax | grep dbdaemon
output TRACE DISPLAY $DISPLAY
export COMMAND="$GDB $PROGRAM $OPTIONS $DATABASE_OPTION $SUITE_OPTION $ROUTINE_OPTION  -p 2300" # -S 1
output TRACE "command: $COMMAND"
run

sleep 1
