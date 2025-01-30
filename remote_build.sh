#!/bin/zsh
source ~/.zshrc
echo remote_build
pwd

export BUILD_PATH=$1
export RELATIVE_PATH=$2
export PROGRAM=$3
export QUICK_BUILD=$4

echo building $PROGRAM on $OS_VERSION RELATIVE_PATH $RELATIVE_PATH BUILD_PATH $BUILD_PATH

export APPLICATION_PATH="Project/git/alr/applications"
export LOCAL_APPLICATION_PATH="/Users/wayne/$APPLICATION_PATH"
export LOCAL_BUILD_PATH="$LOCAL_APPLICATION_PATH/$RELATIVE_PATH"
export REMOTE_BUILD_PATH="/Volumes/applications/$RELATIVE_PATH"
#echo LOCAL_APPLICATION_PATH $LOCAL_APPLICATION_PATH
#echo LOCAL_BUILD_PATH $LOCAL_BUILD_PATH
#echo QUICK_BUILD $QUICK_BUILD
#echo RELATIVE_PATH $RELATIVE_PATH
#echo REMOTE_BUILD_PATH $REMOTE_BUILD_PATH

function local_build () {
   export LOCAL_OUTPUT=build.txt
   echo local build
   cd $BUILD_PATH
   rm -f $LOCAL_OUTPUT
   pwd 2>&1 | tee -a $LOCAL_OUTPUT
   alr build -- -j10 -s -k -gnatE 2>&1 | tee -a $LOCAL_OUTPUT
   echo alr completed
}

case  ${QUICK_BUILD} in

   "")
      ;;

   *)
      local_build
      exit;
esac

case  ${OS_VERSION%%.*} in

   "15")
      echo desktop
      echo $PATH
      export LOCATION=remote
      $LOCAL_APPLICATION_PATH/rsync.sh
      export LOCAL_OUTPUT=build.txt
      export BUILD_HOME=`pwd`
      echo BUILD_HOME $BUILD_HOME
      sshpass -p 'grandkidsaregreat' ssh wayne@MacBook $LOCAL_APPLICATION_PATH/remote_build.sh $BUILD_PATH $RELATIVE_PATH $PROGRAM 2>&1 | tee -a $LOCAL_OUTPUT
      echo sshpass completed
      ;;

   *)
      echo laptop $LOCAL 2>&1 | tee $LOCAL_OUTPUT
      export LOCATION=local
      local_build
esac

echo LOCATION $LOCATION
case $LOCATION in

   "local")
      # no need to copy
      ;;

   "remote")
      # check if build worked
      export REMOTE_OUTPUT=$REMOTE_BUILD_PATH/build.txt
#     echo "REMOTE_OUTPUT $REMOTE_OUTPUT"
#     echo grep $REMOTE_OUTPUT
      grep "Build finished successfully" $REMOTE_OUTPUT
      RESULT=$?
      if [ $RESULT -eq 0 ]; then
         echo "compile successfully. copy $PROGRAM"
         ls -l $REMOTE_BUILD_PATH/bin
         COMMAND="rsync -lptv $REMOTE_BUILD_PATH/bin/$PROGRAM bin"
         echo COMMAND $COMMAND
         eval $COMMAND
      else
         echo "compile failed."
         rm -f $REMOTE_BUILD_PATH/bin/camera_aunit
      fi
      ;;

   *)
      echo "invalid build location $LOCATION"
      exit
      ;;

esac
