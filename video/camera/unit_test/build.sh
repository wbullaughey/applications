#!/bin/zsh
source ~/.zshrc
echo building camera unit test on $OS_VERSION
export APPLICATION_PATH="Project/git/alr/applications"
export RELATIVE_PATH="video/camera/unit_test"
export LOCAL_APPLICATION_PATH="/Users/wayne/$APPLICATION_PATH"
export LOCAL_BUILD_PATH="$LOCAL_APPLICATION_PATH/$RELATIVE_PATH"
export REMOTE_BUILD_PATH="/Volumes/applications/$RELATIVE_PATH"
echo LOCAL_APPLICATION_PATH $LOCAL_APPLICATION_PATH
echo LOCAL_BUILD_PATH $LOCAL_BUILD_PATH
echo RELATIVE_PATH $RELATIVE_PATH
echo REMOTE_BUILD_PATH $REMOTE_BUILD_PATH

pwd
case  ${OS_VERSION%%.*} in

   "15")
      echo desktop
      echo $PATH
      export LOCATION=remote
      $LOCAL_APPLICATION_PATH/rsync.sh
      export LOCAL_OUTPUT=build.txt
      sshpass -p 'grandkidsaregreat' ssh wayne@MacBook $LOCAL_BUILD_PATH/build.sh remote 2>&1 | tee -a $LOCAL_OUTPUT
      echo sshpass completed
      ;;

   *)
      echo laptop $LOCAL 2>&1 | tee $LOCAL_OUTPUT
      export LOCATION=local
      export LOCAL_OUTPUT=build.txt
      echo local build
      cd $LOCAL_BUILD_PATH
      rm $LOCAL_OUTPUT
      pwd 2>&1 | tee -a $LOCAL_OUTPUT
      alr -v build -- -j10 -s -k -gnatE 2>&1 | tee -a $LOCAL_OUTPUT
      echo alr completed
esac

echo LOCATION $LOCATION
case $LOCATION in

   "local")
      # no need to copy
      ;;

   "remote")
      # check if build worked
      export REMOTE_OUTPUT=$REMOTE_BUILD_PATH/build.txt
      echo "REMOTE_OUTPUT $REMOTE_OUTPUT"
      echo grep $REMOTE_OUTPUT
      grep "Build finished successfully" $REMOTE_OUTPUT
      RESULT=$?
      if [ $RESULT -eq 0 ]; then
         echo "compile successfully. copy camera_aunit"
         ls -l $REMOTE_BUILD_PATH/bin
         COMMAND="rsync -lptv $REMOTE_BUILD_PATH/bin/camera_aunit bin"
         echo COMMAND $COMMAND
         eval $COMMAND
      else
         echo "compile failed."
         rm $REMOTE_BUILD_PATH/bin/camera_aunit
      fi
      ;;

   *)
      echo "invalid build location $LOCATION"
      exit
      ;;

esac
