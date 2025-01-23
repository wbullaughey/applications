#!/bin/zsh
source ~/.zshrc
echo building camera unit test on $OS_VERSION
export APPLICATION_PATH="Project/git/alr/applications"
export RELATIVE_PATH="/video/camera/unit_test"
export LOCAL_APPLICATION_PATH="/Users/wayne/$APPLICATION_PATH"
export LOCAL_BUILD_PATH="$LOCAL_APPLICATION_PATH/$RELATIVE_PATH"
export REMOTE_BUILD_PATH="/Volumes/wayne/$APPLICATION_PATH/$RELATIVE_PATH"
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
#      case $LOCATION in
#
#         "local")
#            export OUTPUT=build.txt
#            echo local build
##           cd $LOCAL_BUILD_PATH
#            ;;
#
#         "remote")
            $LOCAL_APPLICATION_PATH/rsync.sh
      #     which sshpass
      #     sshpass -p 'grandkidsaregreat' ssh  wayne@MacBook ls $REMOTE_BUILD_PATH
            sshpass -p 'grandkidsaregreat' ssh wayne@MacBook $LOCAL_BUILD_PATH/build.sh remote
#           echo "COMMAND $COMMAND
#           eval $COMMAND
            exit
#           ;;
#
#        *)
#           echo "invalid build location $LOCATION"
#           exit
#           ;;
#     esac
      ;;

   *)
      echo laptop $LOCAL 2>&1 | tee $OUTPUT
      export LOCATION=local
      export OUTPUT=build.txt
      echo local build
      cd $LOCAL_BUILD_PATH
      rm $OUTPUT
      pwd 2>&1 | tee -a $OUTPUT
esac
#     echo "path=$PATH"
#     which alr

alr -v build -- -j10 -s -k -gnatE 2>&1 | tee -a $OUTPUT
case $LOCATION in

   "local")
      # no need to copy
      ;;

   "remote")
      # check if build worked
      grep "Build finished successfully" $OUTPUT
      RESULT=$?
      if [ $RESULT -eq 0 ]; then
         echo "compile successfully. copy camera_aunit"
         rsync -lptv bin/camera_aunit $REMOTE_BUILD_PATH/bin
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
