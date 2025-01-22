#!/bin/zsh
source ~/.zshrc
export SOURCE=$1
echo building $SOURCE test on $OS_VERSION
export LOCATION=$2
export MODULE=$3
export RELATIVE_PATH="Project/git/alr/applications/$MODULE"
export LOCAL_BUILD_PATH="/Users/wayne/$RELATIVE_PATH"
echo LOCAL_BUILD_PATH $LOCAL_BUILD_PATH
export REMOTE_BUILD_PATH="/Volumes/wayne/$RELATIVE_PATH"
echo REMOTE_BUILD_PATH $REMOTE_BUILD_PATH
export OUTPUT=build.txt

pwd
case  ${OS_VERSION%%.*} in

   "15")
      echo desktop
      echo $PATH
      case $LOCATION in

         "local")
            echo local build
#           cd $LOCAL_BUILD_PATH
            ;;

         "remote")
            ./rsync.sh
      #     which sshpass
      #     sshpass -p 'grandkidsaregreat' ssh  wayne@MacBook ls $REMOTE_BUILD_PATH
            export COMMAND="sshpass -p 'grandkidsaregreat' ssh wayne@MacBook $REMOTE_BUILD_PATH/build.sh local $RELATIVE_PATH
            echo "COMMAND $COMMAND
            eval $COMMAND
            # check if build worked
            echo "output file $REMOTE_BUILD_PATH/$OUTPUT"
            ls -l $REMOTE_BUILD_PATH/$OUTPUT
            grep "Build finished successfully" $REMOTE_BUILD_PATH/$OUTPUT
            RESULT=$?
            if [ $RESULT -eq 0 ]; then
               echo "compile successfully. copy camera_aunit"
               echo "rsync -lptv $REMOTE_BUILD_PATH/bin/$MODULE bin "
               rsync -lptv $REMOTE_BUILD_PATH/bin/$MODULE bin
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
      ;;

   *)
      echo laptop $LOCAL 2>&1 | tee $OUTPUT
      export OUTPUT=build.txt
      echo local build
      cd $LOCAL_BUILD_PATH
      rm $OUTPUT
      pwd 2>&1 | tee -a $OUTPUT
esac
