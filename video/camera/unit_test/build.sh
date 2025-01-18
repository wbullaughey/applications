#!/bin/zsh
source ~/.zshrc
echo building on $OS_VERSION
export LOCATION=$1
export RELATIVE_PATH="Project/git/alr/applications/video/camera/unit_test"
export BUILD_PATH="/Volumes/wayne/$RELATIVE_PATH"
echo BUILD_PATH $BUILD_PATH

pwd
case  ${OS_VERSION%%.*} in

   "15")
      echo desktop
      echo $PATH
      ./rsync.sh
      which sshpass
      sshpass -p 'grandkidsaregreat' ssh wayne@MacBook $BUILD_PATH/build.sh remote
      ;;

   *)
      export OUTPUT=build.txt
      echo laptop 2>&1 | tee $OUTPUT
      case $LOCATION in

         "local")
            echo local build
            ;;

         "remote")
            echo remote build
            cd /Users/wayne/Project/git/alr/applications/video/camera/unit_test
            ;;

         *)
            echo "invalid build location $LOCATION"
            exit
            ;;

      esac
      pwd 2>&1 | tee -a $OUTPUT
      echo "path=$PATH"
      which alr
      alr -v build -- -j10 -s -k -gnatE 2>&1 | tee -a $OUTPUT
      rsync -lptv bin/camera_aunit $BUILD_PATH/bin
      ;;
esac

