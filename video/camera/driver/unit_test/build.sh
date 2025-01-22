#!/bin/zsh
source ~/.zshrc

LOCATION=$1
RELATIVE_PATH=$2
echo LOCATION $LOCATION driver unit test
echo RELATIVE_PATH $RELATIVE_PATH

pwd
case $LOCATION in

   "local")
      case /${RELATIVE_PATH}/ in

         "//")
            echo build on laptop
            ;;

         *)
            echo build from desktop
            echo "cd ~/$RELATIVE_PATH"
            cd ~/$RELATIVE_PATH
            ;;
      esac
      pwd
      alr build -- -j10 -s -k -gnatE  2>&1 | tee build.txt
      ;;

   "remote")
      cd ../../../.. # cd to application subdirectory
      pwd
      ./build_base.sh driver $LOCATION video/camera/driver
      ;;

   *)
      echo "invalid build location $LOCATION"
      ;;

esac
