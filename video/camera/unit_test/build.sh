#!/bin/zsh
source ~/.zshrc
echo building on $OS_VERSION
pwd
case  ${OS_VERSION%%.*} in

   "15")
      echo desktop
      echo $PATH
      ./rsync.sh
      which sshpass
      sshpass -p 'grandkidsaregreat' ssh wayne@MacBook /Volumes/wayne/Project/git/alr/applications/video/camera/unit_test/build.sh
      ;;

   *)
      export OUTPUT=build.txt
      echo laptop 2>&1 | tee $OUTPUT
      cd /Users/wayne/Project/git/alr/applications/video/camera/unit_test
      pwd 2>&1 | tee -a $OUTPUT
      echo "path=$PATH"
      which alr
      alr -v build -- -j10 -s -k -gnatE 2>&1 | tee -a $OUTPUT
      rsync -lptv bin/camera_aunit /Volumes/wayne/Project/git/alr/applications/video/camera/unit_test/bin
      ;;
esac

