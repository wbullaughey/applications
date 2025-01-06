#!/bin/zsh
source ~/.zshrc
echo building on $OS_VERSION
pwd
case  ${OS_VERSION%%.*} in

   "15")
      echo desktop
      echo $PATH
      which sshpass
      sshpass -p 'grandkidsaregreat' ssh wayne@MacBook /Users/wayne/Projects/applications/video/camera/unit_test/build.sh
      ;;

   *)
      echo lapstop
      cd /Users/wayne/Projects/applications/video/camera/unit_test
      pwd
      alr -v build -- -j10 -s -k -gnatE
      ;;
esac

