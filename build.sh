#!/bin/zsh
source ~/.zshrc
echo building on $OS_VERSION
export RELATIVE_PATH="Project/Ada/alire/scope"
export BUILD_PATH="/Volumes/wayne/$RELATIVE_PATH"
echo BUILD_PATH $BUILD_PATH
export OUTPUT=rsync.txt
export LOCAL_PATH=`pwd`

pwd
case  ${OS_VERSION%%.*} in

   "15")
      echo desktop
      echo $PATH
      ../rsync.sh $RELATIVE_PATH
      which sshpass
      sshpass -p 'grandkidsaregreat' ssh wayne@MacBook $BUILD_PATH/build.sh
      ;;

   *)
      export OUTPUT=build.txt
      echo laptop 2>&1 | tee $OUTPUT
      cd /Users/wayne/Project/Ada/alire/scope
      echo "path=$PATH"
      which alr
      alr -v build -- -j10 -s -k -gnatE 2>&1 | tee -a $OUTPUT
      rsync -lptv bin/camera_aunit $BUILD_PATH/bin
      ;;
esac

