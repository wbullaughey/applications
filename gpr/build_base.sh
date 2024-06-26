
#!/bin/sh
echo running $0 at $CURRENT_DIRECTORY
export TOOL=$1
export APPLICATION=$2
export ARCHITECTURE=$3
export GCC_ARCHITECTURE=`gcc -dumpmachine`
export TARGET=$4
export PROJECT_DIRECTORY=$5
export GPR_OPTIONS=$6
shift 6
export BUILD_DATE=`date`
export EXTRA_CPP_FLAGS='-DOBSOLETE_DEFS=1'
export GPR="-P $APPLICATION$APPLICATION_SUFFIX.gpr"
export GPR_BUILD_OPTIONS="-j10 -p" # -vP2"
export GPR_INSTALL_OPTIONS="$GPR_INSTALL_OPTIONS -v -f -m"
export LIB=static

echo "OS_Type = $OS_Type"

case /$ARCHITECTURE/ in

    /windows/)
        echo "building for gcc target " $GCC_ARCHITECTURE
        export PRJ_TARGET=Windows_NT
        ;;

    /macosx/)
        echo load bash_profile
        source "/Users/$USER/.bash_profile"
        export PRJ_TARGET=OSX
        ;;
    /linux.)
        echo load bash_profile
        source "/home/$USER/.bash_profile"
        export PRJ_TARGET=LINUX
        ;;

    //)
        echo "missing ARCHITECTURE"
        exit
        ;;
    *)
        export GPR_INSTALL_OPTIONS="$GPR_INSTALL_OPTIONS --prefix=/embed/uclibc_lightsender/c++"
        ;;
esac

echo SHELL $SHELL

case $SHELL in

   /bin/bash)
      source ~/.bash_profile
      echo ".bash_profile PATH $PATH"
      ;;

   /bin/zsh)
      source ~/.zshrc
      echo ".zshrc PATH $PATH"
      ;;

   *)
      echo "default PATH $PATH"
      ;;

esac


if [ "/$ARCHITECTURE_DIRECTORY/" == "//" ]; then
    export ARCHITECTURE_DIRECTORY=$ARCHITECTURE
fi

if [ "/$GC_ARCHITECTURE/" == "//" ]; then
    export GC_ARCHITECTURE=$ARCHITECTURE
fi

if [ "/$OS/" == "//" ]; then
    export OS=$ARCHITECTURE
fi

if [ "$TOOL" == "gprbuild" ]; then
    export GPR_OPTIONS="$GPR_BUILD_OPTIONS $GPR_OPTIONS"
elif [ "$TOOL" == "gprinstall" ]; then
    export GPR_OPTIONS="$GPR_INSTALL_OPTIONS $GPR_OPTIONS"
else
    echo missing tool
    exit
fi

if [ /$_system_name/ == /OSX/ ]; then
    source ~/.bash_profile
fi

echo GNAT_TOOL_PATH $GNAT_TOOL_PATH
if [ /$CONFIGURATION/ != // ]; then
    export CONFIGURATION="--config=/etc/launchd.conf"
fi

if (( $TRACE ))
then
   echo APPLICATION=$APPLICATION
   echo APPLICATION_SUFFIX=$APPLICATION_SUFFIX
   echo ARCHITECTURE=$ARCHITECTURE
   echo ARCHITECTURE_DIRECTORY=$ARCHITECTURE_DIRECTORY
   echo building libraries with $LIB
   echo CONFIGURATION=$CONFIGURATION
   echo EXTRA_PATHS=$EXTRA_PATHS
   echo GC_ARCHITECTURE=$GC_ARCHITECTURE
   echo GPR=$GPR
   echo "GPR_OPTIONS $GPR_OPTIONS"
   echo GPR_PATH=$GPR_PATH
   echo GLOBAL_DIRECTORY=$GLOBAL_DIRECTORY
   echo GPR_OPTIONS=$GPR_OPTIONS $GPR_BUILD_OPTIONS
   echo GPR_PROJECT_PATH = "$GPR_PROJECT_PATH"
   echo OS=$OS
   echo "PROJECT_DIRECTORY=$PROJECT_DIRECTORY"
   echo TARGET=$TARGET
   echo TOOL=$TOOL
   echo USER=$USER
fi

sleep 2     # make sure network write completes before reads start
which $TOOL
which gprconfig
export COMMAND="$TOOL $GPR $GPR_OPTIONS $CONFIGURATION $*"
#if (( $TRACE ))
#then
   echo PATH=$PATH
   echo "COMMAND=$COMMAND"
#fi
$COMMAND


