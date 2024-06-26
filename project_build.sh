#!/bin/sh
echo running $0 at `pwd`

echo "GPR_DIRECTORY=$GPR_DIRECTORY"
echo EXTRA_PATHS=$EXTRA_PATHS
echo "GNOGA_PATH $GNOGA_PATH"
echo "GPR_OPTIONS $GPR_OPTIONS"

$GPR_DIRECTORY/build_base.sh gprbuild $PROGRAM $ARCHITECTURE $TARGET $PROJECT_DIRECTORY \
-aP.                                            \
-aP $GPR_DIRECTORY                              \
-aP $PROJECT_DIRECTORY                          \
$GNOGA_PATH                                 \
$EXTRA_PATHS

