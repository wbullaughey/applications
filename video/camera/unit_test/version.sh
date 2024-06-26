export VERSION=`sw_vers | grep ProductVersion | cut -d':' -f2` #  | tr -s '\t' ''
echo VERSION $VERSION
