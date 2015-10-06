#!/bin/sh

flist=`ls -1d *-*`

for f in `echo $flist` ; do
  fp=`echo $f | awk -F. '{print $1}'`
  fg="$fp.gs"
  echo "renaming $f to $fg ..."
#  mv $f $fg
done


exit
