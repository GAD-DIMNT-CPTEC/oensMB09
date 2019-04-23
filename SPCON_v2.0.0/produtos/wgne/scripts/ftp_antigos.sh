#!/bin/bash  -x


for d in $(seq 20120301 20120331);  do

d=${d}00
echo FAZENDO:   $d
dir=/stornext/online7/pnt/oens_MCGA/wgne/$d
cd $dir

HOST_FTP='ftp.hq.ncep.noaa.gov'
USER_FTP="anonymous"
PWD_FTP="alex.fernandes@cptec.inpe.br"

file1=$d'_CPTC_OLRA'
file2=$d'_CPTC_U200'
file3=$d'_CPTC_U850'

i=0

rm -f ${dir}/.out
ftp -invp $HOST_FTP << EOF > ${dir}/.out

user $USER_FTP $PWD_FTP
espv

cd /pub/incoming/WHphase

put $file1
put $file2
put $file3

close
EOF

a=`cat ${dir}/.out | grep "150 Ok to send data" | wc -l`

if [ $a -eq 3 ]; then
      echo "FTP OK"
else
      echo "FTP COMPROBLEMAS..."
      exit 3
fi

rm -f ${dir}/.out

done
