#!/bin/bash 

dir=/gfs/dk22/modoper/tempo/global/oens/wgne/dataout/T126L28
cd $dir

HOST_FTP='ftp.hq.ncep.noaa.gov'
USER_FTP="anonymous"
PWD_FTP="alex.fernandes@cptec.inpe.br"


d=20100724
while [ $d -lt 20100805 ]; do

file1=$d'00_CPTC_OLRA'
file2=$d'00_CPTC_U200'
file3=$d'00_CPTC_U850'

i=0

rm -f ${dir}/.out
ftp -inv $HOST_FTP << EOF > ${dir}/.out

user $USER_FTP $PWD_FTP

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


sleep 120
d=`date -d "$d 1 day" +"%Y%m%d"`
done


rm -f ${dir}/.out
