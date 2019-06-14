#!/bin/ksh

cd /gfs/home3/io_dop/users/alex/oenspro/run

touch a.undef
i=0
while [ `ls -ltr a.undef | awk '{print $5}'` -le 147643 ]
do
let i=$i+1
echo $i
echo "0" >> a.undef

done

exit 0
