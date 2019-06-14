#!/bin/ksh

for arq in `ls -ltr /gfs/home3/io_dop/users/alex/oenspro/include/dataman.oenspro.bangu.??? | awk '{print $9}'`
do
arqn=`basename $arq`
arqd=`dirname $arq`

cat $arq | sed -e s%"/gfs/dk20/modoper/tempo/global"%"/gfs/dk19/io_dop/users/alex"%g |\
sed -e s%"/bangu/samfs/modoper/tempo/global"%"/rede/hsm/io_dop/users/alex"%g > \
${arq}.tmp

mv ${arq}.tmp ${arq}
done

exit 0
