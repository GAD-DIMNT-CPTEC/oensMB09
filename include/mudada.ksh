#!/bin/ksh

for arq in `ls dataman.oenspro*`; do

cat $arq | sed -e s%"/bangu/samfs/modoper/tempo/global/oens"%"/bangu/samfs/modoper/tempo/global/oens/nmc/T126L28"%g > ${arq}.tmp
mv ${arq}.tmp ${arq}

done

exit 0
