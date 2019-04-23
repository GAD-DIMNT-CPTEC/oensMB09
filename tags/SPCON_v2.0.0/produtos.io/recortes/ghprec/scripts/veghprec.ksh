#!/bin/ksh -x

LABELI=$1


exit 0

ftp -in tucupi.cptec.inpe.br << EOF
user pnt &arun/a*
cd /web1/ftp/pub/produtos/ensemble/GHPREC/
dir PRECSC${LABELI}* ghprec.out
quit
EOF
echo "PASSEI"
set -A EXT grb gmp ctl
set -A SIZ 60000 8000 300 
i=0
while [ $i -lt 3 ]
do
for siz in `cat ghprec.out |grep ${EXT[$i]} | awk '{print $5}'`
do
if [ $siz -ge ${SIZ[$i]} ]
then
echo "Arquivo ${EXT[$i]} Ok"
else
echo "ERRO no Arquivo ${EXT[$i]}... Abortando"
exit 1
fi
done

let i=$i+1
done

echo "SAINDO"

exit 0
