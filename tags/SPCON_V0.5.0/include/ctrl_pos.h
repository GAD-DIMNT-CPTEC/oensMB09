tenta=0
set -x
while [ $tenta -lt 5 ]
do
flag=0
##############################################################################################
# FILE CONTROL
#
#### VECTOR DISCRIPTION
# EXT    -> FILE's EXTENSION
# SIZ    -> FILE's SIZE
# QTF    -> FILE's NUMBER
# ARQINI -> INITIAL STRING FILE
#
cont=0
set -A ARQINI GPOS${PREFX}
set -A TYP P.icn P.inz P.fct
YYYY=`echo $LABELI | cut -c 1-4`
MM=`echo $LABELI | cut -c 5-6`
DD=`echo $LABELI | cut -c 7-8`

ls -ltr ${ROPERM}/pos/dataout/T${TRC}L${LV}/${ARQINI[0]}${LABELI}* > lsgrb${HH}a2.out

k=0
while [ ${k} -lt 3 ]
do
case ${k} in
0)
        set -A EXT grb idx ctl
        set -A SIZ 23314006 3865 4783
        set -A QTF 1 1 1
;;
1)
        set -A EXT grb idx ctl
        set -A SIZ 23314006 3865 4783
        set -A QTF 1 1 1
;;
2)
        set -A EXT grb idx ctl
        set -A SIZ 24789576 3009 5743
        set -A QTF 60 60 60
;;
esac

j=0
while [ ${j} -lt 3 ]
do
echo ${ARQINI[0]}${LABELI}"*"${TYP[$k]}"*"${EXT[$j]}
cont=0
for arq in `cat lsgrb${HH}a2.out | grep ${ARQINI[0]}${LABELI} | grep ${TYP[$k]} |grep ${EXT[$j]}| awk '{print $5}'`
do
#        echo $arq
        if [ $arq -ge ${SIZ[$j]} ]
        then
                cont=$(($cont+1))
        fi
done
if [ ${cont} -ge ${QTF[$j]} ]
then
        echo "*** Number of Files - OK ${ARQINI[0]} ${EXT[$j]}"
        smslabel Info "Number of Files - OK ${ARQINI[0]} ${EXT[$j]}"
        flag=0
else
        echo "*** Number of Files ${ARQINI[0]} ${EXT[$j]} - ERROR ${cont}"
        smslabel Info "Number of Files ${ARQINI[0]} ${EXT[$j]}"
        flag=1
        sleep 60
fi

j=$(($j+1))
done

k=$(($k+1))
done
rm -f lsgrb${HH}a2.out

if [ `ls -ltr ${ROPERM}/pos/dataout/T${TRC}L${LV}/GFGN${PREFX}${LABELI}*T${TRC}L${LV} | awk '{print $5}'` -lt 18214560 ]; then
      flag=1
fi
if [ `ls -ltr ${ROPERM}/pos/dataout/T${TRC}L${LV}/GFGN${PREFX}${LABELI}*T${TRC}L${LV}.ctl | awk '{print $5}'` -lt 1166 ]; then
      flag=1
fi

if [ ${flag} -eq 0 ]
then
        break
fi

let tenta=$tenta+1
done

if [ ${flag} -eq 1 ]
then
       smslabel Info "Number of Files ${ARQINI[0]} ${EXT[$j]}"
       exit 1
fi
