#! /bin/ksh -x

LABELI=$1
LABELF=$2

YYYY=`echo ${LABELI} | cut -c 1-4`
MM=`echo ${LABELI} | cut -c 5-6`
DD=`echo ${LABELI} | cut -c 7-8`

cp -rpf /bangu/samfs/modoper/tempo/global/oens/nmc/T126L28/GFGH/$YYYY/$MM/$DD/*${LABELI}* /gfs/dk20/modoper/tempo/global/oenspro/model/dataout/T126L28/

RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 01N
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 01P
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 02N
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 02P
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 03N
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 03P
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 04N
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 04P
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 05N
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 05P
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 06N
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 06P
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 07N
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} 07P
RunGrh.members.sx6 run 126 28 ${LABELI} ${LABELF} AVN

rm -f /gfs/dk20/modoper/tempo/global/oenspro/model/dataout/T126L28/*${LABELI}*


exit 0
