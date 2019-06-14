#! /bin/ksh

typeset -Z2 PERT
LABELI=`date +'%Y%m%d'`12
LABELI=$1
LABELF=`/gfs/home3/io_dop/bin/caldate.3.0.1 $LABELI + 36h 'yyyymmddhh'`
QUEUE=Inter
OPERM=/gfs/home3/io_dop/tempo/global/oenspro
TRUNC=126
LEV=28
MAQUI=sx6
NPERT=07  # NUMERO DE PERTURBACOES

####################################################
#
# PREPARA A ANALISE
#
./runense1.sx6 126 28 $LABELI 15 07 NO AVN
while [ 0 ]
do
sleep 10
if [ `qstat | grep setense. | wc -l` -ge 01 ]
then
echo 'RODANDO RUNENSE1.SX6... sleep 60'
sleep 60
else
echo '+++ RUNENSE1.SX6 RODADO! saindo...'
break
fi
done

####################################################
#
# RODADA DE CONTROLE
#
./runctrpntg1.sx6 126 28 $LABELI $LABELF sstwkl
while [ 0 ]
do
sleep 10
if [ `qstat | grep setctrpn | wc -l` -ge 01 ]
then
echo 'RODANDO RUNCTRPNTG.SX6... sleep 60'
sleep 60
else
echo '+++ RUNCTRPNTG.SX6 RODADO! saindo...'
break
fi
done


echo '+++ RODANDO OS MEMBROS!'
####################################################
#
# RODADA DOS MEMBROS
#

./runpntg1.sx6 126 28 $LABELI 15 AVN 07 sstwkl

PERT=1
while [ $PERT -le ${NPERT} ]
do
echo "\n+++ MEMBRO $PERT - SUBMETENDO...\n"

./runperpnt1.sx6 126 28 $PERT R $LABELI $LABELF 15 NO 07 sstwkl
sleep 10
PERT=$(($PERT+1))
done

while [ 0 ]
do
sleep 10
CTRL1=`qstat | grep setpntg  | wc -l`
CTRL2=`qstat | grep setper   | wc -l`
CTRL3=`qstat | grep setctrpn | wc -l`
if [ $CTRL1 -ge 01 -o $CTRL2 -ge 01 -o $CTRL3 -ge 01 ]
then
echo 'RODANDO MEMBROS... sleep 60'
sleep 60
else
echo '+++ MEMBROS RODADOS! saindo...'
break
fi
done

while [ 0 ]
do
sleep 10
CTRL1=`qstat | grep setpntg  | wc -l`
CTRL2=`qstat | grep setper   | wc -l`
CTRL3=`qstat | grep setctrpn | wc -l`
if [ $CTRL1 -ge 01 -o $CTRL2 -ge 01 -o $CTRL3 -ge 01 ]
then
echo 'RODANDO MEMBROS... sleep 60'
sleep 60
else
echo '+++ MEMBROS RODADOS! saindo...'
break
fi
done

./criasize.ksh ${LABELI} # CRIA A LISTAGEM DE ARQUIVOS "SIZEBANGU"

./runensmed1.sx6 run 126 28 ${LABELI} 15 30 15 01
echo 'RODANDO ENSEMBLE MEDIO... sleep 60'
while [ 0 ]
do
sleep 10
if [ `qstat | grep setensm | wc -l` -ge 01 ]
then
echo 'RODANDO RUNENSMED1.SX6... sleep 60'
sleep 60
else
echo '+++ RUNENSMED1.SX6 RODADO! saindo...'
break
fi
done

# GRIBA O ENSEMBLE MEDIO
./rungpgrib.sx6 ${LABELI} ENM
while [ 0 ]
do
sleep 10
if [ `qstat | grep GRIBA | wc -l` -ge 01 ]
then
echo 'RODANDO RUNENSMED1.SX6... sleep 60'
sleep 60
else
echo '+++ RUNENSMED1.SX6 RODADO! saindo...'
break
fi
done

BANGU=/bangu/samfs/io_dop/tempo/global/oenspro/nmc/T126L28
OENSDK=/gfs/dk19/io_dop/tempo/global/oenspro

YYYY=`echo $LABELI | cut -c 1-4`
MM=`echo $LABELI | cut -c 5-6`
DD=`echo $LABELI | cut -c 7-8`
HH=`echo $LABELI | cut -c 9-10`

echo "\n+++ Movendo Arquivos para BANGU\n"

mkdir -p ${BANGU}/ANL/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GDHN/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/

sleep 300

echo " "
echo "cp -rpf ${OENSDK}/pos/dataout/T126L28/*${LABELI}2* ${BANGU}/ANL/${YYYY}/${MM}/${DD}/"
#echo "cp -rpf ${OENSDK}/model/dataout/T126L28/GFCT*${LABELI}2* ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/"
#echo "cp -rpf ${OENSDK}/model/dataout/T126L28/GFGH*${LABELI}2* ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/"
echo "cp -rpf ${OENSDK}/produtos/grib/${YYYY}/${MM}/${DD}/GPOSENM${LABELI}* ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/"
echo "cp -rpf ${OENSDK}/produtos/grib/${YYYY}/${MM}/${DD}/GPOS???${LABELI}* ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/"
echo " "

cp -rpf ${OENSDK}/pos/dataout/T126L28/*${LABELI}2* ${BANGU}/ANL/${YYYY}/${MM}/${DD}/
#cp -rpf ${OENSDK}/model/dataout/T126L28/GFCT*${LABELI}2* ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/
#cp -rpf ${OENSDK}/model/dataout/T126L28/GFGH*${LABELI}2* ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/
cp -rpf ${OENSDK}/produtos/grib/${YYYY}/${MM}/${DD}/GPOSENM${LABELI}* ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/
cp -rpf ${OENSDK}/produtos/grib/${YYYY}/${MM}/${DD}/GPOS???${LABELI}* ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/

rm -f ${OENSDK}/pos/dataout/T126L28/*${LABELI}2* 
rm -f ${OENSDK}/model/dataout/T126L28/GFCT*${LABELI}2*
rm -f ${OENSDK}/model/dataout/T126L28/GFGH*${LABELI}2*
rm -f ${OENSDK}/produtos/grib/${YYYY}/${MM}/${DD}/GPOSENM${LABELI}*
rm -f ${OENSDK}/produtos/grib/${YYYY}/${MM}/${DD}/GPOS???${LABELI}*

set +x

#mv -f ${OENSDK}/pos/dataout/T126L28/*${LABELI}2* ${BANGU}/ANL/${YYYY}/${MM}/${DD}/
#mv -f ${OENSDK}/model/dataout/T126L28/GFCT*${LABELI}2* ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/
#mv -f ${OENSDK}/model/dataout/T126L28/GFGH*${LABELI}2* ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/
#mv -f ${OENSDK}/produtos/grib/${YYYY}/${MM}/${MM}/GPOSENM${LABELI}* ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/
#mv -f ${OENSDK}/produtos/grib/${YYYY}/${MM}/${MM}/GPOS???${LABELI}* ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/

echo "\n\nSAINDO...\n"
exit 0


