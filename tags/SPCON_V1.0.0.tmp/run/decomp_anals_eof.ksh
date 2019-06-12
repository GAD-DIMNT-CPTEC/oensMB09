#!/bin/bash -x

HOUR=`date +'%H:%M'`

export FILEENV=`find ./ -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`

. ${FILEENV} ${1} ${2}
cd ${HOME_suite}/run

TRC=`echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0"`
LV=`echo ${TRCLV} | cut -c 7-11 | tr -d "L0"`

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

TRUNC=${RESOL}
LEV=${NIVEL}

if [ -z "${2}" ]; then
   echo "PERT: NMC, AVN CTR 01N  "
   exit
else
   PREFIC=${2}
fi

if [ -z "${3}" ]; then
   echo "argument is not set (H)"
   exit
else
   HUMID=${3}
fi

if [ -z "${4}" ]; then
   echo "Fifth argument is not set (LABELI: yyyymmddhh)"
   exit
else
   LABELI=${4}
fi

export NUM=${PREFIC:0:2}
export PT=${PREFIC:2:1}
export NUMPERT=${NPERT}

MR=126; IR=384; JR=192; KR=28; LR=17

MPHN=1; MPTR=1; MPHS=1; MPSAN=1; MPSAS=1
MTHN=1; MTTR=1; MTHS=1; MTSAN=1; MTSAS=1
MQHN=1; MQTR=1; MQHS=1; MQSAN=1; MQSAS=1
MUHN=1; MUTR=1; MUHS=1; MUSAN=1; MUSAS=1
MVHN=1; MVTR=1; MVHS=1; MVSAN=1; MVSAS=1

export PBS_SERVER=aux20-eth4

cd ${HOME_suite}/run

NAMEL=GEOFPE${NUM}
export NAMEL

GNAME=GANL${PREFXI}
export GNAME

NAMER=GANL${PREFXI}
export NAMER

NAMES1=GANL${NUM}P
export NAMES1

NAMES3=GANL${NUM}N
export NAMES3

EXTL=P.rpt
export EXTL

EXTG=S.unf
export EXTG

ERR1=P.rp1
ERR2=P.rp2
ERR3=P.rp3
export ERR1 ERR2 ERR3

ERS1=S.rp1
ERS2=S.rp2
ERS3=S.rp3
export ERS1 ERS2 ERS3

GNAMEL=${NAMEL}${LABELI}${EXTL}.${TRUNC}${LEV}

cat <<EOT3 > ${SC1_suite}/deceof/datain/deceof${NUM}${LABELI}.nml
 &DATAIN
  GNAMEL='${GNAMEL} '
  DIRL='${SC1_suite}/deceof/datain/ '
  DIRI='${SC1_suite}/model/datain/ '
  DIRG='${SC2_suite}/eof/dataout/${TRUNC}${LEV}/ '
  DIRS='${SC1_suite}/model/datain/ '
 &END
 &HUMIDI
  HUM='${HUMID}'
 &END
EOT3

filephn=prssehn${NUM}${MPHN}${LABELI}
fileptr=prssetr${NUM}${MPTR}${LABELI}
filephs=prssehs${NUM}${MPHS}${LABELI}
filepsan=prssesan${NUM}${MPSAN}${LABELI}
filepsas=prssesas${NUM}${MPSAS}${LABELI}
echo 'filephn= '${filephn} 
echo 'fileptr= '${fileptr} 
echo 'filephs= '${filephs} 
echo 'filepsan='${filepsan} 
echo 'filepsas='${filepsas} 
filethn=tempehn${NUM}${MTHN}${LABELI}
filettr=tempetr${NUM}${MTTR}${LABELI}
fileths=tempehs${NUM}${MTHS}${LABELI}
filetsan=tempesan${NUM}${MTSAN}${LABELI}
filetsas=tempesas${NUM}${MTSAS}${LABELI}
echo 'filethn= '${filethn} 
echo 'filettr= '${filettr} 
echo 'fileths= '${fileths} 
echo 'filetsan='${filetsan} 
echo 'filetsas='${filetsas} 
fileqhn=humpehn${NUM}${MQHN}${LABELI}
fileqtr=humpetr${NUM}${MQTR}${LABELI}
fileqhs=humpehs${NUM}${MQHS}${LABELI}
fileqsan=humpesan${NUM}${MQSAN}${LABELI}
fileqsas=humpesas${NUM}${MQSAS}${LABELI}
echo 'fileqhn= '${fileqhn} 
echo 'fileqtr= '${fileqtr} 
echo 'fileqhs= '${fileqhs} 
echo 'fileqsan='${fileqsan} 
echo 'fileqsas='${fileqsas} 
fileuhn=winpehn${NUM}${MUHN}${LABELI}
fileutr=winpetr${NUM}${MUTR}${LABELI}
fileuhs=winpehs${NUM}${MUHS}${LABELI}
fileusan=winpesan${NUM}${MUSAN}${LABELI}
fileusas=winpesas${NUM}${MUSAS}${LABELI}
echo 'fileuhn= '${fileuhn} 
echo 'fileutr= '${fileutr} 
echo 'fileuhs= '${fileuhs} 
echo 'fileusan='${fileusan} 
echo 'fileusas='${fileusas} 
filevhn=winpehn${NUM}${MVHN}${LABELI}
filevtr=winpetr${NUM}${MVTR}${LABELI}
filevhs=winpehs${NUM}${MVHS}${LABELI}
filevsan=winpesan${NUM}${MVSAN}${LABELI}
filevsas=winpesas${NUM}${MVSAS}${LABELI}
echo 'filevhn= '${filevhn} 
echo 'filevtr= '${filevtr} 
echo 'filevhs= '${filevhs} 
echo 'filevsan='${filevsan} 
echo 'filevsas='${filevsas} 

rm -f  ${SC1_suite}/deceof/datain/${GNAMEL}

cat <<EOT2 > ${SC1_suite}/deceof/datain/${GNAMEL}
${GNAME}${LABELI}${EXTG}.${TRUNC}${LEV}
${filephn}
${fileptr}
${filephs}
${filepsan} 
${filepsas}
${filethn}
${filettr}
${fileths}
${filetsan}
${filetsas}
${fileqhn}
${fileqtr}
${fileqhs}
${fileqsan}
${fileqsas}
${fileuhn}
${fileutr}
${fileuhs}
${fileusan}
${fileusas}
${filevhn}
${filevtr}
${filevhs}
${filevsan}
${filevsas}
${NAMES1}${LABELI}${EXTG}.${TRUNC}${LEV}
EOT2

cd ${HOME_suite}/deceof/bin/${TRUNC}${LEV}

cp -v deceof.${TRUNC}${LEV} deceof.${NUM}.${LABELI}.${TRUNC}${LEV}

./deceof.${NUM}.${LABELI}.${TRUNC}${LEV} < ${HOME_suite}/deceof/datain/deceof${NUM}${LABELI}.nml > ${HOME_suite}/deceof/output/deceof.${NUM}.${LABELI}.${HOUR}.${TRUNC}${LEV}

wait

filephn=prssnhn${NUM}${MPHN}${LABELI}
fileptr=prssntr${NUM}${MPTR}${LABELI}
filephs=prssnhs${NUM}${MPHS}${LABELI}
filepsan=prssnsan${NUM}${MPSAN}${LABELI}
filepsas=prssnsas${NUM}${MPSAS}${LABELI}
echo 'filephn= '${filephn} 
echo 'fileptr= '${fileptr} 
echo 'filephs= '${filephs} 
echo 'filepsan='${filepsan} 
echo 'filepsas='${filepsas} 
filethn=tempnhn${NUM}${MTHN}${LABELI}
filettr=tempntr${NUM}${MTTR}${LABELI}
fileths=tempnhs${NUM}${MTHS}${LABELI}
filetsan=tempnsan${NUM}${MTSAN}${LABELI}
filetsas=tempnsas${NUM}${MTSAS}${LABELI}
echo 'filethn= '${filethn} 
echo 'filettr= '${filettr} 
echo 'fileths= '${fileths} 
echo 'filetsan='${filetsan} 
echo 'filetsas='${filetsas} 
fileqhn=humpnhn${NUM}${MQHN}${LABELI}
fileqtr=humpntr${NUM}${MQTR}${LABELI}
fileqhs=humpnhs${NUM}${MQHS}${LABELI}
fileqsan=humpnsan${NUM}${MQSAN}${LABELI}
fileqsas=humpnsas${NUM}${MQSAS}${LABELI}
echo 'fileqhn= '${fileqhn} 
echo 'fileqtr= '${fileqtr} 
echo 'fileqhs= '${fileqhs} 
echo 'fileqsan='${fileqsan} 
echo 'fileqsas='${fileqsas} 
fileuhn=winpnhn${NUM}${MUHN}${LABELI}
fileutr=winpntr${NUM}${MUTR}${LABELI}
fileuhs=winpnhs${NUM}${MUHS}${LABELI}
fileusan=winpnsan${NUM}${MUSAN}${LABELI}
fileusas=winpnsas${NUM}${MUSAS}${LABELI}
echo 'fileuhn= '${fileuhn} 
echo 'fileutr= '${fileutr} 
echo 'fileuhs= '${fileuhs} 
echo 'fileusan='${fileusan} 
echo 'fileusas='${fileusas} 
filevhn=winpnhn${NUM}${MVHN}${LABELI}
filevtr=winpntr${NUM}${MVTR}${LABELI}
filevhs=winpnhs${NUM}${MVHS}${LABELI}
filevsan=winpnsan${NUM}${MVSAN}${LABELI}
filevsas=winpnsas${NUM}${MVSAS}${LABELI}
echo 'filevhn= '${filevhn} 
echo 'filevtr= '${filevtr} 
echo 'filevhs= '${filevhs} 
echo 'filevsan='${filevsan} 
echo 'filevsas='${filevsas} 

rm -f  ${SC1_suite}/deceof/datain/${GNAMEL}

cat <<EOT4 > ${SC1_suite}/deceof/datain/${GNAMEL}
${GNAME}${LABELI}${EXTG}.${TRUNC}${LEV}
${filephn}
${fileptr}
${filephs}
${filepsan}
${filepsas}
${filethn}
${filettr}
${fileths}
${filetsan}
${filetsas}
${fileqhn}
${fileqtr}
${fileqhs}
${fileqsan}
${fileqsas}
${fileuhn}
${fileutr}
${fileuhs}
${fileusan}
${fileusas}
${filevhn}
${filevtr}
${filevhs}
${filevsan}
${filevsas}
${NAMES3}${LABELI}${EXTG}.${TRUNC}${LEV}
EOT4

cd ${HOME_suite}/deceof/bin/${TRUNC}${LEV}

cp -v deceof.${TRUNC}${LEV} deceof.${NUM}.${LABELI}.${TRUNC}${LEV}

./deceof.${NUM}.${LABELI}.${TRUNC}${LEV} < ${HOME_suite}/deceof/datain/deceof${NUM}${LABELI}.nml > ${HOME_suite}/deceof/output/deceof.${NUM}.${LABELI}.${HOUR}.${TRUNC}${LEV}

exit 0
