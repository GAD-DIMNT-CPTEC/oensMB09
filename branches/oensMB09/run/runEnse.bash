#!/bin/bash -x
#help#
#******************************************************************#
#                                                                  #
#     Name:           runense.sx6                                  #
#                                                                  #
#     Function:       This script submits the                      #
#                     ensemble script.                             #
#                     It runs in Korn Shell.                       #
#                                                                  #
#     Date:           May    26th, 2003.                           #
#     Last change:    May    26th, 2003.                           #
#                                                                  #
#     Valid Arguments for runense.sx6                              #
#                                                                  #
#     First :    HELP: help or nothing for getting help            #
#    First  : COMPILE: help, make, clean or run                    #
#    Second :     TRC: three-digit triangular truncation           #
#     Third :      LV: two-digit number of vertical sigma-layers   #
#     Forth :  LABELI: initial forecasting label                   #
#     Fifth :  NFDAYS: number of forecasting days                  #
#     Sixth : NUMPERT: number of random perturbations              #
#   Seventh :   HUMID: YES or NO (humidity will be perturbed)      #
#                                                                  #
#              LABELx : yyyymmddhh                                 #
#                       yyyy = four digit year                     #
#                       mm = two digit month                       #
#                       dd = two digit day                         #
#                       hh = two digit hour                        #
#                                                                  #
#******************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
cat < ${0} | sed -n '/^#help#/,/^#help#/p'
exit 0
fi
#
#       Test of Valid Arguments
#
if [ "${1}" != "run" ]
then
if [ "${1}" != "make" ]
then
if [ "${1}" != "clean" ]
then
echo "First argument: ${1}, is wrong. Must be: make, clean or run"
exit
fi
fi
fi

if [  -z "${2}" ]
then
echo "Second argument is not set: TRC"
exit
else
export TRC=`echo $2 | awk '{print $1/1}'`
fi

if [  -z "${3}" ]
then
echo "Third argument is not set (LV)"
exit
else
export LV=`echo ${3} | awk '{print $1/1}'` 
fi
if [  -z "${4}" ]
then
echo "Forth argument is not set (LABELI yyyymmddhh)"
exit
fi
if [  -z "${5}" ]
then
echo "Fifth argument is not set (NFDAYS)"
exit
fi
if [  -z "${6}" ]
then
echo "Sixth argument is not set (NUMPERT)"
exit
fi
if [  -z "${7}" ]
then
echo "Seventh argument is not set (HUMID)"
exit
else
if [ "${7}" != "YES" ]
then
if [ "${7}" != "NO" ]
then
echo "Seventh argument: ${6}, is wrong. Must be: YES or NO"
exit
fi
fi
HUMID=${7}
fi
#
#   Set machine, Run time and Extention
#
COMPILE=${1}
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

HSTMAQ=`hostname`
MACHINE=una
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
QUEUE=PNT-EN
EXT=out
EXTL=S.unf
EXTR=R.unf
EXTZ=Z
echo ${MACHINE}
echo ${RUNTM}
echo ${EXT}
#
#   Set directories and remote machines
#
#   OPERM  is the directory for sources, scripts and printouts.
#   SOPERM is the directory for input and output files.
#   ROPERM is the directory for big selected output files.
#   IOPERM is the directory for the input files.
#   RMTCPR: is the remote machine to run the special products.
#   RMUSCF: is the remote archieve machine user.
#   DIRSCR: is the directory of the machine RMTCPR where there
#           are the scripts to create the archive directories.

#
PATHA=`pwd`
export FILEENV=`find ${PATHA} -name EnvironmentalVariablesMCGA -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} AVN
cd ${HOME_suite}/run

RMTCPR=una
RMUSCF=`whoami`
DIRSCR=/home/ensglob/scripts
echo ${OPERM}
echo ${SOPERM}
echo ${ROPERM}
echo ${IOPERM}
echo ${RCTROPERM}
export OPERM SOPERM ROPERM IOPERM 
export RMTCPR RMUSCF DIRSCR
#
#   Set truncation and layers
#
export TRC=`echo $2 | awk '{print $1/1}'`
export LV=`echo ${3} | awk '{print $1/1}'` 
export RESOL=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export NIVEL=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
export RESOL NIVEL
#
LABELI=${4}
NFDAYS=${5}
NUMPERT=${6}
export LABELI NFDAYS NUMPERT
#   Set final forecasting label and UTC Hour
caldaya ()
{
echo ${LABELI} > labeli.out
yi=`awk '{ print substr($1,1,4)/1 }' labeli.out`
mi=`awk '{ print substr($1,5,2)/1 }' labeli.out`
di=`awk '{ print substr($1,7,2)/1 }' labeli.out`
hi=`awk '{ print substr($1,9,2)/1 }' labeli.out`
rm -f labeli.out
let ybi=${yi}%4
if [ ${ybi} = 0 ]
then
declare -a md=( 31 29 31 30 31 30 31 31 30 31 30 31 )
else
declare -a md=( 31 28 31 30 31 30 31 31 30 31 30 31 )
fi
let df=${di}+1
let mf=${mi}
let yf=${yi}
let hf=${hi}+12
let n=${mi}-1
if [ ${hf} -gt 23 ]
then
let hf=${hf}-24
let df=${df}+1
fi
if [ ${df} -gt ${md[${n}]} ]
then
let df=${df}-${md[${n}]}
let mf=${mf}+1
if [ ${mf} -eq 13 ]
then
let mf=1
let yf=${yf}+1
fi
fi
if [ ${df} -lt 10 ]
then DF=0${df}
else DF=${df}
fi
if [ ${mf} -lt 10 ]
then MF=0${mf}
else MF=${mf}
fi
YF=${yf}
if [ ${hf} -lt 10 ]
then HF=0${hf}
else HF=${hf}
fi
}
#
caldaya
LABELF=${YF}${MF}${DF}${HF}
LABELS=${yi}${mi}${di}
export LABELF LABELS

calday ()
{
echo ${LABELI} > labelf${PERR}.out
yi=`awk '{ print substr($1,1,4)/1 }' labelf.out`
mi=`awk '{ print substr($1,5,2)/1 }' labelf.out`
di=`awk '{ print substr($1,7,2)/1 }' labelf.out`
hi=`awk '{ print substr($1,9,2)/1 }' labelf.out`
rm -f labelf.out
let ybi=${yi}%4
if [ ${ybi} = 0 ]
then
declare -a md=( 31 29 31 30 31 30 31 31 30 31 30 31 )
else
declare -a md=( 31 28 31 30 31 30 31 31 30 31 30 31 )
fi
let df=${di}+${NFDAYS}
let mf=${mi}
let yf=${yi}
let hf=${hi}
let n=${mi}-1
if [ ${df} -gt ${md[${n}]} ]
then
let df=${df}-${md[${n}]}
let mf=${mf}+1
if [ ${mf} -eq 13 ]
then
let mf=1
let yf=${yf}+1
fi
fi
if [ ${df} -lt 10 ]
then DF=0${df}
else DF=${df}
fi
if [ ${mf} -lt 10 ]
then MF=0${mf}
else MF=${mf}
fi
YF=${yf}
if [ ${hf} -lt 10 ]
then HF=0${hf}
else HF=${hf}
fi
}
#
calday 

LABELF2=${YF}${MF}${DF}${HF}
export LABELF2
cd ${OPERM}/run
#
#   Create the archive directories
#
. makedir1.scr ${TRC} ${LV} ${LABELI} > ${OPERM}/run/setout/setmakedir${RESOL}${NIVEL}.${MACHINE}.${RUNTM}.${EXT}

cat <<EOT0 > ${OPERM}/run/setense${RESOL}${NIVEL}.${MACHINE}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:        setense${RESOL}${NIVEL}.${MACHINE}        #
#                                                            #
#     Function:    This script file is used to set the       #
#                  environmental variables and start the     #
#                  ensemble scripts.                         #
#                                                            #
#************************************************************#
#
#  At SX6 Both the output (stdout) and the error
#  messages (stderr) are written to the same file
#
#PBS -o ${HSTMAQ}:${OPERM}/run/setout/setense${RESOL}${NIVEL}.${MACHINE}.${RUNTM}.${EXT}
#PBS -j oe
#PBS -l walltime=4:00:00
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N ENSE${NUM}
#

cd ${OPERM}/run
echo ${OPERM}/run/runPre ${TRC} ${LV}  ${LABELI} AVN 0
${OPERM}/run/runPre ${TRC} ${LV}  ${LABELI} AVN 0 
sleep 30
echo ${OPERM}/run/runpntg.una ${TRC} ${LV} ${LABELI} ${NFDAYS} AVN ${NUMPERT} sstwkl
nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} AVN ${NUMPERT} sstwkl      1> ${OPERM}/run/runpntg.out  2> ${OPERM}/run/errrunpntg.out &
sleep 30
echo ${OPERM}/run/runense.bash ${COMPILE} ${TRC} ${LV} ${LABELI} ${NFDAYS} ${NUMPERT} ${HUMID} "AVN"
${OPERM}/run/runense.bash ${COMPILE} ${TRC} ${LV} ${LABELI} ${NFDAYS} ${NUMPERT} ${HUMID} "AVN"
##########################################CHECK###################################################
itt=0
while [ \${itt} -lt 14 ];do
cc=0
i=1
while [ \${i} -le ${NUMPERT} ]
do
if [ \${i} -le 9 ];then
if [  -s  ${DK_suite}/model/datain/GANL0\${i}R${LABELI}S.unf.${CASE}  ] && [  -s ${DK_suite}/model/datain/GANLAVN${LABELI}R.unf.${CASE}  ];then
let cc=cc+2
fi
else
if [  -s  ${DK_suite}/model/datain/GANL\${i}R${LABELI}S.unf.${CASE}  ] && [  -s ${DK_suite}/model/datain/GANLAVN${LABELI}R.unf.${CASE}  ];then
let cc=cc+2
fi
fi
let i=i+1
done
let itt=cc
echo \${itt}
sleep 10
done
##########################################CHECK###################################################
sleep 60
echo ${OPERM}/run/runctrmodgpro.bash 120 24  ENSMODCTR ${TRC} ${LV} ${LABELI} ${LABELF} CTR hold
${OPERM}/run/runctrmodgpro.bash 120 24 ENSMODCTR ${TRC} ${LV} ${LABELI} ${LABELF} CTR hold
sleep 30
echo ${OPERM}/run/runctrreco.bash ${COMPILE} ${TRC} ${LV} ${LABELI}  CTR
${OPERM}/run/runctrreco.bash ${COMPILE} ${TRC} ${LV} ${LABELI}  CTR
export PBS_SERVER=sdb

 sleep 30
i=1
PT=R
while [ \${i} -le ${NUMPERT} ]
do
if [ \${i} -le 9 ]
then
#
# Script runperpntg run the members of the ensemble
#
if [ ! -s  ${DK_suite}/model/datain/GANL0\${i}N${LABELI}S.unf.${CASE} ] || [ ! -s ${DK_suite}/model/datain/GANL0\${i}P${LABELI}S.unf.${CASE} ]
then
echo "${OPERM}/run/runperpntg.bash ${COMPILE} ${TRC} ${LV} 0\${i} \${PT} ${LABELI} ${LABELF} ${NFDAYS} ${HUMID} ${NUMPERT} sstwkl "
nohup ${OPERM}/run/runperpntg.bash ${COMPILE} ${TRC} ${LV} 0\${i} \${PT} ${LABELI} ${LABELF} ${NFDAYS} ${HUMID} ${NUMPERT} sstwkl      1> ${OPERM}/run/runperpntg0\${i}.out  2> ${OPERM}/run/runperpntg0\${i}.err &
fi

else 
#
# Script runperpntg run the members of the ensemble
#
if [ ! -s  ${DK_suite}/model/datain/GANL\${i}N${LABELI}S.unf.${CASE} ] || [ ! -s ${DK_suite}/model/datain/GANL\${i}P${LABELI}S.unf.${CASE} ]
then
echo " ${OPERM}/run/runperpntg.bash ${COMPILE} ${TRC} ${LV} \${i} \${PT} ${LABELI} ${LABELF} ${NFDAYS} ${HUMID} ${NUMPERT} sstwkl "
nohup  ${OPERM}/run/runperpntg.bash ${COMPILE} ${TRC} ${LV} \${i} \${PT} ${LABELI} ${LABELF} ${NFDAYS} ${HUMID} ${NUMPERT} sstwkl      1> ${OPERM}/run/runperpntg\${i}.out  2> ${OPERM}/run/runperpntg\${i}.err &
fi

fi
echo \${i}
let i=i+1
done
##########################################CHECK###################################################
#----
itt=0
while [ \${itt} -lt 14 ];do
cc=0
i=1
while [ \${i} -le ${NUMPERT} ]
do
if [ \${i} -le 9 ];then
if [  -s  ${DK_suite}/model/datain/GANL0\${i}N${LABELI}S.unf.${CASE}  ] && [  -s ${DK_suite}/model/datain/GANL0\${i}P${LABELI}S.unf.${CASE}  ];then
let cc=cc+2
fi
else
if [  -s  ${DK_suite}/model/datain/GANL\${i}N${LABELI}S.unf.${CASE}  ] && [  -s ${DK_suite}/model/datain/GANL\${i}P${LABELI}S.unf.${CASE}  ];then
let cc=cc+2
fi
fi
let i=i+1
done
let itt=cc
echo \${itt}
sleep 10
done
##########################################CHECK###################################################

i=1
while [ \${i} -le ${NUMPERT} ]
do
if [ \${i} -le 9 ];then
echo "nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} 0\${i}P ${NUMPERT} sstwkl"> ${OPERM}/run/runpntg0\${i}P.out 
nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} 0\${i}P ${NUMPERT} sstwkl      1> ${OPERM}/run/runpntg0\${i}P.out  2> ${OPERM}/run/errrunpntg0\${i}P.err &
echo "nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} 0\${i}N ${NUMPERT} sstwkl"> ${OPERM}/run/runpntg0\${i}N.out 
nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} 0\${i}N ${NUMPERT} sstwkl      1> ${OPERM}/run/runpntg0\${i}N.out  2> ${OPERM}/run/errrunpntg0\${i}N.err &
else
echo "nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} \${i}P ${NUMPERT} sstwkl" > ${OPERM}/run/runpntg\${i}P.out 
nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} \${i}P ${NUMPERT} sstwkl       1> ${OPERM}/run/runpntg\${i}P.out  2> ${OPERM}/run/errrunpntg0\${i}P.err &
echo "nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} \${i}N ${NUMPERT} sstwkl"> ${OPERM}/run/runpntg\${i}N.out 
nohup ${OPERM}/run/runpntg.bash ${TRC} ${LV} ${LABELI} ${NFDAYS} \${i}N ${NUMPERT} sstwkl       1> ${OPERM}/run/runpntg\${i}N.out  2> ${OPERM}/run/errrunpntg0\${i}N.err &
fi
echo \${i}
let i=i+1
done
##########################################CHECK###################################################

itt=0
while [ \${itt} -lt 14 ];do
cc=0
i=1
while [ \${i} -le ${NUMPERT} ]
do
if [ \${i} -le 9 ];then
if [  -s  ${DK_suite2}/pos/dataout/${CASE}/${LABELI}/GPOS0\${i}N${LABELI}${LABELF2}P.fct.${CASE}.lst  ] && [  -s ${DK_suite2}/pos/dataout/${CASE}/${LABELI}/GPOS0\${i}P${LABELI}${LABELF2}P.fct.${CASE}.lst  ];then
let cc=cc+2
fi
else
if [  -s  ${DK_suite2}/pos/dataout/${CASE}/${LABELI}/GPOS\${i}N${LABELI}${LABELF2}P.fct.${CASE}.lst  ] && [  -s ${DK_suite2}/pos/dataout/${CASE}/${LABELI}/GPOS\${i}P${LABELI}${LABELF2}P.fct.${CASE}.lst  ];then
let cc=cc+2
fi
fi
let i=i+1
done
let itt=cc
echo \${itt}
sleep 10
done
##########################################CHECK###################################################

nohup ${OPERM}/run/runensmed.bash  ${COMPILE} ${TRC} ${LV} ${LABELI} ${NFDAYS}  ${NUMPERT}   AVN   1> ${OPERM}/run/runensmed${LABELI}.out  2> ${OPERM}/run/runensmed${LABELI}.err &

EOT0
#
echo "chmod 744  setense${RESOL}${NIVEL}.${MACHINE}"
chmod 744  ${OPERM}/run/setense${RESOL}${NIVEL}.${MACHINE}
#
#   Submit setense${RESOL}${NIVEL}.${MACHINE} to NQS ${QUEUE}
#
echo "qsub  -q ${QUEUE} ${OPERM}/run/setense${RESOL}${NIVEL}.${MACHINE}"
#qsub -q ${QUEUE} ${OPERM}/run/setense${RESOL}${NIVEL}.${MACHINE}
${OPERM}/run/setense${RESOL}${NIVEL}.${MACHINE} 
#iyear=2004
#imonth=3
#iday=26
#while [ ${iday} -eq 26 ];do
#ihrs=0
#while [ ${ihrs} -eq 00 ];do
#DATE=`echo ${iyear} ${imonth}  ${iday} |awk '{ printf("%4.4d%2.2d%2.2d\n",$1, $2 ,$3)  }'  `
#HH=`echo ${ihrs}|awk '{ printf("%2.2d\n",$1)  }'  `
#echo ${DATE}   ${HH}  
#
#LABELI=`../utils/bin/caldate.3.0.2 $DATE${HH} -  00h "yyyymmddhh"`
#LABELF=`../utils/bin/caldate.3.0.2 $DATE${HH} + 360h "yyyymmddhh"`
#
#. runPre 042 28 ${DATE}${HH} NMC
#. runpntg.bash 42 28 ${LABELI} 15 NMC 07 sstwkl
#. runPos 4 4 ENSPost 042 28 ${LABELI} ${LABELF} hold
#. runense1.una 42 28 ${LABELI} 15 07 NO NMC
#.  runctrmodgpro.una 8 4 ENSMODCTR 42 28 ${LABELI} ${LABELF} CTR hold
#. runctrreco1.una  42 28 ${LABELI}  NMC
#. runperpntg1.una 42 28  $PERT R ${LABELI} ${LABELF} 15 NO 07 sstwkl
#
#let ihrs=${ihrs}+6
#done
#let iday=${iday}+1
#done

#   runPre
#   
#   runpntg.bash ________ runModel
#                    |
#                    |___ runPos__runrectigge.1.4.ksh___vescptigge.ksh
#                    |
#                    |___ runGrh
#
#
#
#   runense1.una____ __makedir1.scr
#                   |
#                   |__runrecanl1.una
#                   |
#                   |__runrdpt1.una
#                   |
#                   |__rundrpt1.una
#
#
#   runctrmodgpro.una
# 
#   runctrreco1.una___runrecfct1.una
#   
#   runperpntg1.una___runModel
#                    |
#                    |_runperreco1.una___runrecfct.una
#                                        |
#                                        |__runeofs.una____rundeco.una
#   
#   runpntg.bash ________ runModel
#    		     |
#    		     |___ runPos__runrectigge.1.4.ksh___vescptigge.ksh
#    		     |
#    		     |___ runGrh  
#
#
#
#
#
#   runplumes.sx6
