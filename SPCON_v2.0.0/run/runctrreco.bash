#!/bin/bash -x
# runctrreco.bash TQ0126L028 CTR 2012111200

#help#
#*******************************************************************#
#                                                                   #
#     Name:           runctrreco1.una                               #
#                                                                   #
#     Function:       This script submits the                       #
#                     recomposition scripts                         #
#                     of control forecast.                          #
#                     It runs in Korn Shell.                        #
#                                                                   #
#      Date:           Sep    27th, 2011.                           #
#      Last change:    Sep    27th, 2011.                           #
#                                                                   #
#     Valid Arguments for runctrreco.sx6                            #
#                                                                   #
#     First  : COMPILE: help, make, clean or run                    #
#     Second :     TRC: three-digit triangular truncation           #
#     Third  :      LV: two-digit number of vertical sigma-layers   #
#     Fouth  :  LABELI: initial forecasting label                   #
#     Seventh:  PERT: NMC, AVN CTR 01N                              #
#                                                                   #
#              LABELx: yyyymmddhh                                   #
#                      yyyy = two digit year                        #
#                        mm = two digit month                       #
#                        dd = two digit day                         #
#                        hh = two digit UTC hour                    #
#                                                                   #
#*******************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
cat < $0 | sed -n '/^#help#/,/^#help#/p'
exit 0
fi

export FILEENV=`find ./ -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${1} ${2}
cd ${HOME_suite}/run

TRC=`echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0"`
LV=`echo ${TRCLV} | cut -c 7-11 | tr -d "L0"`
export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

if [ -z "${2}" ]; then
   echo "PERT: NMC, AVN CTR 01N  "
   exit
else
   PREFIC=${2}
fi

if [ -z "${3}" ]; then
   echo "Fifth argument is not set (LABELI: yyyymmddhh)"
   exit
else
   LABELI=${3}
fi
#
#   Set truncation and layers
#
#
#   Set machine, Run time and Extention
#
COMPILE=run
HSTMAQ=`hostname`
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out

cd ${HOME_suite}/run

#
cd ${HOME_suite}/run
export PBS_SERVER=aux20-eth4
#
mkdir -p ${SC2_suite}/model/exec/setout
SCRIPTSFILES=setctrreco.${PREFIC}.${RESOL}${NIVEL}.${LABELI}

cat <<EOT0 > ${HOME_suite}/run/${SCRIPTSFILES}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:        setctrreco${RESOL}${NIVEL}.${MACHINE}     #
#                                                            #
#     Function:    This script file is used to set the       #
#                  environmental variables and start the     #
#                  recomposition script.                     #
#                                                            #
#************************************************************#
#
#PBS -o ${SC2_suite}/model/exec/setout/setctrreco.${PREFIC}.${RESOL}${NIVEL}.${LABELI}.${MACHINE}.${RUNTM}.out
#PBS -e ${SC2_suite}/model/exec/setout/setctrreco.${PREFIC}.${RESOL}${NIVEL}.${LABELI}.${MACHINE}.${RUNTM}.err
#PBS -l walltime=0:05:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N CTRRECO_${PREFIC}
#PBS -q ${QUEUE1}
export PBS_SERVER=aux20-eth4

#
#
#   Set date (year,month,day) and hour (hour:minute) 
#
#   DATE=yyyymmdd
#   HOUR=hh:mn
#
DATE=`date +'%Y'``date +'%m'``date +'%d'`
HOUR=`date +'%H:%M'`
echo 'Date: '\$DATE
echo 'Hour: '\$HOUR
export DATE HOUR
#
#   Set labels (date, UTC hour, ...)
#
#   LABELI = yyyymmddhh
#   LABELI = input file start label
#
LABELI=${LABELI}
echo \${LABELI}
export LABELI
#
mkdir ${SC2_suite}/model/dataout/${RESOL}${NIVEL}/\${LABELI}

cd ${SC2_suite}/model/dataout/${RESOL}${NIVEL}/\${LABELI}
rm -f GFCTCTR\${LABELI}\${LABELI}*
rm -f GFCTCTR\${LABELI}*.dir.${RESOL}${NIVEL}
rm -f GFCTCTR\${LABELI}*.dir.${RESOL}${NIVEL}.files
#

cd ${HOME_suite}/run

if [ "${COMPILE}" != "run" ]; then
   echo ${HOME_suite}/run/runrecfct.${MACHINE} ${COMPILE} ${TRC} ${LV} FCTCTR \${LABELI} \${i} ${PREFIC}
   ${HOME_suite}/run/runrecfct.${MACHINE} ${COMPILE} ${TRC} ${LV} FCTCTR \${LABELI} \${i} ${PREFIC}
else 
   cd ${SC2_suite}/model/dataout/${RESOL}${NIVEL}/\${LABELI}/
   for i in \$(ls GFCTCTR\${LABELI}* | cut -c 18-27) ; do
      cd ${HOME_suite}/run
      echo ${HOME_suite}/run/runrecfct.bash ${TRCLV} ${PREFIC} FCT${PREFIC} \${LABELI} \${i}
      ${HOME_suite}/run/runrecfct.bash ${TRCLV} ${PREFIC} FCT${PREFIC} \${LABELI} \${i}
   done
fi
EOT0
chmod 744 ${HOME_suite}/run/${SCRIPTSFILES}


echo "qsub ${HOME_suite}/run/${SCRIPTSFILES}"

echo  ${HOME_suite}/run/${SCRIPTSFILES}
jobnumber=$(qsub ${HOME_suite}/run/${SCRIPTSFILES} | cut -d. -f1)
it=2
while [ ${it} -gt 0 ];do
   it=`qstat | grep $jobnumber | wc -l`
   sleep 30
done

exit 0
