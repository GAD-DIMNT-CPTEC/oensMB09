#!/bin/bash -x 
#help#
#******************************************************************#
#                                                                  #
#     Name:           runperreco.sx6                               #
#                                                                  #
#     Function:    This script submits the                         #
#                  recomposition script.                           #
#                                                                  #                     
#     Date:           May       28th, 2003.                        #
#     Last change:    May       28th, 2003.                        #
#                                                                  #
#     Valid Arguments for runperreco.sx6                           #
#                                                                  #
#      First :    HELP: help or nothing for getting help           #
#      First : COMPILE: help, make, clean or run                   #
#      Second :    TRC: three-digit triangular truncation          #
#      Third:       LV: two-digit number of vertical sigma-layers  #
#      Fourth :    NUM: pertubation number                         #
#      Fifth:   LABELI: initial forecasting label                  #
#      Sixth :  NFDAYS: number of forecasting days                 #
#      Seventh : HUMID: YES or NO (humidity will be perturbed)     #
#      Eigth : NUMPERT: number of random perturbation              #
#                                                                  #
#             LABELx : yyyyddhh                                    #
#                      yyyy = four digit year                      #
#                        mm = two digit month                      #
#                        dd = two digit day                        #
#                        hh = two digit hour                       #
#                                                                  #
#******************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
cat < $0 | sed -n '/^#help#/,/^#help#/p'
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

if [ -z "${2}" ]
then
echo "Second argument is not set (TRC)"
exit
else
export TRC=`echo ${2} | awk '{print $1/1}'`
fi

if [ -z "${3}" ]
then
echo "Third argument is not set (LV)"
exit
else
export LV=`echo ${3} | awk '{print $1/1}'` 
fi

if [ -z "${4}" ]
then
echo "Fourth argument is not set: NUM"
exit
else
PERR=${4}
fi

if [ -z "${5}" ]
then
echo "Fifth argument is not set (LABELI: yyyymmddhh)"
exit
else
LABELI=${5}
fi

if [ -z "${6}" ]
then
echo "Sixth argument is not set: NFDAYS" 
exit
fi

if [ -z "${7}" ]
then
echo "Seventh argument is not set: HUMID" 
exit
else
HUMID=${7}
fi
if [ -z "${8}" ]
then
echo "Eigth argument is not set: NUMPERT" 
exit
else
NUMPERT=${8}
fi
#
#   Set machine, Run time and Extention
#
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname`
export COMPILE=${1}
MACHINE=una
QUEUE=Inter
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
echo ${MACHINE}
echo ${RUNTM}
echo ${EXT}
#
#   Set directories
#
#   OPERM  is the directory for sources, scripts and printouts.
#   SOPERM is the directory for input and output files.
#   ROPERM is the directory for big selected output files.
#   IOPERM is the directory for the input files.
#
PATHA=`pwd`
export FILEENV=`find ${PATHA} -name EnvironmentalVariablesMCGA -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} ${PERR}
cd ${HOME_suite}/run
echo ${OPERM}
echo ${SOPERM}
echo ${ROPERM}
echo ${IOPERM}
#
#   Set truncation and layers
#
export TRUNC=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export LEV=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
export RESOL=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export NIVEL=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
#
cd ${OPERM}/run
#
SCRIPTNAME=setperreco.1.${PERR}${RESOL}${NIVEL}.${LABELI}.${MACHINE}
export PBS_SERVER=aux20-eth4
mkdir -p ${ROPERM}/recfct/output
cat <<EOT0 > ${OPERM}/run/${SCRIPTNAME}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:      setperreco${PERR}${RESOL}${NIVEL}.${MACHINE}#
#                                                            #
#     Function:    This script file is used to set the       #
#                  environmental variables and start the     #
#                  recomposition script.                     #
#                                                            #
#************************************************************#
#
#PBS -o ${HSTMAQ}:${ROPERM}/recfct/output/setperreco.${LABELI}.${RUNTM}.${EXT}
#PBS -j oe
#PBS -l walltime=0:40:00
#############PBS -l mppwidth=1
#PBS -l mppnppn=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N ENSRRECO${NUM}
#PBS -q ${AUX_QUEUE}
export PBS_SERVER=aux20-eth4

# 
#   Set date (year,month,day) and hour (hour:minute) 
#
#   DATE=yyyymmdd
#   HOUR=hh:mn
#
DATE=`date +'%Y'``date +'%m'``date +'%d'`
HOUR=`date +'%H:%M'`
echo 'Date: '\${DATE}
echo 'Hour: '\${HOUR}
export DATE HOUR
#
NUM=${4}
#   Set labels (date, UTC hour, ...)
#
#   LABELI = yyyymmddhh
#   LABELI = input file start label
#
LABELI=${5}
echo \${LABELI}
export LABELI
NFDAYS=${6}
export NFDAYS
#
cd ${ROPERM}/model/dataout/${RESOL}${NIVEL}/\${LABELI}
rm -f GFCT\${NUM}R\${LABELI}\${LABELI}*
rm -f GFCT\${NUM}R\${LABELI}*.dir.${RESOL}${NIVEL}
rm -f GFCT\${NUM}R\${LABELI}*.dir.${RESOL}${NIVEL}.files

for arq in \`ls GFCT\${NUM}R\${LABELI}*F.fct.${RESOL}${NIVEL}\`
do
      ln -sf ${DK_suite2}/model/dataout/${RESOL}${NIVEL}/\${LABELI}/\$arq ${DK_suite}/model/datain/
done      
#
ext=R.fct.${RESOL}${NIVEL}
echo ${ext}
cd  ${IOPERM}
rm -f lst.GFCT\${NUM}R\${LABELI}.dr
#ls -1 GFCT\${NUM}R\${LABELI}*F.fct.${RESOL}${NIVEL}  > lst.GFCT\${NUM}R\${LABELI}
#ls -1 GFCT\${NUM}R\${LABELI}*F.fct.${RESOL}${NIVEL} > lst.GFCT\${NUM}R\${LABELI}.dr
#ls -1 GFCTCTR\${LABELI}*${RESOL}${NIVEL} > lst.GFCTCTR\${LABELI}
ls -1 GFCT\${NUM}R\${LABELI}*${RESOL}${NIVEL}  > lst.GFCT\${NUM}R\${LABELI}

#awk '{ print \$8 }' lst.GFCT\${NUM}R\${LABELI}.dr  >  ${DK_suite}/model/datain/lst.GFCT\${NUM}R\${LABELI}
#awk '{ for (i=1;i<=15;i++) { if(length($i) == 43 ) print $i }  }' lst.GFCT\${NUM}R\${LABELI}.dr  >  lst.GFCT\${NUM}R\${LABELI}
xx=\`awk '{ print substr(\$1,18,10) }' lst.GFCT\${NUM}R\${LABELI}\`
echo \${xx}
cd ${SOPERM}/eof/datain
\rm templ\${NUM}\${LABELI} 
it=1

for i in \${xx} 
do
cd ${SOPERM}/eof/datain
cat <<EOT1 >> ${SOPERM}/eof/datain/templ\${NUM}\${LABELI}
${ROPERM}/recfct/dataout/${RESOL}${NIVEL}/GFCTCTR\${LABELI}\${i}\${ext}
${ROPERM}/recfct/dataout/${RESOL}${NIVEL}/GFCT\${NUM}R\${LABELI}\${i}\${ext}
EOT1

ls -ltr ${SOPERM}/eof/datain/templ\${NUM}\${LABELI}

cd ${OPERM}/run
#for arq in \`ls ${ROPERM}/recfct/dataout/${RESOL}${NIVEL}/GFCT\${NUM}R\${LABELI}*.fct*\`
for arq in \`ls ${ROPERM}/recfct/dataout/${RESOL}${NIVEL}/GFCT\${NUM}R\${LABELI}\${i}*.fct*\`
do
      echo ln -sf \$arq ${ROPERM}/model/datain/
      ln -sf \$arq ${ROPERM}/model/datain/
done
export FIRST='     '
export SECOND='     '
export it
. ${OPERM}/run/runrecfct.${MACHINE} ${COMPILE} ${TRC} ${LV} FCT\${NUM}R \${LABELI} \${i}  \${NUM}R  hold
let it=${it}+1
export it
done
sleep 30
export FIRST='     '
export SECOND='     '
cd ${OPERM}/run
. ${OPERM}/run/runeofs_hn.${MACHINE} ${COMPILE} ${TRC} ${LV} ${HUMID} \${NUM} \${LABELI} \${NFDAYS} ${NUMPERT} hold
. ${OPERM}/run/runeofs_hs.${MACHINE} ${COMPILE} ${TRC} ${LV} ${HUMID} \${NUM} \${LABELI} \${NFDAYS} ${NUMPERT} hold
. ${OPERM}/run/runeofs_sas.${MACHINE} ${COMPILE} ${TRC} ${LV} ${HUMID} \${NUM} \${LABELI} \${NFDAYS} ${NUMPERT} hold
. ${OPERM}/run/runeofs_san.${MACHINE} ${COMPILE} ${TRC} ${LV} ${HUMID} \${NUM} \${LABELI} \${NFDAYS} ${NUMPERT} hold
. ${OPERM}/run/runeofs_tr.${MACHINE} ${COMPILE} ${TRC} ${LV} ${HUMID} \${NUM} \${LABELI} \${NFDAYS} ${NUMPERT} hold
let it=${it}+1
export it

#
EOT0
chmod 744  ${OPERM}/run/${SCRIPTNAME}
#AMM echo "qsub -x -s /bin/ksh -q ${QUEUE} ${OPERM}/run/${SCRIPTNAME}"
#AMM qsub -x -s /bin/ksh -q ${QUEUE} ${OPERM}/run/${SCRIPTNAME}
echo "${OPERM}/run/${SCRIPTNAME}"
${OPERM}/run/${SCRIPTNAME}
#
