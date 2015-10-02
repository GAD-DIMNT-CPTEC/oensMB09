#!/bin/ksh -x
#help#
#************************************************************************************#
#                                                                                    #
# script to run CPTEC Post-processing on PC Clusters under MPI Scali                 #
# and Sun Grid Engine without OpenMP                                                 #
#                                                                                    #
# assumptions: assume present but NOT at the same directory:                         #
#              $FEXE/PostGrib (Post-processing Executable file)                      #
#              $FSCR/POSTIN-GRIB (Post-processing input Namelist file)               #
#                                                                                    #
# usage: run_post_UNA.sh cpu_mpi  cpu_node name TRC LV LABELI LABELF name ps NMC hold #
# where:                                                                             #
# cpu_mpi: integer, the desired number of mpi processes                              #
# cpu_node: integer, the desired number of mpi processes per shared memory node      #
#************************************************************************************#
#help#
#
#       Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
fi
RES=$3
if [ -z "${4}" ]
then
  echo "TRC is not set" 
  exit 2
else
  TRC=`echo ${4} | awk '{print $1/1}'`   
fi
if [ -z "${5}" ]
then
  echo "LV is not set" 
  exit 2
else
  LV=`echo ${5} | awk '{print $1/1}'`    
fi

if [ -z "${6}" ]
then
  echo "LABELI is not set" 
  exit 3
else
  export LABELI=${6}  
fi
if [ -z "${7}" ]
then
  echo "LABELF is not set" 
  exit 3
else
  export LABELF=${7}  
fi
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
if [ ${#CASE} -ne 10 ]; then
  cat < ${0} | sed -n '/^#help#/,/^#help#/p'
  exit 1
else
#
# SETTING THE APPROPRIATED ENVIRONMENT
#
set -x
  PATHA=${HOME_suite}
  export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`
  export PATHENV=`dirname ${FILEENV}`
. ${FILEENV} ${CASE}  
  cd ${HOME_suite}/run
set +x
fi

echo "`echo ${0}`: Start execution at `date`"
set -x
LABELS=`echo $LABELI | cut -c 1-8`
HH=`echo $LABELI | cut -c 9-10`
LABELP=`date -d "$LABELS ${HH}:00 12 hours ago" +"%Y%m%d%H"`

SCRIPTFILEPATH=${HOME_suite}/run/mapas${DIRRESOL}.${MAQUI}
tmstp=$(date +"%s")
cat << _EOF > ${SCRIPTFILEPATH}.grads
#!/bin/bash -x
#PBS -W umask=026
#PBS -o ${SC2_suite}/out_err/${TRCLV}/$LABELI/mapas.out
#PBS -e ${SC2_suite}/out_err/${TRCLV}/$LABELI/mapas.err
#PBS -lselect=1:ncpus=1
#PBS -q $QUEUE1
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES

umask 026
ti=\$(date +"%s %Y/%m/%d %H:%M:%S") # TEMPO INICIAL

cd ${HOME_suite}/produtos/mapas/scripts
echo run ${HOME_suite}/produtos/mapas/scripts/Mapa1.sx6.gs
echo ${LABELI} ${LABELP} 1 7 ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
 
${GRADSB}/grads -bp << EOF
run ${HOME_suite}/produtos/mapas/scripts/Mapa1.sx6.gs
${LABELI} ${LABELP} 1 7 ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
EOF
mkdir -p  ${SC2_suite}/produtos/mapas/${TRCLV}/${LABELI}/
mv *png ${SC2_suite}/produtos/mapas/${TRCLV}/${LABELI}/

${GRADSB}/grads -bp  << EOF
run Mapa1.sx6.gs
${LABELI} ${LABELP} 2 7 ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
EOF
mv *png ${SC2_suite}/produtos/mapas/${TRCLV}/${LABELI}/

${GRADSB}/grads -bl  << EOF
run Mapa1.sx6.gs
${LABELI} ${LABELP} 3 7 ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
EOF
mv *png ${SC2_suite}/produtos/mapas/${TRCLV}/${LABELI}/

${GRADSB}/grads -bp  << EOF
run Mapa1.sx6.gs
${LABELI} ${LABELP} 4 7 ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl ${SC2_suite}/pos/dataout/${TRCLV}/${LABELP}/GPOSNMC${LABELP}.ctl
EOF
mv *png ${SC2_suite}/produtos/mapas/${TRCLV}/${LABELI}/

${GRADSB}/grads -bp << EOF
run Mapa2.sx6.gs
${LABELI} 4 7  ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl  ${SC2_suite}/pos/dataout/${TRCLV}/${LABELI}/GPOSNMC${LABELI}.ctl
quit
EOF
mv *png ${SC2_suite}/produtos/mapas/${TRCLV}/${LABELI}/

rm -f ${HOME_suite}/produtos/mapas/scripts/gpos* \

      ${HOME_suite}/produtos/mapas/scripts/GPOS*
mkdir -p ${CTRL_HOME}/${CASE}/${LABELI:0:6}
date +"${HOME_suite}/run/runMap @ aux 1 @ ini \$ti @ fim %s %Y/%m/%d %H:%M:%S" >> ${CTRL_HOME}/${CASE}/${LABELI:0:6}/${LABELI:6:4}.txt
_EOF

export PBS_SERVER=aux20-eth4 # PARA RODAR NOS NOS DE PROCESSAMENTO AUXILIAR
SBJ=$(qsub ${SCRIPTFILEPATH}.grads | cut -d. -f1)
#
it=2
while [ ${it} -gt 0 ];do
	it=0$(qstat | grep $SBJ | wc -l)
	sleep 30
done


exit 0
