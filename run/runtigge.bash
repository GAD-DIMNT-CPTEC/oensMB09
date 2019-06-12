#!/bin/bash -x
#help#
#*******************************************************************#
#                                                                   #
#     Name:           runrectigger.1.4.una                          #
#     runrectigger.1.4.una 42 28 2004032600  NMC 7                  #
#                                                                   #
#     Function:       This script generated and submit              #
#                     a script to start the global model            #
#                     at a given date/time to integration           #
#                     from initial date till final date.            #
#                                                                   #
#     Date:           June   02th, 2003.                            #
#     Last change:    June   02th, 2003.                            #
#                                                                   #
#     Valid Arguments for runpntg.sx6                               #
#                                                                   #
#     First :   HELP: help or nothing for getting help              #
#     First :    TRC: three-digit triangular truncation             #
#     Second:     LV: two-digit number of vertical sigma-layers     #
#     Third : LABELI: initial forecasting label                     #
#     PREFX:   PREFX: preffix for name of input files               #
#     Fifth :   PERT: perturbation                                  #
#                                                                   #
#             PERT  : NMC,01P,01N,02P,02N,...                       #
#             LABELx: yyyymmddhh                                    #
#                     yyyy = four digit year                        #
#                       mm = two digit month                        #
#                       dd = two digit day                          #
#                       hh = two digit UTC hour                     #
#*******************************************************************#
#help#
#
# Help:
#
if [ "${1}" = "help" -o -z "${1}" ]
then
    cat < ${0} | sed -n '/^#help#/,/^#help#/p'
    exit 0
fi

#
#       Testing Valid Arguments
#
if [ -z "${1}" ]
then
TRC=62
else
export TRC=`echo $1 | awk '{print $1/1}'`
fi
if [ -z "${2}" ]
then
LV=28
else
export LV=`echo $2 | awk '{print $1/1}'` 
fi
if [ -z "${3}" ]
then
LABELI="NMC"
exit
else
export LABELI=${3}
fi


if [ -z "${4}" ]
then
PREFX="NMC"
exit
else
export PREFX=${4}
fi

#
#   Set host, machine, NQS Queue, Run time and Extention
#
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname` # example una2
RUNTM=`date +'%Y%m%d%H:%M'`
EXT=out
export HSTMAQ MAQUI RUNTM EXT
#
#   Set Perturbation
#
PERT=`echo ${5} |awk '{ printf("%3.3s\n",$1)  }' `
export PERT
#
#   Set directories and remote machines
#
#   OPERM : is the directory for sources, scripts and printouts 
#           at SX6.
#   SOPERM: is the directory for input and output files at SX6.
#   ROPERM: is the directory for big selected output files at SX6.
#   CTLDIR: is the directory where the outputs will be available
#           after they where send to the machine RMTCPF.
#   DIRPRD: is the directory of the machine RMTCPR where there
#           are the programs to generate special products.
#   RMTUSR: is the remote user at the telecom machine RMTCPR.
#   RMTCPR: is the remote machine to run the special products.
#   RMTCPF: is the remote archieve machine.
#   RMUSCF: is the remote archieve machine user.
#   RMPWCF: is the remote archieve machine password.
#   RMTCPY: is the remote products machine for external users.
#
#
PATHA=`pwd`
export FILEENV=`find ${PATHA} -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} CTR
cd ${HOME_suite}/run

#
#   Set Horizontal Truncation (TRUNC) and Vertical Layers (LEV)
#
export TRUNC=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export LEV=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `

LABELI=$3

if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO. runmodgmpi.sx6 YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO. runmodgmpi.sx6 YYYYMMDDHH"
            exit 2
      else
            YYYY=`echo $LABELI |cut -c 1-4`
            MM=`echo $LABELI |cut -c 5-6`
            DD=`echo $LABELI |cut -c 7-8`
            HH=`echo $LABELI |cut -c 9-10`

            LABELF=`date -d "${NFDAYS} day ${YYYY}${MM}${DD}" +"%Y%m%d${HH}"`
            YYYYF=`echo $LABELF |cut -c 1-4`
            MMF=`echo $LABELF |cut -c 5-6`
            DDF=`echo $LABELF |cut -c 7-8`
            HHF=`echo $LABELF |cut -c 9-10`
      fi
fi

if [ -s $PREFX ]; then
      echo "ERRO - PARAMETRO PERT FORMATO: runrectigge.sx6 yyyymmddhh 01N"
      exit 2
fi
if [ "${PREFX}" = "NMC" ]; then
      membnum=000
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "AVN" -o "${PREFX}" = "NMC" ]; then
      membnum=000
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "01N" ]; then
      membnum=001
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "02N" ]; then
      membnum=002
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "03N" ]; then
      membnum=003
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "04N" ]; then
      membnum=004
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "05N" ]; then
      membnum=005
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "06N" ]; then
      membnum=006
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "07N" ]; then
      membnum=007
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "01P" ]; then
      membnum=008
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "02P" ]; then
      membnum=009
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "03P" ]; then
      membnum=010
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "04P" ]; then
      membnum=011
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "05P" ]; then
      membnum=012
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "06P" ]; then
      membnum=013
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi
if [ "${PREFX}" = "07P" ]; then
      membnum=014
      mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}
fi

PREFX=$4

#. smslabel Info "Aguardando Pos-processados: $LABELI" > /dev/null

REQ="D"
HH=`echo $LABELI | cut -c 9-10`
LABELS=`echo $LABELI | cut -c 1-8`
LABELF=`date -d "${LABELS} 15 days" +"%Y%m%d${HH}"`

export GRIB_DEFINITION_PATH=${HOME_suite}/tigge/grib_api/definitions
export GRIB_TEMPLATES_PATH=${HOME_suite}/tigge/grib_api/samples
export GRIB_SAMPLES_PATH=${HOME_suite}/tigge/grib_api/samples
GRADS=${GRADSB}
WGRIB18=wgrib
ARQINI=z_tigge_c_sbsj_${LABELI}0000_glob
DIRINI=sbsj_${LABELI}00_glob_prod

# EXECUTAVIES DO TIGGE
#CONVERTER - converte de grib para grib2
#TIGGENAME - converte o nome do grib2 para o nome de convencao do TIGGE
#TIGGECHECK - Verifica se o arquivo gerado esta correto

CONVERTER=${SC1_suite}/tigge/bin/grib_convert
TIGGENAME=${SC1_suite}/tigge/bin/tigge_name
TIGGECHECK=${SC1_suite}/tigge/bin/tigge_check
CONVERTER=/stornext/home/modoper/oens_MCGA/tigge/TMP/grib_api-1.0.0/tools/grib_convert
TIGGENAME=/stornext/home/modoper/oens_MCGA/tigge/TMP/grib_api-1.0.0/tigge/tigge_name
TIGGECHECK=/stornext/home/modoper/oens_MCGA/tigge/TMP/grib_api-1.0.0/tigge/tigge_check
GRIBSET=/stornext/home/modoper/oens_MCGA/tigge/TMP/grib_api-1.0.0/tools/grib_set

tiggedirmopora=/usr/local/ldm/tigge/data
tiggescratch=${tiggedirmopora}/scratch/${DIRINI}
tiggeoutgoing=${tiggedirmopora}/outgoing

set +x
mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}

cd ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}

echo PWD=`pwd`

# PEGA TODOS OS ARQUIVOS DO .lst PARA RECORTE
echo "GERANDO RECORTES..."

#AAF COMENTADO PARA FAZER O LOOP FIXO for arq in `cat ${ARQLST} | grep -v inz`
#AAF COMENTADO PARA FAZER O LOOP FIXO do            

rm -f ${SC1_suite}/tigge/bin/ens.${PREFX}.list
rm -Rf   ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${PREFX}
mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${PREFX}

# REMOVE DIRETORIO COM ARQUIVOS tigge_VARIAVEL*.grib
rm -Rf   ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}
mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}

 set -x
nd=0; p=0
while [ $nd -le 360 ]; do # VARIA DE 00 a 360 horas de previsao para calcular a data de previsao do arquivo
#      labelfct=`${CALDATE} ${LABELI} + ${nd}h "yyyymmddhh"` #calcula a data de previsao do arquivo
      labelddate=`echo $LABELI | cut -c 1-8`
      hhddate=`echo $LABELI | cut -c 9-10`
      labelfct=`date -d "$labelddate ${hhddate}:00 ${nd} hours" +"%Y%m%d%H"` #calcula a data de previsao do arquivo
      
      if [ ${LABELI} -eq ${labelfct} ]; then
            icnfct=icn
            tarq=`ls -ltrH --full-time ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/GPOS${PREFX}${LABELI}${LABELI}*icn*grb | grep -v "inz" | grep -v "~l" | awk '{print $5}'` # capturaa o tamanho do arquivo
      else
            icnfct=fct
            tarq=`ls -ltrH --full-time ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/GPOS${PREFX}${LABELI}${labelfct}*grb | grep -v "inz" | grep -v "~l" | awk '{print $5}'` # capturaa o tamanho do arquivo
      fi
      if [ $LABELI -ne $labelfct ]; then # verifica a data para pegar o tamnha dos icn e fct, que sao diferentes
            taor=296424 #tamanho fct
            fct=fct
      else
            taor=266781 #tamanho icn
            fct=icn
      fi
      if [ $tarq -ge ${taor} ]; then # verifica se o tamanho do arquivo estah correto, se estiver gera os arquivos do tigge para o arquivo de previsao
                                     # a cada arquivo pos-processado, serah verificado o tamanho e caso esteja correto, comeca os recortes z_tigge*
            arq=`ls -ltrH --full-time ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/GPOS${PREFX}${LABELI}${labelfct}*${icnfct}*.grb | grep -v "inz" | awk '{print $9}'` #captura o nome do arquivo pos-processado
            echo ${nd} - ${labelfct} - $arq #imprime parametros para controle (pode-se comentar a linha)
            arqens=`basename $arq`
            mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}
            ln -vsf $arq ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}/$arqens
             echo " "
      fi
      
cat <<EOF>> ${SC1_suite}/tigge/bin/ens.${PREFX}.list
$arqens
EOF
      echo "${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/GPOS${PREFX}${LABELI}${labelfct}P.${fct}.${TRUNC}${LEV}.grb -a $tarq -ge ${taor} "
      if [ \( -L ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/GPOS${PREFX}${LABELI}${labelfct}P.${fct}.${TRUNC}${LEV}.grb \) -a \( $tarq -ge ${taor} \) ]; then
            let nd=${nd}+6 #CONTINUACAO DO LOOP DE ARQUIVOS (IF TAMANHO DE ARQUIVO OK INCREMENTA DATA DE PREVISAO DO ARQUIVO)
      else
            echo "AGUARDANDO ARQUIVO ${SC2_suite}/pos/dataout/${TRUNC}${LEV}/${LABELI}/GPOS${PREFX}${LABELI}${labelfct}P.${fct}.${TRUNC}${LEV}.grb"
            sleep 63
      fi
done #FIM DO LOOP DE ARQUIVOS

echo "ARQUIVOS ENCONTRADOS, INICIANDO RECORTES..."
cpu_mpi=1
cpu_node=1
num=$(($cpu_mpi+$cpu_node-1))
fra=$(($num/$cpu_node))
cpu_tot=$(($fra*$cpu_node))

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${PREFX}.${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

tmstp=$(date +"%s")

echo ${SCRIPTFILENAME}

cat <<EOF_> ${SCRIPTFILENAME}
#!/bin/bash -x
#PBS -W umask=026
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -l select=1:ncpus=1
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -V
#PBS -N TIGGE${PREFX}
#

export GRIB_DEFINITION_PATH=${HOME_suite}/tigge/share/grib_api/definitions
export GRIB_TEMPLATES_PATH=${HOME_suite}/tigge/share/grib_api/templates
export GRIB_SAMPLES_PATH=${HOME_suite}/tigge/grib_api/samples

mkdir -p ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}
cd ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}
#
ulimit

${SC1_suite}/tigge/bin/tigge.x < ${SC1_suite}/tigge/bin/ens.${PREFX}.list

set -x
hhhtemp=000

rm -f GPOS${PREFX}${LABELI}*grb 

echo "ENTRANDO EM: ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}"
cd ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}

echo "ENTRANDO EM: ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}"
cd ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}

# REMOVE ARQUIVOS QUE NAO SAO PROCESSADOS
find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX} -name "tigge_str*" -exec rm -vf {} \;
find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX} -name "tigge_ttr*" -exec rm -vf {} \;
find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX} -name "tigge_slhf*" -exec rm -vf {} \;
find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX} -name "tigge_sshf*" -exec rm -vf {} \;
find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX} -name "tigge_sm*" -exec rm -vf {} \;
if [ $membnum -ne 0000 ]; then
   find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX} -name "tigge_lsm*" -exec rm -vf {} \;
   find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX} -name "tigge_oro*" -exec rm -vf {} \;
fi

echo "\nCONVERTENDO PARA GRIB2 e RENOMEANDO\n"
hhhssr=006
#for arqgrib1 in \$(ls -l ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}/ | awk '{print \$9}' | grep -i tigge ); do
for arqgrib1 in \$(find ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}/ -name "tigge*.grb" ); do

      export GRIB_DEFINITION_PATH=${HOME_suite}/tigge/share/grib_api/definitions
      export GRIB_TEMPLATES_PATH=${HOME_suite}/tigge/share/grib_api/templates
      export GRIB_SAMPLES_PATH=${HOME_suite}/tigge/share/grib_api/samples

      echo " "
      time ${CONVERTER} ${HOME_suite}/tigge/rules/cptec.rules_grib_final.prod.${TRUNC}${LEV} \${arqgrib1} \${arqgrib1}.${membnum}.2
      echo ${TIGGENAME} \${arqgrib1}.${membnum}.2 | grep "CORRECT" | cut -d: -f2
      tiggename=\$(${TIGGENAME} \${arqgrib1}.${membnum}.2 | grep "CORRECT" | cut -d: -f2)
      tiggename=\$(echo \$tiggename | sed -e s%"c_46"%"c_sbsj"%g)
      tiggeconf=\$(echo \$tiggename | cut -c 52-54)

      if [ \$(echo \$tiggename | egrep "_lsm" | wc -l) -ge 1 ]; then
         echo "PROCESSANDO LSM - \$tiggename"
      fi
      
      echo  mv \${arqgrib1}.${membnum}.2 ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename
      mv \${arqgrib1}.${membnum}.2 ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename

      if [ \$(echo \$tiggename | egrep "_ttr|_str|_ssr|_tp|_sf" | wc -l) -ge 1 ]; then
         echo "Arrumando acumulado de tp, ssr e sf"
         hhr_new=\$(echo \$tiggename | cut -d_ -f10)
         $GRIBSET -s marsStartStep=0,marsEndStep=\$hhr_new ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\${tiggename}.2
         mv ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\${tiggename}.2 ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename
      fi
      
      if [ $membnum -eq 000 ]; then
         $GRIBSET -s typeOfProcessedData=3 ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\${tiggename}.2
         mv ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\${tiggename}.2 ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename
      else
         $GRIBSET -s typeOfProcessedData=4 ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\${tiggename}.2
         mv ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\${tiggename}.2 ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename
      fi
      
      ${TIGGECHECK} ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename
      if [ \$? -ne 0 ]; then
            echo "ERRO - ${TIGGECHECK} ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename"
            #rm -f ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/\$tiggename
      fi
      
done

cd ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/

tt=pf
#rm -f ${ARQINI}_prod_??_??_????_???_*_ttr.grib
#rm -f ${ARQINI}_prod_??_??_????_???_*_str.grib
#rm -f ${ARQINI}_prod_??_??_????_???_*_slhf.grib
#rm -f ${ARQINI}_prod_??_??_????_???_*_sshf.grib
rm -f ${ARQINI}_prod_??_??_????_???_*_sm.grib
rm -f ${ARQINI}_prod_??_??_????_???_*_bld.grib

if [ $membnum -ne 0000 ]; then
NARQTIN=3228
#NARQTIN=0
else
NARQTIN=3350
#NARQTIN=0
fi

# VERIFICACAO DE ARQUIVOS
echo "\n\n"
NARQVERIF=\$(find . -name "${ARQINI}_????_??_??_????_\${tiggeconf}*.grib" -exec ls -ltr {} \; | awk 'BEGIN {i=0} {if ($5==147643 || $5==147667 ) i+=1} END {printf ("%d",i)}')

if [ \${NARQVERIF} -lt \${NARQTIN} ]
then
# CASO ESTEJA INCORRETO O NUMERO DE ARQUIVOS, SAI COM ERRO 23
set -e
set -u
      echo "ERRO NO NUMERO DE ARQUIVOS!"
      exit 23
fi

#rm -Rf ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${PREFX}

cd ${HOME_suite}/run
./runcattigge.bash $TRC $LV ${LABELI} ${PREFX}

exit 0
EOF_

# SUBMISSAO NA FILA
echo "GERANDO RECORTES"
export PBS_SERVER=aux20-eth4

JID=$(qsub ${SCRIPTFILENAME} | awk -F "." '{print $1}')
echo "PROCESSO No: $JID"
it=2
while [ ${it} -gt 0 ]; do
	it=`qstat @$PBS_SERVER | grep $JID | wc -l`
	sleep 3
done


# VERIFICACAO
if [ $membnum -ne 0000 ]; then
   NARQTIN=3228
else
   NARQTIN=3350
fi

# VERIFICACAO DE ARQUIVOS
cd ${SC2_suite}/tigge/dataout/${TRUNC}${LEV}/${LABELI}/${membnum}/
echo "\n\n"
NARQVERIF=$(find . -name "${ARQINI}_*.grib" -exec ls -ltr {} \; | awk 'BEGIN {i=0} {if ($5==147643 || $5==147667 ) i+=1} END {printf ("%d",i)}')

if [ ${NARQVERIF} -lt ${NARQTIN} ]; then
# CASO ESTEJA INCORRETO O NUMERO DE ARQUIVOS, SAI COM ERRO 23
set -e
set -u
   echo "ERRO NO NUMERO DE ARQUIVOS MEMBRO $membnum!"
   exit 23
else
   tar -cvf ${ARQINI}_${membnum}.tar ${ARQINI}_*.grib
   gzip -9 ${ARQINI}_${membnum}.tar
fi

exit 0
