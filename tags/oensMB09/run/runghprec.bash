#!/bin/bash -x
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runplumes.sx6                                     #
#                                                                       #
#     Function:       This script submits the global                    #
#                     model script to the NQS queue.                    #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           October 08th, 2002.                               #
#     Last change:    October 08th, 2002.                               #
#                                                                       #
#     Valid Arguments for runplumes.sx6:                                #
#                                                                       #
#     First:     COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#     Third:          LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#     Fifth:      NFCTDY: number of forecast days                       #
#     Sixth:      NMEMBR: number of members of the ensemble             #
#    Seventh:      PREFX: preffix for input and output files            #
#                                                                       #
#                  LABELx: yyyymmddhh                                   #
#                          yyyy = four digit year                       #
#                            mm = two digit month                       #
#                            dd = two digit day                         #
#                            hh = two digit hour                        #
#                                                                       #
#***********************************************************************#
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
echo "Sixth argument is not set (LABELI: yyyymmddhh)"
exit
else
LABELI=${4}
fi
if [ -z "${5}" ]
then
echo "Seventh argument is not set (NFDAYS)"
exit
else
NFDAYS=${5}
fi
if [ -z "${6}" ]
then
echo "Eigth argument is not set (NUMPERT)"
exit
else
NPERT=${6}
fi

if [ -z "${7}" ]
then
echo "Eigth argument is not set (NUMPERT)"
exit
else
PREFX=${7}
fi


#
#   Set machine, Run time and Extention
#
export COMPILE=${1}
CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `
HSTMAQ=`hostname`
MACHINE=una
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
echo ${MACHINE}
echo ${RUNTM}
echo ${EXT}
#

export FILEENV=`find ${HOME}/oensMB09 -maxdepth 2 -name EnvironmentalVariablesOENS -print`
. ${FILEENV} ${CASE} ${PREFX}
cd ${HOME_suite}/run

if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunmodgmpi.sx6 YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunmodgmpi.sx6 YYYYMMDDHH"
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

NFCTDY=${NFDAYS}
let NMEMBR=${NPERT}*2+1
OUT=out
NPROC=1

dirghprec=${HOME_suite}/produtos/recortes/scripts
dirinp=${SC2_suite}/plumes/dataout/$CASE/${LABELI}
dirout=${SC2_suite}/produtos/recortes/GHPREC/$CASE/${LABELI}; mkdir -p $dirout


################################################################
#
# Grads:  (tupa)
#
################################################################

#export GRADSB=/stornext/home/oper/bin/grads-2.0.a9.oga

############################################################
#
# Run task
#
# Set characteristics of the product
#
#############################################################

cd $dirghprec

tit='CPTEC GLOBAL ENSEMBLE SURFACE PRODUCTS '$CASE'  COLD' 
let tmax=${NFCTDY}*24
stt='SC'
nloc=5
rm -f loc${stt}.${LABELI}.txt
cat << EOT1 > loc${stt}.${LABELI}.txt
55 58 59 60 61
EOT1

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Loc is found from file :(Ex.:) LOCMM20041004002004101900.T126L28
# 55 -> CRICIUMA (SC) 
# 58 -> LAGES (SC) 
# 58 -> FLORIANOPOLIS (SC)
# 60 -> CHAPECO (SC) 
# 61 -> JOINVILLE (SC)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
RESOL=${CASE:0:6}; NIVEL=${CASE:6:4}
echo ${RESOL} ${NIVEL} ${LABELI} ${LABELF} ${NMEMBR} ${tmax} ${stt} ${nloc} ${dirinp} ${dirout}

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

cat <<_EOT5> ${SCRIPTFILENAME}
#!/bin/bash 
#PBS -W umask=026
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -lselect=1:ncpus=1
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -V
#PBS -N GHPRECSC

cd $dirghprec
${GRADSB}/grads -lb <<EOT
run rec_gridhistory_prec.gs
${RESOL} ${NIVEL} ${LABELI} ${LABELF} ${NMEMBR} ${tmax} ${stt} ${nloc} ${dirinp} ${dirout}
EOT

rm -f loc${stt}.${LABELI}.txt

#######################################################################
#
# Produce de Ctl
#
#######################################################################

echo ${dirout}/PRECSC${LABELI}*
GPOS=PREC

LABELG=$(date -d "$YYYY$MM$DD $HH:00 1 hour" +"%HZ%d%b%Y")
cat <<EOT2> ${dirout}/\${GPOS}${stt}${LABELI}${LABELF}.${RESOL}${NIVEL}.ctl
DSET ^\${GPOS}${stt}${LABELI}${LABELF}.${RESOL}${NIVEL}.bin
*
UNDEF -2.56E+33 
*
TITLE TOTAL PRECIPITATION                     
*
XDEF    ${nloc} LINEAR 1 1
YDEF   ${NMEMBR} LINEAR       1.0000      1.0000
ZDEF    1 LINEAR 1000 1
TDEF  ${tmax} LINEAR \${LABELG} 1HR
*
VARS  1
prec 0 99 membros
ENDVARS
EOT2

echo ${GRADSB}/lats4d.sh -i ${dirout}/\${GPOS}${stt}${LABELI}${LABELF}.${RESOL}${NIVEL}.ctl -format grads_grib -o ${dirout}/\${GPOS}${stt}${LABELI}${LABELF}.${RESOL}${NIVEL}
mkdir -p ${dirout}/grib
${GRADSB}/lats4d.sh -i ${dirout}/\${GPOS}${stt}${LABELI}${LABELF}.${RESOL}${NIVEL}.ctl -format grads_grib -o ${dirout}/grib/\${GPOS}${stt}${LABELI}${LABELF}.${RESOL}${NIVEL}
rm -f ${dirout}/\${GPOS}${stt}${LABELI}*
mv ${dirout}/grib/* ${dirout}
rmdir ${dirout}/grib
exit 0
_EOT5

export PBS_SERVER=aux20-eth4
echo "Submetendo: ${SCRIPTFILENAME}"
JID=$(qsub ${SCRIPTFILENAME} | awk -F "." '{print $1}')
it=2
while [ ${it} -gt 0 ];do
	it=`qstat | grep $JID | wc -l`
	sleep 3
done

exit 0
