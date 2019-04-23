#!/bin/bash
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runprcmed.sx6                                     #
#                                                                       #
#     Function:       This script initiates the variable                #
#                     so that the program can calculate                 #
#                     the avarage of the members of a                   #
#                     forecast.                                         #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           May 17th, 2004.                                   #
#     Last change:    May 17th, 2004.                                   #
#                                                                       #
#     Valid Arguments for runprcmed.sx6:                                #
#                                                                       #
#      First:    COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#      Third:         LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#      Fifth:     NFCTDY: number of forecasting days                    #
#                                                                       #
#                  LABELx: yyyymmddhh                                   #
#                          yyyy = four digit year                       #
#                            mm = two digit month                       #
#                            dd = two digit day                         #
#                            hh = two digit hour                        #
#                                                                       #
#***********************************************************************#
#end#
#
#       Help:
#
#

TRC=$1
LV=$2
LABELI=$3
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunensmedg.sx6 YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunensmedg.sx6 YYYYMMDDHH"
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

PERT=$4

CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

PREFX=$PERT
NFCTDY=${NFDAYS}

PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`

. ${FILEENV} ${CASE} ${PREFX}
echo $TRC

NMEMBR=`echo ${NPERT}*2+1 | bc -l`
OUT=out
NPROC=1

export LABELI LABELF
echo "LABELI="${LABELI}
echo "LABELF="${LABELF}
#
#     Select parameter for the resolution:
#
case ${TRC} in
021) MR=22 ; IR=64 ; JR=32 ; NPGH=93 ;
     DT=1800
;;
030) MR=31 ; IR=96 ; JR=48 ; NPGH=140 ;
     DT=1800
;;
042) MR=43 ; IR=128 ; JR=64 ; NPGH=187 ;
     DT=1800
;;
047) MR=48 ; IR=144 ; JR=72 ; NPGH=26 ;
     DT=1200
;;
062) MR=63 ; IR=192 ; JR=96 ; NPGH=315 ;
     DT=1200
;;
079) MR=80 ; IR=240 ; JR=120 ; NPGH=26 ;
     DT=900
;;
085) MR=86 ; IR=256 ; JR=128 ; NPGH=26 ;
     DT=720
;;
094) MR=95 ; IR=288 ; JR=144 ; NPGH=591 ;
     DT=720
;;
106) MR=107 ; IR=320 ; JR=160 ; NPGH=711 ;
     DT=600
;;
126) MR=127 ; IR=384 ; JR=192 ; NPGH=284 ;
     DT=600
;;
159) MR=160 ; IR=480 ; JR=240 ; NPGH=1454 ;
     DT=450
;;
170) MR=171 ; IR=512 ; JR=256 ; NPGH=1633 ;
     DT=450
;;
213) MR=214 ; IR=640 ; JR=320 ; NPGH=2466 ;
     DT=360
;;
254) MR=255 ; IR=768 ; JR=384 ; NPGH=3502 ;
     DT=300
;;
319) MR=320 ; IR=960 ; JR=480 ; NPGH=26 ;
     DT=240
;;
*) echo "Wrong request for horizontal resolution: ${TRC}" ; exit 1;
esac
#
#   Set host, machine, NQS Queue, Run time and Extention
#
HSTMAQ=`hostname`
RUNTM=`date +'%Y%m%d%T'`
EXT=${PREFX}
#
#   Set truncation and layers
#
RESOL=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
NIVEL=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
#
cd ${HOME_suite}/run
#

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

cat <<EOT0 > ${SCRIPTFILENAME}
#!/bin/bash
#PBS -W umask=026
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -lselect=1:ncpus=1
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -V
#PBS -N GHPRECSC

#*****************************************************************#
#                                                                 #
#       Name:           setprcmed${RESOL}${NIVEL}.${MAQUI}        #
#                                                                 #
#       Function:       This script file is used to set the       #
#                       environmental variables and start         #
#                       the week mean precipitation.              #
#                                                                 #
#*****************************************************************#
#
#  At SX6 Both the output (stdout) and the error
#  messages (stderr) are written to the same file
#
#   Set date (day,month,year) and hour (hour:minute) 
#
#   DATE=yyyymmdd
#   HOUR=hh:mn:ss
#
DATE=`date +'%Y%m%d'`
HOUR=`date +'%T'`
export DATE HOUR
#
#   Set directories
#
#   OPERMOD  is the directory for sources, scripts and
#            printouts files.
#   SOPERMOD is the directory for input and output data
#            and bin files.
#   ROPERMOD is the directory for big selected output files.
#   AOPERMOD is the directory for alternative initial conditions.
#

cd ${HOME_suite}/run
#
#   Set Horizontal Truncation and Vertical Layers
#
LEV=${NIVEL}
TRUNC=${RESOL}
export TRUNC LEV
#
# Define variables to generate variable data file names:
#
OUT=${EXT}
export OUT
#
EXTS=S.unf
export EXTS 
#
#   Set SX6 FORTRAN variables for output time diagnostics
#
#   F_PROGINF gives the elapsed, user, system and vector instruction
#             execution time, and execution count of all instructions
#             and number of vector instruction executions.
#   F_FILEINF gives informations about I/O operations.
#
F_PROGINF=DETAIL
export F_PROGINF
#
#   Set FORTRAN compilation flags
#
#   -float0 floating-point data format IEEE is enabled
#   -ew     sets the basic numeric size to 8 bytes
#
#   Set FORTRAN environment file name
#
#   FFFn is associated with FORTRAN file unit = n
#
FFF=F_FF
export FFF
#
#   Set environmental variables to binary conversion
#
F_UFMTIEEE=80,90
export F_UFMTIEEE
F_UFMTADJUST80=TYPE2
F_UFMTADJUST90=TYPE2
export F_UFMTADJUST80 F_UFMTADJUST90
#
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#   Run prcmed
#
#Parameter to be read by prcmed.f90 : namelist file
#UNDEF     : ( REAL    ) undef value set up according to original grib files
#IMAX      : ( INTEGER ) number of points in zonal way
#JMAX      : ( INTEGER ) number of points in merdional way
#NFCTDY    : ( INTEGER ) number of forecast days
#FREQCALC  : ( INTEGER ) interval in hours of output ensemble forecast
#NWEEK     : ( INTEGER ) number of week to compute mean field
#DATAINDIR : ( CHAR    ) input directory (ensemble mean)
#DATAOUTDIR: ( CHAR    ) output directory (week mean precipitation)
#PREFX     : ( CHAR    ) preffix for input and output files 
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
cat <<EOT > ${SC1_suite}/produtos/prcmed/bin/prcmed.${LABELI}.nml
UNDEF     :   9.999E+20
IMAX      :   ${IR}
JMAX      :   ${JR}
NFCTDY    :   ${NFDAYS}
FREQCALC  :   6
NWEEK     :   2
DATAINDIR :   ${SC2_suite}/ensmed/dataout/${CASE}/${LABELI}/
DATAOUTDIR:   ${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/
PREFX     :   ENM
RESOL     :   ${CASE}
EOT
mkdir -p ${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/
#
#-------------------------------------------------------------
#
#   Run the prcmed fortran program
#
cd ${SC1_suite}/produtos/prcmed/bin
${SC1_suite}/produtos/prcmed/bin/prcmed.x ${LABELI}

LABELWI=`date -d "${LABELI:0:8} ${LABELI:8:2}:00 1 day" +"%Y%m%d%H"`
LABELWF=`date -d "${LABELI:0:8} ${LABELI:8:2}:00 7 day" +"%Y%m%d%H"`
GPOS=GPRCENM
rm -f ${HOME_suite}/produtos/prcmed/scripts/file*
${SC1_suite}/produtos/prcmed/bin/dayint_mod_medw.x ${LABELI} \${LABELWI} \${LABELWF} ${SC1_suite}/produtos/prcmed/climatologia/
${SC1_suite}/produtos/prcmed/bin/prec_clim_inmet.x ${LABELI} \${LABELWI} \${LABELWF} ${SC1_suite}/produtos/prcmed/INMET/prec/
${SC1_suite}/produtos/prcmed/bin/tmed_clim_inmet.x ${LABELI} \${LABELWI} \${LABELWF} ${SC1_suite}/produtos/prcmed/INMET/

echo "${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/\${GPOS}${LABELI}\${LABELWF}P.fct.${CASE}.ctl" >> ${HOME_suite}/produtos/prcmed/scripts/filefct${LABELI}.${CASE}
echo "${SC1_suite}/produtos/prcmed/bin/cprec${LABELI}\${LABELWF}.ctl" >> ${HOME_suite}/produtos/prcmed/scripts/fileclm${LABELI}.${CASE}

LABELWI=`date -d "${LABELI:0:8} ${LABELI:8:2}:00 8 day" +"%Y%m%d%H"`
LABELWF=`date -d "${LABELI:0:8} ${LABELI:8:2}:00 14 day" +"%Y%m%d%H"`

${SC1_suite}/produtos/prcmed/bin/dayint_mod_medw.x ${LABELI} \${LABELWI} \${LABELWF} ${SC1_suite}/produtos/prcmed/climatologia/
${SC1_suite}/produtos/prcmed/bin/prec_clim_inmet.x ${LABELI} \${LABELWI} \${LABELWF} ${SC1_suite}/produtos/prcmed/INMET/prec/
${SC1_suite}/produtos/prcmed/bin/tmed_clim_inmet.x ${LABELI} \${LABELWI} \${LABELWF} ${SC1_suite}/produtos/prcmed/INMET/

echo "${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/\${GPOS}${LABELI}\${LABELWF}P.fct.${CASE}.ctl" >> ${HOME_suite}/produtos/prcmed/scripts/filefct${LABELI}.${CASE}
echo "${SC1_suite}/produtos/prcmed/bin/cprec${LABELI}\${LABELWF}.ctl" >> ${HOME_suite}/produtos/prcmed/scripts/fileclm${LABELI}.${CASE}

NWK=2

echo ${LABELI} \${NWK} ${CASE} ${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/gif ${SC1_suite}/produtos/prcmed/bin ${HOME_suite}/produtos/prcmed/scripts/
echo ${RESOL} \${NWK} ${LABELI}

mkdir -p ${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/gif

cd ${HOME_suite}/produtos/prcmed/scripts/
${GRADSB}/grads -lb << EOT
run plot_wmp.gs
${LABELI} \${NWK} ${CASE} ${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/gif ${SC1_suite}/produtos/prcmed/bin ${HOME_suite}/produtos/prcmed/scripts/
${CASE} \${NWK} ${LABELI}
EOT

${GRADSB}/grads -lb << EOT
run plot_wmp_as.gs
${LABELI} \${NWK} ${CASE} ${SC2_suite}/produtos/prcmed/dataout/${CASE}/${LABELI}/gif ${SC1_suite}/produtos/prcmed/bin ${HOME_suite}/produtos/prcmed/scripts/
${CASE} \${NWK} ${LABELI}
EOT

exit 0
#
EOT0
#

export PBS_SERVER=aux20-eth4
echo "Submetendo: ${SCRIPTFILENAME}"
JID=$(qsub ${SCRIPTFILENAME} | awk -F "." '{print $1}')
it=2
while [ ${it} -gt 0 ];do
	it=`qstat | grep $JID | wc -l`
	sleep 3
done

exit 0
