#!/bin/bash
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runaccumulated.sx6                                #
#                                                                       #
#     Function:       This script evaluate the accumulated              #
#                     from CPTEC global ensemble forecasting.           #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           Apr 04th, 2005.                                   #
#     Last change:    Apr 04th, 2005.                                   #
#                                                                       #
#     Valid Arguments for runaccumulated.sx6:                           #
#                                                                       #
#      First:    COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#      Third:         LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#      Fifth:     NFCTDY: number of forecasting days                    #
#      Sixth:     NMEMBR: total number of members of the ensemble       #
#    Seventh:      PREFX: preffix for input and output files            #
#                                                                       #
#                  LABELx: yyyymmddhh                                   #
#                          yyyy = four digit year                       #
#                            mm = two digit month                       #
#                            dd = two digit day                         #
#                            hh = two digit hour                        #
#                                                                       #
#***********************************************************************#
#            Para compilar:   runaccumulated.sx6 clean
#            Para rodar: runaccumulated.sx6 run 126 28 2008051900 15      15       AVN
#                                               TRC LV LABELI     NFCTOY  NMEMBR  PREFIX
#end#
#
#       Help:
#
#

LABELI=$3
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

LABELS=`echo $LABELI | cut -c 1-8`
LABELF=`date -d "$LABELS 15 days" +"%Y%m%d${HH}"`
TRC=$1
LV=$2
PREFX=$4

CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`

. ${FILEENV} ${CASE} ${PREFX}

NMEMBR=`echo "${NPERT}*2+1" | bc -l`
OUT=out
NPROC=1
#
#  End of setting parameters to run
#
export LABELI LABELF
echo "LABELI="${LABELI}
echo "LABELF="${LABELF}
#
#     Select parameter for the resolution:
#

if [ "run" = "run" ]
then
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
fi
#
#   Set host, machine, NQS Queue, Run time and Extention
#
RUNTM=`date +'%Y%m%d%T'`
EXT=${OUT}
echo ${MAQUI}
echo ${QUEUEP}
echo ${RUNTM}
echo ${EXT}
#
#   Set truncation and layers
#
RESOL=TQ${TRC}L${LV}
TRC=TQ${TRC}
#
cd ${HOME_suite}
#
LABEL12=`date -d "${YYYY}${MM}${DD} ${HH}:00 12 hours ago" +"%Y%m%d%H"`
YYYY12=`echo $LABEL12 | cut -c 1-4`
MM12=`echo $LABEL12 | cut -c 5-6`
DD12=`echo $LABEL12 | cut -c 7-8`
#
LABEL24=`date -d "${YYYY}${MM}${DD} ${HH}:00 24 hours ago" +"%Y%m%d%H"`
YYYY24=`echo $LABEL24 | cut -c 1-4`
MM24=`echo $LABEL24 | cut -c 5-6`
DD24=`echo $LABEL24 | cut -c 7-8`
#

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH
tmstp=$(date +"%s")

cat <<EOT0 > ${SCRIPTFILENAME}
#!/bin/bash -x
#PBS -W umask=026
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -N probability

#
#*****************************************************************#
#                                                                 #
#       Name:           setaccumulated${RESOL}${NIVEL}.${MAQUI}   #
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
cd ${HOME_suite}
#
#   Set Horizontal Truncation and Vertical Layers
#
LEV=${NIVEL}
TRUNC=${RESOL}
export TRUNC LEV
#
#   Set machine
MACH=${MAQUI}
export MACH
#
#   Set option for compiling or not the source codes.
#
#   If COMPILE=make then only the modified sources will be compiled.
#   If COMPILE=clean then the touch files will be removed and 
#              all sources will be compiled.
#             =run for run with no compilation
#
#   If COMPILE is make or clean then the script generates the binary file 
#              and exits;
#              if it is run then the script runs the existent binary file.
#
COMPILE=run
export COMPILE
echo \${COMPILE}
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
#
F_UFMTIEEE=70,71,80
export F_UFMTIEEE
F_UFMTADJUST70=TYPE2
F_UFMTADJUST71=TYPE2
F_UFMTADJUST80=TYPE2
export F_UFMTADJUST70 F_UFMTADJUST71 F_UFMTADJUST80
#
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#   Run prcmed
#
#Parameter to be read by prcmed.f90 : namelist file
#UNDEF     : ( REAL    ) undef value set up according to original grib files
#IMAX      : ( INTEGER ) number of points in zonal direction
#JMAX      : ( INTEGER ) number of points in merdional direction
#NMEMBERS  : ( INTEGER ) number of members of the ensemble
#NFCTDY    : ( INTEGER ) number of forecast days
#FREQCALC  : ( INTEGER ) interval in hours of output ensemble forecast
#DIRINP    : ( CHAR    ) input directory (ensemble members)
#DIROUT    : ( CHAR    ) output directory (accumulated outputs)
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
#PREFX     : ( CHAR    ) preffix for input and output files 
#DIRLAB=`echo ${LABELI} | cut -c1-8`
cat <<EOT > ${SC1_suite}/produtos/wgne/bin/accumsetup.${LABELI}.nml
UNDEF     :   -9999
IMAX      :   ${IR}
JMAX      :   ${JR}
IMAXINT   :   144
JMAXINT   :   73
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFDAYS}
FREQCALC  :   6
DIRINP    :   ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/
DIROUT    :   ${SC2_suite}/produtos/wgne/dataout/${CASE}/${LABELI}/
RESOL     :   ${CASE}
PREFX     :   ${PREFX}
EOT
#
#   Run the accumulated fortran program
#
LABELR=\$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 24 hours ago" +"%Y%m%d%H")
ln -s ${SC2_suite}/pos/dataout/${CASE}/\${LABELR}/GPOS* ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/

cd ${SC1_suite}/produtos/wgne/bin
mkdir -p ${SC2_suite}/produtos/wgne/dataout/${CASE}/${LABELI}/

${SC1_suite}/produtos/wgne/bin/accumulated.x ${LABELI}

rm -f ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS???\${LABELR}*

exit 0
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
