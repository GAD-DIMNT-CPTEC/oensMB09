#!/bin/bash -x
#help#
#***********************************************************************#
#                                                                       #
#     Name:           runcluster.sx6                                    #
#                                                                       #
#     Function:       This script evaluate the probabilities            #
#                     from CPTEC global ensemble forecasting.           #
#                     It runs in Korn Shell.                            #
#                                                                       #
#     Date:           Apr 04th, 2005.                                   #
#     Last change:    Apr 04th, 2005.                                   #
#                                                                       #
#     Valid Arguments for runcluster.sx6:                               #
#                                                                       #
#      First:    COMPILE: help, make, clean or run                      #
#     Second:        TRC: three-digit triangular truncation             #
#      Third:         LV: two-digit number of vertical sigma-layers     #
#     Fourth:     LABELI: initial forecasting label                     #
#      Fifth:     NFCTDY: number of forecasting days                    #
#      Sixth:     NMEMBR: number of members of the ensemble             #
#    Seventh:      PREFX: preffix for input and output files            #
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

LABELI=$1
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

PREFX=$2
TRC=$3
LV=$4

CASE=`echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' `

PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} ${PREFX}

if [ -s $PREFX ]; then
      echo "ERRO - PARAMETRO PERT\nFORMATO: runrectigge.sx6 yyyymmddhh 01N"
      exit 2
fi

NFCTDY=${FSCT}
NMEMBR=`echo "${NPERT}*2+1" | bc -l`
OUT=out
NPROC=1
#
#  End of setting parameters to run
#
########
#
#   Set final forecasting labels and UTC Hour
#
calday ()
{
yi=`echo ${LABELI} | cut -c 1-4`
mi=`echo ${LABELI} | cut -c 5-6`
di=`echo ${LABELI} | cut -c 7-8`
hi=`echo ${LABELI} | cut -c 9-10`
let ybi=${yi}%4
if [ ${ybi} = 0 ]
then
md=(31 29 31 30 31 30 31 31 30 31 30 31)
else
md=(31 28 31 30 31 30 31 31 30 31 30 31)
fi
df=$(echo ${di}+${NFDAYS} | bc -l)
mf=${mi}
yf=${yi}
hf=${hi}
n=$(echo ${mi}-1 | bc -l)
if [ $df -gt ${md[${n}]} ]
then
df=$(echo ${df}-${md[${n}]} | bc -l)
mf=$(echo ${mf}+1 | bc -l)
if [ ${mf} -eq 13 ]
then
mf=1
yf=$(echo $yf+1 | bc -l)
fi
fi
if [ $df -lt 10 ]
then DF=0${df}
else DF=${df}
fi
if [ $mf -lt 10 ]
then MF=0${mf}
else MF=${mf}
fi
YF=${yf}
if [ $hf -lt 10 ]
then HF=0${hf}
else HF=${hf}
fi
}
calday
LABELF=${YF}${MF}${DF}${HF}
export LABELI LABELF
echo "LABELI="${LABELI}
echo "LABELF="${LABELF}
#
#
#   Set eta forecasting labels: LABELE
#   
#   Write clusters for 5 days forecasting
#
NFDYETA=5
LABELE=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 $NFDYETA days" +"%Y%m%d%H")
export LABELE
echo "LABELE="${LABELE}
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
HSTMAQ=`hostname`
MAQUI=bash

RUNTM=`date +'%Y%m%d%T'`
EXT=${OUT}
echo ${MAQUI}
echo ${QUEUE}
echo ${RUNTM}
echo ${EXT}
#
#   Set truncation and layers
#
RESOL=T${TRC}
NIVEL=L${LV}
#
cd ${HOME_suite}/run
#

SCRIPTFILENAME=${HOME_suite}/run/set$(basename ${0}).${CASE}.$(hostname)
OUTPUTFILEPATH=${SC2_suite}/out_err/${CASE}/${LABELI}; mkdir -p $OUTPUTFILEPATH

cat <<EOT0 > ${SCRIPTFILENAME}
#!/bin/ksh 
#*****************************************************************#
#                                                                 #
#       Name:           setcluster${RESOL}${NIVEL}.${MAQUI}       #
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
#PBS -l walltime=4:00:00
#PBS -W umask=026
#PBS -o ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.out
#PBS -e ${OUTPUTFILEPATH}/$(basename ${0}).${LABELI}.${tmstp}.err
#PBS -q $QUEUE3
#PBS -S /bin/bash
#PBS -N CLUSTER
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
#
cd ${HOME_suite}/run
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
F_UFMTIEEE=70,20
export F_UFMTIEEE
F_UFMTADJUST20=TYPE2
F_UFMTADJUST70=TYPE2
export F_UFMTADJUST70 F_UFMTADJUST20
#
F_SETBUF=2048
export F_SETBUF
echo " F_SETBUF = \${F_SETBUF}"
#
#  Now, verify if compile or run
#
if [ "\${COMPILE}" != "run" ]
then
cd ${HOME_suite}/cluster/source
make -f Makefile clean
make -f Makefile 
#
else
#
#   Run cluster
#
#Parameter to be read by cluster.f90 : namelist file
#IMAX      : ( INTEGER ) number of points in zonal direction
#JMAX      : ( INTEGER ) number of points in merdional direction
#NMEMBERS  : ( INTEGER ) number of members of the ensemble
#NFCTDY    : ( INTEGER ) number of forecast days
#GRPETA    : ( INTEGER ) number of outputs clusters for eta ensemble
#FREQCALC  : ( INTEGER ) interval in hours for computing clusters
#LONW      : ( REAL    ) western longitude for region used to evaluate the clusters 
#LONE      : ( REAL    ) eastern longitude for region used to evaluate the clusters 
#LATS      : ( REAL    ) southern latitude for region used to evaluate the clusters 
#LATN      : ( REAL    ) northern latitude for region used to evaluate the clusters 
#DATALSTDIR: ( CHAR    ) input directory (ensemble members)
#DATARMSDIR: ( CHAR    ) input directory of climatological rms of GCM-CPTEC
#DATAOUTDIR: ( CHAR    ) output directory of cluster means
#DATACLTDIR: ( CHAR    ) output directory of cluster tables
#RESOL     : ( CHAR    ) horizontal and vertical model resolution
#PREFX     : ( CHAR    ) preffix for input and output files 

mkdir -p ${SC2_suite}/produtos/cluster/dataout/${CASE}/clusters/

cat <<EOT > ${SC1_suite}/produtos/cluster/bin/clustersetup.${LABELI}.nml
IMAX      :   ${IR}
JMAX      :   ${JR}
NMEMBERS  :   ${NMEMBR}
NFCTDY    :   ${NFDAYS}
GRPETA    :    4
FREQCALC  :    6
LONW      :   -101.25 
LONE      :    -11.25 
LATS      :    -60.00
LATN      :     15.00
DATALSTDIR:   ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/
DATARMSDIR:   ${SC1_suite}/produtos/cluster/rmsclim/
DATAOUTDIR:   ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/
DATACLTDIR:   ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/clusters/
RESOL     :   ${CASE}
PREFX     :   ${PREFX}
EOT
#
#-------------------------------------------------------------

#
#   Run the cluster fortran program
#
mkdir -p ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/clusters/
cd ${SC1_suite}/produtos/cluster/bin
${SC1_suite}/produtos/cluster/bin/cluster.x ${LABELI} ${LABELE}

cd ${HOME_suite}/cluster/scripts

NRNDP=$NPERT
NPERT=1
while [ \${NPERT} -le \${NRNDP} ]; do
   
   if [ \${NPERT} -lt 10 ];    then
      NPERT='0'\${NPERT}
   fi
   rm -f filefct\${NPERT}P${LABELI}.${TRC}
   rm -f filefct\${NPERT}N${LABELI}.${TRC}
   NPERT=\$( echo \$NPERT+1 | bc -l)
done

   rm -f filefct${PREFX}${LABELI}.${TRC}
   rm -f fileclt${LABELI}.${TRC}


let NHOURS=24*$NFDAYS
NCTLS=0
TIM=0
 while [ \${TIM} -le \${NHOURS} ]; do
   LABELF=\$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 \${TIM} hour" +"%Y%m%d%H")
   echo 'LABELF='\${LABELF}

   if [ \${TIM} -eq 0 ]
   then
      TYPE='P.icn'
   else
      TYPE='P.fct'
   fi

   NRNDP=${NPERT}
   NPERT=1
   while [ \${NPERT} -le \${NRNDP} ]
   do
      if [ \${NPERT} -lt 10 ]
      then
         NPERT='0'\${NPERT}
      fi

      if [ -s ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS\${NPERT}P${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl ]
      then
cat <<EOT>> filefct\${NPERT}P${LABELI}.${TRC}
${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS\${NPERT}P${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl
EOT
      else
         echo "${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS\${NPERT}P${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl does not exist"
         exit
      fi

      if [ -s ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS\${NPERT}N${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl ]
      then
cat <<EOT>> filefct\${NPERT}N${LABELI}.${TRC}
${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS\${NPERT}N${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl
EOT
      else
         echo "${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS\${NPERT}N${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl does not exist"
         exit
      fi

      let NPERT=NPERT+1
   done

   if [ -s ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl ]
   then
cat <<EOT>> filefct${PREFX}${LABELI}.${TRC}
${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl
EOT
   else
      echo "${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS${PREFX}${LABELI}\${LABELF}\${TYPE}.${CASE}.ctl does not exist"
      exit
   fi

   if [ -s ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/clusters/clusters${LABELI}\${LABELF}.${CASE} ]
   then
cat <<EOT>> fileclt${LABELI}.${TRC}
${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/clusters/clusters${LABELI}\${LABELF}.${CASE}
EOT
   else
      echo "${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/clusters/clusters${LABELI}\${LABELF}.${CASE} does not exist"
      exit
   fi

   let NCTLS=\$NCTLS+1
   let TIM=\$TIM+6
done
for ctl in \$(ls ${SC2_suite}/pos/dataout/${CASE}/${LABELI}/GPOS*ctl); do
   gribmap -i \$ctl
done

#
# Plot the figures
#

mkdir -p ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/gif
${GRADSB}/grads -pb <<_E.
run plot_temp_zgeo.gs
${TRC} ${LABELI} ${NMEMBR} \${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/gif
_E.
echo "${TRC} ${LABELI} ${NMEMBR} \${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/gif"

exit 0

echo "${TRC} ${LABELI} ${NMEMBR} \${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/gif"
${GRADSB}/grads -pb <<_E.
run plot_prec_psnm_wind.gs
${TRC} ${LABELI} ${NMEMBR} \${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/gif
_E.

echo "${TRC} ${LABELI} ${NMEMBR} \${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/gif"
${GRADSB}/grads -pb <<_E.
run plot_tems.gs
${TRC} ${LABELI} ${NMEMBR} \${NCTLS} ${CASE} ${PREFX} ${SC2_suite}/produtos/cluster/dataout/${CASE}/${LABELI}/gif
_E.
exit 0


fi

EOT0
#
chmod 700 ${SCRIPTFILENAME}
${SCRIPTFILENAME}










exit 0

#
#   Run cluster script
#
echo "qsub  $SCRIPTFILENAME"

export PBS_SERVER=aux20-eth4
JID=$(qsub ${SCRIPTFILENAME} | awk -F "." '{print $1}')
it=2
while [ ${it} -gt 0 ];do
	it=`qstat | grep $JID | wc -l`
	sleep 3
done

exit 0
