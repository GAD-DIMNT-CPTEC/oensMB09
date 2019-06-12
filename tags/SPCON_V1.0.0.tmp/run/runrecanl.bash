#!/bin/bash
#./runrecanl.bash TQ0126L028 NMC ANLNMC 2012111200
#help#
#*******************************************************************#
#                                                                   #
#     Name:           runrecanl1.sx6                                 #
#                                                                   #
#     Function:       This script submits the                       #
#                     recomposition scripts.                        #
#                     It runs in Korn Shell.                        #
#                                                                   #
#     Date:           May    26th, 2003.                            #
#     Last change:    May    26th, 2003.                            #
#                                                                   #
#     Valid Arguments for runrecanl.sx6                             #
#                                                                   #
#     First  : COMPILE: help, make, clean or run                    #
#     Second :     TRC: three-digit triangular truncation           #
#     Third  :      LV: two-digit number of vertical sigma-layers   #
#     Fourth :   TYPES: input spectral file type                    #
#     Fifth  :  LABELI: input field start label                     #
#     Seventh:  TOTAL: hold                                         #
#                       or none for analysis                        #
#                                                                   #
#               TYPES : ANLNMC: for analysis                        #
#                       PERCTR: for control forecast                #
#                       PER01R: for pertubation 1 forecast          #
#                       PER02R: for pertubation 2 forecast          #
#              LABELx : yyyymmddhh                                  #
#                       yyyy = four digit year                      #
#                       mm = two digit month                        #
#                       dd = two digit day                          #
#                       hh = two digit hour                         #
#                                                                   #
#*******************************************************************#
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
#   Set directories
#
#   HOME_suite - HOME DA SUITE
#   SC1_suite - /scratchin
#   SC2_suite - /scratchout
#

export FILEENV=`find ./ -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${1} ${2}
cd ${HOME_suite}/run

if [ -z "${3}" ]; then
   echo "Fourth argument is not set: TYPES"
   exit
else
   PERR=${3}
fi
if [ -z "${4}" ]; then
   echo "Fifth argument is not set (LABELI: yyyymmddhh)"
   exit
else
   LABELI=${4}
fi

#
#   Set truncation and layers
#
TRC=`echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0"`
LV=`echo ${TRCLV} | cut -c 7-11 | tr -d "L0"`
export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

#
case ${TRC} in
021) MR=21 ; IR=64 ; JR=32 ; 
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
030) MR=30 ; IR=96 ; JR=48 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
042) MR=42 ; IR=128 ; JR=64 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=11 ;;
     28) KR=28 ; LR=11 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
047) MR=47 ; IR=144 ; JR=72 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
062) MR=62 ; IR=192 ; JR=96 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=11 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
079) MR=79 ; IR=240 ; JR=120 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
095) MR=95 ; IR=288 ; JR=144 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
106) MR=106 ; IR=320 ; JR=160 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
126) MR=126 ; IR=384 ; JR=192 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=11 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
159) MR=159 ; IR=480 ; JR=240 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
170) MR=170 ; IR=512 ; JR=256 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
213) MR=213 ; IR=640 ; JR=320 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
319) MR=319 ; IR=960 ; JR=480 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
*) echo "Wrong request for horizontal resolution: ${TRC}" ; exit 1;
esac
#
#   Set machine, Run time and Extention
#
HSTMAQ=`hostname`
MAQUI=sx6
#QUEUE=Inter
RUNTM=`date +'%y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
echo ${MAQUI}
echo ${QUEUE1}
echo ${RUNTM}
echo ${EXT}
#
export PBS_SERVER=aux20-eth4

set -x
cd ${HOME_suite}/run

#
COMPILE=run
mkdir -p ${SC2_suite}/recanl/output
cat <<EOT0 > setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:        setrecanl${PERR}${RESOL}${NIVEL}.${MAQUI} #
#                                                            #
#     Function:    This script file is used to set the       #
#                  environmental variables and start the     #
#                  recomposition script.                     #
#                                                            #
#************************************************************#
#
#PBS -o ${SC2_suite}/recanl/output/setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI}.${RUNTM}.out
#PBS -e ${SC2_suite}/recanl/output/setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI}.${RUNTM}.err
#PBS -l walltime=0:30:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N RECANL${NUM}
#PBS -q ${QUEUE1}
export PBS_SERVER=aux20-eth4
. ${FILEENV} ${1} ${2}

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
TYPES=${PERR} 
echo \${TYPES}
export TYPES 
#   LABELI = yyyymmddhh
#   LABELI = input file start label
#
LABELI=${LABELI}
echo \${LABELI}
export LABELI
#
#   Prefix names for the FORTRAN files
#
#   NAMEL - List file name prefix
#   NAMES - Input spectral file name prefix
#   NAMER - Output gridded file name prefix
#
#   Suffix names for the FORTRAN files
#
#   EXTL - List file name suffix
#   ERSi - Input spectral file name suffix
#   ERRi - Output gridded file name suffix
#
NAMEL=G\${TYPES}
echo \${NAMEL}
export NAMEL
NAMES=G\${TYPES}
echo \${NAMES}
export NAMES
NAMER=G\${TYPES}
echo \${NAMER}
export NAMER
if [ "\${TYPES}" = ANLAVN -o "\${TYPES}" = ANLNMC ]; then
   EXTL=S.unf
   echo \${EXTL}
   export EXTL
   ERS1=S.unf
   echo \${ERS1}
   export ERS1
   ERR1=R.unf
   echo \${ERR1}
   export ERR1
else
EXTL=F.ens
   echo \${EXTL}
   export EXTL
   ERS1=F.ens
   echo \${ERS1}
   export ERS1
   ERR1=R.ens
   echo \${ERR1}
   export ERR1
fi
#
#   Set directories
#
#   OPERMOD  is the directory for sources, scripts and
#            printouts files.
#   SC1_suite is the directory for input and output data
#            and bin files.
#   ROPERMOD is the directory for big selected output files.
#   IOPERMOD is the directory for input file.
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
#   Set FORTRAN compilation flags
#
#   -integer_size 64 sets the integer basic numeric size to 8 bytes
#   -real_size 64    sets the real basic numeric size to 8 bytes
#
#
#FTNFLAG='-Wf" -pvctl noaltcode -O nodiv nomove "'
#FTNFLAG='  -h byteswapio -s integer64 -s real64 '
FTNFLAG='  -byteswapio -i8 -r8 '

export FTNFLAG
#
#   Set C pre-processing flags
#
INC=${HOME_suite}/include/\${TRUNC}\${LEV}
mkdir -p \${INC}
CPP=" -I\${INC}"

export INC CPP
#
#   Set FORTRAN compiler name
#

F77="ftn  "
export F77
#
#   Set FORTRAN environment file name
#
#   $FFFn is associated with FORTRAN file unit = n
#
#FFF=FORT
#export FFF
#
#   Set environmental variables to binary conversion
#
#FORT_CONVERT10=BIG_ENDIAN
#FORT_CONVERT20=BIG_ENDIAN
#export FORT_CONVERT10 FORT_CONVERT20
#
F_UFMTIEEE=10,20
F_UFMTADJUST10=TYPE2
#AMM F_UFMTADJUST20=TYPE2
#AMM export F_UFMTIEEE F_UFMTADJUST10 F_UFMTADJUST20
export F_UFMTIEEE F_UFMTADJUST10
#
#  Now, build the necessary INCLUDE for the choosen truncation and 
#       vertical resolution.. 
#
if [ "${COMPILE}" != "run" ]
then
#
cd \${INC}
#
cat <<EOT1 > recanl.n
      INTEGER IMAX,JMAX,MEND,KMAX,LMAX
      PARAMETER (IMAX=${IR},JMAX=${JR},MEND=${MR},KMAX=${KR},LMAX=${LR})
EOT1
if (diff recanl.n recanl.h > /dev/null)
then
    echo "recanl.n and recanl.h are the same"
    rm -f recanl.n
else
    echo "recanl.n and recanl.h are different"
    mv recanl.n recanl.h
fi
#
#  End of includes
#
fi
#
#  Now, build the necessary NAMELIST input:
#
GNAMEL=\${NAMEL}\${LABELI}\${EXTL}.\${TRUNC}\${LEV}
echo \${GNAMEL}
echo ${SC1_suite}/recanl/datain/\${GNAMEL}

#
cat <<EOT2 > ${SC1_suite}/recanl/datain/\${GNAMEL}
\${NAMES}\${LABELI}\${ERS1}.\${TRUNC}\${LEV}
\${NAMER}\${LABELI}\${ERR1}.\${TRUNC}\${LEV}
EOT2
#
cat <<EOT3 > ${SC1_suite}/recanl/datain/recanl\${TYPES}.nml
 &DATAIN
  LDIM=1
  DIRL='${SC1_suite}/recanl/datain/ '
  DIRS='${SC1_suite}/model/datain/ '
  DIRR='${SC2_suite}/recanl/dataout/\${TRUNC}\${LEV}/ '
  GNAMEL='\${GNAMEL} '
 &END
EOT3
#
#
mkdir -p ${SC2_suite}/recanl/dataout/\${TRUNC}\${LEV}/ 
cd ${HOME_suite}/run
#
#   Run Decomposition
#

${HOME_suite}/recanl/scripts/recanl.scr
#
EOT0
#
#   Change mode to be executable
#
chmod 750 setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI}

jobnumber=$(qsub setrecanl.${PERR}${RESOL}${NIVEL}.${LABELI}.${MAQUI} | cut -d. -f1)
it=2
while [ ${it} -gt 0 ];do
   it=`qstat | grep $jobnumber | wc -l`
   sleep 30
done

exit 0
