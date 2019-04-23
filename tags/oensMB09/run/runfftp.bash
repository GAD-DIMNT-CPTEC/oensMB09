#!/bin/bash
#help#
#*****************************************************************#
#                                                                 #
#     Name:           runfftp.una                                 #
#                                                                 #
#     Function:       This script submits the FFT                 #
#                     and Legendre Transform library              #
#                     scripts                                     #
#                     It runs in Korn Shell.                      #
#                                                                 #
#     Date:           May      26th, 2003.                        #
#     Last change:    May      26th, 2003.                        #
#                                                                 #
#     Valid Arguments for runfftp.sx6                             #
#                                                                 #
#     First :COMPILE: help, make, clean                           #
#     Second:    TRC: three-digit triangular truncation           #
#     Third :     LV: two-digit number of vertical sigma-layers   #
#                                                                 #
#*****************************************************************#
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
if [ "${1}" != "make" ]
then
if [ "${1}" != "clean" ]
then
echo "First argument: ${1}, is wrong. Must be: make, clean or run"
exit
fi
fi
if [ -z  "${2}" ]
then
echo "Second argument is not set: TRC"
exit
else 
TRC=${2}
fi
if [ -z  "${3}" ]
then
echo "Third argument is not set: LV"
exit
else
LV=${3}
fi
#
#   Set machine, Run time and Extention
#
HSTMAQ=`hostname`
MAQUI=`hostname -s`
QUEUE=PNT-EN
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
echo ${MAQUI}
echo ${QUEUE}
echo ${RUNTM}
echo ${EXT}
#
#   Set directories
#
#   OPERM  is the directory for sources, scripts and printouts.
#   SOPERM is the directory for input and output files.
#
PATHA=`pwd`
export FILEENV=`find ${PATHA} -name EnvironmentalVariablesMCGA -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} CTR
cd ${HOME_suite}/run
echo ${OPERM}
echo ${SOPERM}
#
#   Set truncation and layers
#
TRC=${2}
LV=${3}
export RESOL=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export NIVEL=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `

#
case ${TRC} in
21) MR=21 ; IR=64 ; JR=32 ; 
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
30) MR=30 ; IR=96 ; JR=48 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
42) MR=42 ; IR=128 ; JR=64 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
47) MR=47 ; IR=144 ; JR=72 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
62) MR=62 ; IR=192 ; JR=96 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
79) MR=79 ; IR=240 ; JR=120 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
95) MR=95 ; IR=288 ; JR=144 ;
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
     28) KR=28 ; LR=17 ;;
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
cd ${OPERM}/run
#

cat <<EOT0 > setfftp${RESOL}${NIVEL}.${MAQUI}
#!/bin/bash
#
#************************************************************#
#                                                            #
#     Name:        setfftp${RESOL}${NIVEL}.${MAQUI}          #
#                                                            #
#     Function:    This script file is used to set the       #
#                  the environmental variables and start     #
#                  the FFT and Legendre Transform            #
#                  Library script.                           #
#                                                            #
#************************************************************#
#
#PBS -o crow:${HOME_suite}/run/setout/setfftp${RESOL}${NIVEL}.${MAQUI}.${RUNTM}.${EXT}
#PBS -j oe
#PBS -l walltime=4:00:00
#PBS -l mppwidth=1
#PBS -l mppnppn=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N $RES
#
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
#   Set directories
#
#   OPERMOD  is the directory for sources, scripts and
#            printouts files.
#   SOPERMOD is the directory for input and output data
#            and bin files.
#
OPERMOD=${OPERM}
SOPERMOD=${SOPERM}
export OPERMOD SOPERMOD
echo \${OPERMOD}
echo \${SOPERMOD}
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
echo ${1}
COMPILE=${1}
echo \${COMPILE}
export COMPILE
#
#   Set FORTRAN compilation flags
#
#   -integer_size 64 sets the integer basic numeric size to 8 bytes
#   -real_size 64    sets the real basic numeric size to 8 bytes
#
#
#FTNFLAG='-Wf" -pvctl noaltcode -O nodiv nomove "'
#FTNFLAG='  -h byteswapio -s integer64 -s real64  '
FTNFLAG='  -byteswapio -i8 -r8  '

export FTNFLAG
#
#   Set C pre-processing flags
#
INC=\${OPERMOD}/include/\${TRUNC}\${LEV}
CPP=" -I\${INC}"
export INC CPP
#
#   Set FORTRAN compiler name
#
#F77="f90 -V -float0 -ew "
F77="ftn  "
export F77
#
#  Now, build the necessary INCLUDE for the choosen
#       truncation and vertical resolution. 
#
  if [ "\${COMPILE}" != "run" ]
  then
#
cd \${INC}
#
cat <<EOT1 > fftpln.n
      INTEGER IMAX,JMAX,MEND,KMAX
      PARAMETER (IMAX=${IR},JMAX=${JR},MEND=${MR},KMAX=${KR})
EOT1
if (diff fftpln.n fftpln.h > /dev/null)
then
    echo "fftpln.n and fftpln.h are the same"
    rm -f fftpln.n
else
    echo "fftpln.n and fftpln.h are different"
    mv fftpln.n fftpln.h
fi
#
#  End of includes
#
fi
#
cd ${HOME_suite}/run
#
#   Run FFT and PLN Library
#
\${OPERMOD}/fftpln/scripts/fftpln.scr
#
EOT0
#
#   Change mode to be executable
#
chmod 744 setfftp${RESOL}${NIVEL}.${MAQUI}
#
#   Submit FFT and PLN Library scripts to NQS ${QUEUE}
#
#
echo "qsub -x -s /bin/ksh -q ${QUEUE} ${OPERM}/run/setfftp${RESOL}${NIVEL}.${MAQUI}"
#AMM qsub -x -s /bin/ksh -q ${QUEUE} ${OPERM}/run/setfftp${RESOL}${NIVEL}.${MAQUI}
${OPERM}/run/setfftp${RESOL}${NIVEL}.${MAQUI}
#
