#!/bin/bash
#./rundrpt.bash TQ0126L028 NMC YES 2012111200
#help#
#*******************************************************************#
#                                                                   #
#     Name:           rundrpt1.una                                  #
#                                                                   #
#     Function:       This script submits the                       #
#                     decomposition scripts                         #
#                     It runs in Korn Shell.                        #
#                                                                   #
#     Date:           May    26th, 2003.                            #
#     Last change:    May    26th, 2003.                            #
#                                                                   #
#     Valid Arguments for rundrpt.una                               #
#                                                                   #
#     First  : COMPILE: help, make, clean or run                    #
#     Second : NUMPERT: number of perturbations                     #
#     Third  :     TRC: three-digit triangular truncation           #
#     Fourth :      LV: two-digit number of vertical sigma-layers   #
#     Fifth  :  LABELI: input field label                           #
#     Sixth  :  HUMID: YES or NO (humidity will be perturbed)       #
#     Seventh:  PERT: NMC, AVN CTR 01N                              #
#     Seventh:  TOTAL: hold                                         #
#                       or none for analysis                        #
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
   echo "Sixth argument is not set (HUMID)"
   exit
else
   HUMID=${3}
fi
if [ -z "${4}" ]; then
   echo "Fifth argument is not set (LABELI: yyyymmddhh)"
   exit
else
   LABELI=${4}
fi

#
#   Set machine, Run time and Extention
#
HSTMAQ=`hostname`
RUNTM=`date +'%Y'``date +'%m'``date +'%d'``date +'%H:%M'`
EXT=out
CASE=$TRCLV
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
     18) KR=18 ; LR=11 ;;
     28) KR=28 ; LR=11 ;;
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
     28) KR=28 ; LR=11 ;;
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

cd ${HOME_suite}/run
export PBS_SERVER=aux20-eth4

#
SCRIPTSFILE=setdrpt.${RESOL}${NIVEL}.${LABELI}
cat <<EOT0 > ${HOME_suite}/run/${SCRIPTSFILE}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:        setdrpt${RESOL}${NIVEL}.${MAQUI}          #
#                                                            #
#     Function:    This script file is used to set the       #
#                  environmental variables and start the     #
#                  decomposition script.                     #
#                                                            #
#************************************************************#
#
#PBS -o ${SC2_suite}/decanl/output/setdrpt.1.${RESOL}${NIVEL}.${LABELI}.${MAQUI}.${RUNTM}.out
#PBS -e ${SC2_suite}/decanl/output/setdrpt.1.${RESOL}${NIVEL}.${LABELI}.${MAQUI}.${RUNTM}.err
#PBS -S /bin/bash
#PBS -l walltime=0:01:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N drptCTR
#PBS -q ${QUEUE1}
export PBS_SERVER=aux20-eth4

cd ${HOME_suite}/run
. ${FILEENV} ${1} ${2}

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
#   Set labels (date, UTC hour, ...)
#
#   LABELI = yyyymmddhh
#   LABELI = input file label
#
LABELI=${LABELI}
echo \${LABELI} \${NUMPERT}
export LABELI NUMPERT
#
#   Prefix names for the FORTRAN files
#
#   NAMEL - List file name prefix
#   GNAME - Initial condition file name prefix
#   NAMER - Input gridded file name prefix
#   NAMES - Output spectral file name prefix
#
if [ "${PERT}" = "AVN" ]
then
NAMEL=GANLAVN
GNAME=GANLAVN
else
NAMEL=GANLNMC
GNAME=GANLNMC
fi
export NAMEL
export GNAME
#
#   Suffix names for the FORTRAN files
#
#   EXTL - List file name suffix
#   EXTG - Initial condition file name suffix
#   ERRi - Input gridded file name suffix
#   ERSi - Output spectral file name suffix
#
EXTL=P.rpt
export EXTL
EXTG=S.unf
export EXTG
EXTR=R.unf
PT=R
#
#   Set directories
#
#   OPERMOD  is the directory for sources, scripts and
#            printouts files.
#   SC1_suite is the directory for input and output data
#            and bin files.
#   SC2_suite is the directory for big selected output files.
#   IOPERMOD is the directory for input file.
#
echo \${HOME_suite}
echo \${SC1_suite}
echo \${SC2_suite}
echo \${SC1_suite}/model/datain
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
COMPILE=run
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
#FTNFLAG='  -h byteswapio -s real64  -s integer64'
FTNFLAG='  -byteswapio -r8 -i8'

export FTNFLAG
#
#   Set C pre-processing flags
#
INC=\${HOME_suite}/include/\${TRUNC}\${LEV}
CPP=" -I\${INC}"
export INC CPP
#
#   Set FORTRAN compiler name
#
#F77="f90 -V -float0 -ew "
F77="ftn  "
export F77
#
#   Set FORTRAN environment file name
#
#   $FFFn is associated with FORTRAN file unit = n
#
FFF=FORT
export FFF
#
#   Set environmental variables to binary conversion
#
#FORT_CONVERT10=BIG_ENDIAN
#FORT_CONVERT20=BIG_ENDIAN
#export FORT_CONVERT10 FORT_CONVERT20
#
F_UFMTIEEE=10,11,20
F_UFMTADJUST11=TYPE2
F_UFMTADJUST20=TYPE2
#AMM export F_UFMTIEEE F_UFMTADJUST
#export F_UFMTIEEE F_UFMTADJUST11 F_UFMTADJUST20
#  Now, build the necessary INCLUDE for the choosen truncation and 
#       vertical resolution.. 
#
if [ "\${COMPILE}" != "run" ]
  then
#
cd \${INC}
#
cat <<EOT1 > decanl.n
      INTEGER IMAX,JMAX,MEND,KMAX,LMAX
      PARAMETER (IMAX=${IR},JMAX=${JR},MEND=${MR},KMAX=${KR},LMAX=${LR})
EOT1
if (diff decanl.n decanl.h > /dev/null)
then
    echo "decanl.n and decanl.h are the same"
    rm -f decanl.n
else
    echo "decanl.n and decanl.h are different"
    mv decanl.n decanl.h
fi
#
#  End of includes
#
fi
#  Now, build the necessary NAMELIST input:
#
GNAMEL=\${NAMEL}\${LABELI}\${EXTL}.\${TRUNC}\${LEV}

#para namelist: LDIM=\${NUMPERT}
mkdir -p \${SC1_suite}/decanl/datain
cat <<EOT2 > \${SC1_suite}/decanl/datain/decanl.nml
 &DATAIN
  LDIM=1
  DIRL='\${SC1_suite}/decanl/datain/ '
  DIRI='\${SC1_suite}/model/datain/ '
  DIRG='\${SC2_suite}/rdpert/dataout/\${TRUNC}\${LEV}/ '
  DIRS='\${SC1_suite}/model/datain/ '
  GNAMEL='\${GNAMEL} '
 &END
 &HUMIDI
  HUM='${HUMID}'
 &END
EOT2
i=1
while [ \${i} -le ${NPERT} ]; do
#
   if [ \${i} -le 9 ]; then
cat <<EOT3 > \${SC1_suite}/decanl/datain/\${GNAMEL}
\${GNAME}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
GANL0\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}
GANL0\${i}R\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
EOT3
   else
cat <<EOT3 > \${SC1_suite}/decanl/datain/\${GNAMEL}
\${GNAME}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
GANL\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}
GANL\${i}R\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
EOT3
   fi
#
#
   cd ${HOME_suite}/run
#
#   Run Decomposition
#
   \${HOME_suite}/decanl/scripts/decanl.scr
#
#
   echo \${i}
   let i=i+1
done
EOT0

#
#   Change mode to be executable
#
chmod 750  ${HOME_suite}/run/${SCRIPTSFILE}

echo "SUBMIT: ${HOME_suite}/run/${SCRIPTSFILE}"

set -x
jobnumber=$(qsub ${HOME_suite}/run/${SCRIPTSFILE} | cut -d. -f1)
it=2
while [ ${it} -gt 0 ];do
   it=`qstat | grep $jobnumber | wc -l`
   sleep 30
done

exit 0
