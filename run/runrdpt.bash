#!/bin/bash -x
#./runrdpt.bash TQ0126L028 NMC YES 2012111200
#help#
#*******************************************************************#
#                                                                   #
#     Name:           runrdpt1.sx6                                  #
#                                                                   #
#     Function:       This script submits the                       #
#                     random perturbation                           #
#                     It runs in Korn Shell.                        #
#                                                                   #
#     Date:           May    26th, 2003.                            #
#     Last change:    May    26th, 2003.                            #
#                                                                   #
#     Valid Arguments for runrdpt.sx6                               #
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
#                                                                   #
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
     28) KR=28 ; LR=17 ;;
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
*) echo "Wrong request for horizontal resolution: ${TRC}" ; exit 1;
esac
#
#
export PBS_SERVER=aux20-eth4
cd ${HOME_suite}/run
#
mkdir -p ${SC2_suite}/rdpert/output

cat <<EOT0 > setrdpt.${RESOL}${NIVEL}.${LABELI}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:        setrdpt${RESOL}${NIVEL}.${MAQUI}          #
#                                                            #
#     Function:    This script file is used to set the       #
#                  environmental variables and start the     #
#                  ranDOm perturbation script.               #
#                                                            #
#************************************************************#
#
#PBS -o ${SC2_suite}/rdpert/output/setrdpt.${RESOL}${NIVEL}.${LABELI}.${MAQUI}.${RUNTM}.out
#PBS -e ${SC2_suite}/rdpert/output/setrdpt.${RESOL}${NIVEL}.${LABELI}.${MAQUI}.${RUNTM}.err
#PBS -S /bin/bash
#PBS -l walltime=0:01:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -N rdpt${PREFIC}
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
NUMPERT=${NPERT}
LABELI=${LABELI}
#
echo \${LABELI} \${NUMPERT}
export LABELI NUMPERT
#
#   Prefix names for the FORTRAN files
#
#   NAMER - Recomposed input file prefix
#   NAMEP - Recomposed perturbed file prefix
#
#
NAMER=GANL${PREFIC}
export NAMER 
#
#   Suffix names for the FORTRAN files
#
#   EXTR  - Recomposed input file extension
#
EXTR=R.unf
export EXTR
#
#   Set directories
#
#   HOME_suite  is the directory for sources, scripts and
#            printouts files.
#   SC1_suite is the directory for input and output data
#            and bin files.
#   SC2_suite is the directory for big selected output files.
#   IHOME_suite is the directory for input file.
#
export HOME_suite SC1_suite SC2_suite IHOME_suite
echo \${HOME_suite}
echo \${SC1_suite}
echo \${SC2_suite}
echo \${SC1_suite}/model/datain
#
echo "cp  ${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/\${NAMER}\${LABELI}\${EXTR}.${RESOL}${NIVEL} ${SC1_suite}/model/datain"
cp  ${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/\${NAMER}\${LABELI}\${EXTR}.${RESOL}${NIVEL} ${SC1_suite}/model/datain

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
#FTNFLAG='  -h byteswapio -s real64 -s integer64  '
FTNFLAG='  -byteswapio -r8 -i8  '

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
#FORT_CONVERT10=BIG_enDIAN
#FORT_CONVERT20=BIG_ENDIAN
#export FORT_CONVERT10 FORT_CONVERT20
#
F_UFMTIEEE=10,20
#AMM F_UFMTADJUST=TYPE2
#AMM export F_UFMTIEEE F_UFMTADJUST
export F_UFMTIEEE
#
#  Now, build the necessary INCLUDE for the choosen truncation and 
#       vertical resolution.. 
#
  if [ "\${COMPILE}" != "run" ]
  then
#
cd \${INC}
#
cat <<EOT1 > rdpert.n
      INTEGER IMAX,JMAX,MEND,KMAX,IDM
      PARAMETER (IMAX=${IR},JMAX=${JR},MEND=${MR},KMAX=${KR},IDM=\${NUMPERT})
EOT1
if (diff rdpert.n rdpert.h > /dev/null)
then
    echo "rdpert.n and rdpert.h are the same"
    rm -f rdpert.n
else
    echo "rdpert.n and rdpert.h are different"
    mv rdpert.n rdpert.h
fi
#
#  End of includes
#
fi
#
#  Now, build the necessary NAMELIST input:
#
#  Mariane (1999) stdt=0.6 K, stdu=3 m/s
#
cat <<EOT2 > \${SC1_suite}/rdpert/datain/rdpert.nml
 &DATAIN
  FLONW=0
  FLONE=360.0 
  GLATN=90.0  
  GLATS=-90.0
 &END
 &STPRES
  STDP=1.00
 &END
 &STTEMP
  STDT( 1)=0.60,STDT( 2)=0.60,STDT( 3)=0.60,STDT( 4)=0.60,STDT( 5)=0.60,
  STDT( 6)=0.60,STDT( 7)=0.60,STDT( 8)=0.60,STDT( 9)=0.60,STDT(10)=0.60,
  STDT(11)=0.60,STDT(12)=0.60,STDT(13)=0.60,STDT(14)=0.60,STDT(15)=0.60,
  STDT(16)=0.60,STDT(17)=0.60,STDT(18)=0.60,STDT(19)=0.60,STDT(20)=0.60,
  STDT(21)=0.60,STDT(22)=0.60,STDT(23)=0.60,STDT(24)=0.60,STDT(25)=0.60,
  STDT(26)=0.60,STDT(27)=0.60,STDT(28)=0.60	     
 &END
 &STZWIN
  STDU( 1)=3.00,STDU( 2)=3.00,STDU( 3)=3.00,STDU( 4)=3.00,STDU( 5)=3.00,
  STDU( 6)=3.00,STDU( 7)=3.00,STDU( 8)=3.00,STDU( 9)=3.00,STDU(10)=3.00,
  STDU(11)=3.00,STDU(12)=3.00,STDU(13)=3.00,STDU(14)=3.00,STDU(15)=3.00,
  STDU(16)=3.00,STDU(17)=3.00,STDU(18)=3.00,STDU(19)=3.00,STDU(20)=3.00,
  STDU(21)=3.00,STDU(22)=3.00,STDU(23)=3.00,STDU(24)=3.00,STDU(25)=3.00,
  STDU(26)=3.00,STDU(27)=3.00,STDU(28)=3.00	     
 &END
 &STMWIN
  STDV( 1)=3.00,STDV( 2)=3.00,STDV( 3)=3.00,STDV( 4)=3.00,STDV( 5)=3.00,
  STDV( 6)=3.00,STDV( 7)=3.00,STDV( 8)=3.00,STDV( 9)=3.00,STDV(10)=3.00,
  STDV(11)=3.00,STDV(12)=3.00,STDV(13)=3.00,STDV(14)=3.00,STDV(15)=3.00,
  STDV(16)=3.00,STDV(17)=3.00,STDV(18)=3.00,STDV(19)=3.00,STDV(20)=3.00,
  STDV(21)=3.00,STDV(22)=3.00,STDV(23)=3.00,STDV(24)=3.00,STDV(25)=3.00,
  STDV(26)=3.00,STDV(27)=3.00,STDV(28)=3.00	    
 &END
 &STHUMI
  STDQ( 1)= 0.770,STDQ( 2)= 0.780,STDQ( 3)= 0.780,STDQ( 4)= 0.780,
  STDQ( 5)= 0.800,STDQ( 6)= 0.820,STDQ( 7)= 0.880,STDQ( 8)= 0.980,
  STDQ( 9)= 1.140,STDQ(10)= 1.270,STDQ(11)= 1.370,STDQ(12)= 1.350,
  STDQ(13)= 1.180,STDQ(14)= 1.050,STDQ(15)= 0.900,STDQ(16)= 0.750,
  STDQ(17)= 0.490,STDQ(18)= 0.260,STDQ(19)= 0.120,STDQ(20)= 0.050,
  STDQ(21)= 0.020,STDQ(22)= 0.000,STDQ(23)= 0.000,STDQ(24)= 0.000,
  STDQ(25)= 0.000,STDQ(26)= 0.000,STDQ(27)= 0.000,STDQ(28)= 0.000
 &END
 &HUMIDI
  HUM='${HUMID}'
 &END
 &DATNAM
  DIRO='\${SC2_suite}/recanl/dataout/\${TRUNC}\${LEV}/ '
  DIRP='\${SC2_suite}/rdpert/dataout/\${TRUNC}\${LEV}/ '
  GNAMEO='\${NAMER}\${LABELI}\${EXTR}.\${TRUNC}\${LEV} '
EOT2
mkdir -p \${SC2_suite}/rdpert/dataout/\${TRUNC}\${LEV}/ 
i=1
while [ \${i} -le \${NUMPERT} ]
do
if [ \${i}  -le 9 ]
then
cat <<EOT3 >> \${SC1_suite}/rdpert/datain/rdpert.nml
  GNAMEP(\${i})='GANL0\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}'
EOT3
else
cat <<EOT3 >> \${SC1_suite}/rdpert/datain/rdpert.nml
  GNAMEP(\${i})='GANL\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}'
EOT3
fi
let i=\${i}+1
done
cat <<EOT4 >> \${SC1_suite}/rdpert/datain/rdpert.nml
 &END
EOT4
#
cd ${HOME_suite}/run
#
#   Run Random Perturbation
#
\${HOME_suite}/rdpert/scripts/rdpert.scr
#
EOT0
#
#   Change mode to be executable
#
chmod 750 setrdpt.${RESOL}${NIVEL}.${LABELI}

jobnumber=$(qsub setrdpt.${RESOL}${NIVEL}.${LABELI} | cut -d. -f1)
it=2
while [ ${it} -gt 0 ];do
   it=`qstat | grep $jobnumber | wc -l`
   sleep 30
done

exit 0
