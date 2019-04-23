#!/bin/bash -x
#help#
#*******************************************************************#
#                                                                   #
#      Name:            rundeco.bash                                #
#                                                                   #
#      Function:        This script submits the                     #
#                       decomposition scripts                       #
#                       It runs in Korn Shell.                      #
#                                                                   #
#      Date:            June     02th, 2003.                        #
#      Last change:     Oct      13th, 2011.                        #
#                                                                   #
#     Valid Arguments for rundeco.bash                              #
#                                                                   #
#      First :    HELP: help or nothing for getting help            #
#      First : COMPILE: help, make, clean or run                    #
#      Second:     TRC: three-digit triangular truncation           #
#      Third :      LV: two-digit number of vertical sigma-layers   #
#      Fourth:     NUM: pertubation number                          #
#      Fifth :  LABELI: initial forecasting label                   #
#      Sixth :  NFDAYS: number of forecasting days                  #
#     Seventh:   HUMID: YES or NO (humidity will be perturbed)      #
#       Eigth: NUMPERT: number of random perturbations              #
#                                                                   #
#              LABELx : yyyymmddhh                                  #
#                       yyyy = four digit year                      #
#                       mm = two digit month                        #
#                       dd = two digit day                          #
#                       hh = two digit hour                         #
#                                                                   #
#       Obs.: For clean, run first runfftp.sx6                      #
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
   echo "argument is not set (H)"
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

export NUM=${PREFIC:0:2}
export PT=${PREFIC:2:1}
export NUMPERT=${NPERT}

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
     18) KR=18 ; LR=11 ;;
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

MPHN=1; MPTR=1; MPHS=1; MPSAN=1; MPSAS=1
MTHN=1; MTTR=1; MTHS=1; MTSAN=1; MTSAS=1
MQHN=1; MQTR=1; MQHS=1; MQSAN=1; MQSAS=1
MUHN=1; MUTR=1; MUHS=1; MUSAN=1; MUSAS=1
MVHN=1; MVTR=1; MVHS=1; MVSAN=1; MVSAS=1

export PBS_SERVER=aux20-eth4

cd ${HOME_suite}/run

RUNTM=$(date +"%s")

SCRIPTSFILES=setdeco${PERR}${RESOL}${NIVEL}.${LABELI}

cat <<EOT0 > ${SCRIPTSFILES}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:        setdeco${PERR}${RESOL}${NIVEL}.${MAQUI}   #
#                                                            #
#     Function:    This script file is used to set the       #
#                  environmental variables and start the     #
#                  decomposition script.                     #
#                                                            #
#************************************************************#
#
#PBS -o ${SC2_suite}/decanl/output/setdeco${PERR}${RESOL}${NIVEL}${LABELI}.${MAQUI}.${RUNTM}.out
#PBS -e ${SC2_suite}/decanl/output/setdeco${PERR}${RESOL}${NIVEL}${LABELI}.${MAQUI}.${RUNTM}.err
#PBS -l walltime=0:15:00
#PBS -l mppnppn=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N  ENSDECOL${NUM}
#PBS -q ${QUEUE1}

. ${FILEENV} ${1} ${2}

#
#
#   Set date (year,month,day) and hour (hour:minute) 
#
#   DATE=yymmdd
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
NUM=${NUM}
export NUM
LABELI=${LABELI}
echo \${LABELI}
export LABELI
#
#   Prefix names for the FORTRAN files
#
#   NAMEL - List file name prefix
#   GNAME - Initial condition file name prefix
#   NAMER - Input gridded file name prefix
#   NAMES - Output spectral file name prefix
#
NAMEL=GEOFPE\${NUM}
export NAMEL
GNAME=GANL\${PREFXI}
export GNAME
NAMER=GANL\${PREFXI}
export NAMER
NAMES1=GANL\${NUM}P
export NAMES1
NAMES3=GANL\${NUM}N
export NAMES3
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
ERR1=P.rp1
ERR2=P.rp2
ERR3=P.rp3
export ERR1 ERR2 ERR3
ERS1=S.rp1
ERS2=S.rp2
ERS3=S.rp3
export ERS1 ERS2 ERS3
#
#   Set directories

echo \${HOME_suite}
echo \${SC1_suite}
echo \${SC2_suite}
echo \${SC1_suite}/model/datain

#
cd \${HOME_suite}/run
#
#   Set Horizontal Truncation and Vertical Layers
#
export TRUNC=`echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' `
export LEV=`echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' `
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
echo \$COMPILE
export COMPILE
#
#   Set FORTRAN compilation flags
#
#   -integer_size 64 sets the integer basic numeric size to 8 bytes
#   -real_size 64    sets the real basic numeric size to 8 bytes
#
#
#FTNFLAG='-Wf" -pvctl noaltcode -O nodiv nomove "'
#FTNFLAG='  -h byteswapio -s integer64  -s real64 '
FTNFLAG='  -byteswapio -i8 -r8 '
#FTNFLAG=' -Mbyteswapio -i8 -r8 -O0 -g -Mbounds -Mchkstk -Mchkptr -Mchkfpstk  -Minform=inform -C  -Minline=reshape  -tp k8-64 -traceback'

export FTNFLAG
#
#   Set C pre-processing flags
#
INC=\${HOME_suite}/include/\${TRUNC}\${LEV}
mkdir -p \${INC}
CPP=" -I\${INC}"
export INC CPP
#
#   Set FORTRAN compiler name
#
F77="ftn "
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
F_UFMTIEEE=10,11,20,62,64,72,74
F_UFMTADJUST10=TYPE2
F_UFMTADJUST11=TYPE2
F_UFMTADJUST20=TYPE2
F_UFMTADJUST62=TYPE2
F_UFMTADJUST64=TYPE2
F_UFMTADJUST72=TYPE2
F_UFMTADJUST74=TYPE2
export F_UFMTIEEE
export F_UFMTADJUST10 F_UFMTADJUST11 F_UFMTADJUST20
export F_UFMTADJUST62 F_UFMTADJUST64 F_UFMTADJUST72 F_UFMTADJUST74
#  Now, build the necessary INCLUDE for the choosen truncation and 
#       vertical resolution.. 
#
  if [ "\${COMPILE}" != "run" ]
  then
#
cd \${INC}
#
cat <<EOT1 > deceof.n
      INTEGER IMAX,JMAX,MEND,KMAX,LMAX,NREG,NVARP
      PARAMETER (IMAX=${IR},JMAX=${JR},MEND=${MR},KMAX=${KR},LMAX=${LR})
      PARAMETER (NVARP=5,NREG=5)
      INTEGER JN(NREG),JS(NREG),IW(NREG),IE(NREG)
EOT1
if (diff deceof.n deceof.h > /dev/null)
then
    echo "deceof.n and deceof.h are the same"
    rm -f deceof.n
else
    echo "deceof.n and deceof.h are different"
    mv deceof.n deceof.h
fi
cat <<EOT1 > deceof2.n
      JN(1)=1  ;JN(2)=76 ;JN(3)=118;JN(4)=75 ;JN(5)=109
      JS(1)=75 ;JS(2)=117;JS(3)=192;JS(4)=108;JS(5)=161
      IW(1)=1  ;IW(2)=1  ;IW(3)=1  ;IW(4)=276;IW(5)=266
      IE(1)=384;IE(2)=384;IE(3)=384;IE(4)=372;IE(5)=362
EOT1
if (diff deceof2.n deceof2.h > /dev/null)
then
    echo "deceof2.n and deceof2.h are the same"
    rm -f deceof2.n
else
    echo "deceof2.n and deceof2.h are different"
    mv deceof2.n deceof2.h
fi
#
#  End of includes
#
fi
#
cd \${HOME_suite}/run
#
#  Now, build the necessary NAMELIST input:
#
GNAMEL=\${NAMEL}\${LABELI}\${EXTL}.\${TRUNC}\${LEV}
#
#
cat <<EOT3 > \${SC1_suite}/deceof/datain/deceof\${NUM}.nml
 &DATAIN
  GNAMEL='\${GNAMEL} '
  DIRL='\${SC1_suite}/deceof/datain/ '
  DIRI='\${SC1_suite}/model/datain/ '
  DIRG='\${SC2_suite}/eof/dataout/\${TRUNC}\${LEV}/ '
  DIRS='\${SC1_suite}/model/datain/ '
 &END
 &HUMIDI
  HUM='${HUMID}'
 &END
EOT3
#
#cd \${HOME_suite}/run
#
#   Run Decomposition
#
#\${HOME_suite}/deceof/scripts/deceof.scr
#
#
filephn=prssehn\${NUM}${MPHN}\${LABELI}
fileptr=prssetr\${NUM}${MPTR}\${LABELI}
filephs=prssehs\${NUM}${MPHS}\${LABELI}
filepsan=prssesan\${NUM}${MPSAN}\${LABELI}
filepsas=prssesas\${NUM}${MPSAS}\${LABELI}
echo 'filephn= '\${filephn} 
echo 'fileptr= '\${fileptr} 
echo 'filephs= '\${filephs} 
echo 'filepsan='\${filepsan} 
echo 'filepsas='\${filepsas} 
filethn=tempehn\${NUM}${MTHN}\${LABELI}
filettr=tempetr\${NUM}${MTTR}\${LABELI}
fileths=tempehs\${NUM}${MTHS}\${LABELI}
filetsan=tempesan\${NUM}${MTSAN}\${LABELI}
filetsas=tempesas\${NUM}${MTSAS}\${LABELI}
echo 'filethn= '\${filethn} 
echo 'filettr= '\${filettr} 
echo 'fileths= '\${fileths} 
echo 'filetsan='\${filetsan} 
echo 'filetsas='\${filetsas} 
fileqhn=humpehn\${NUM}${MQHN}\${LABELI}
fileqtr=humpetr\${NUM}${MQTR}\${LABELI}
fileqhs=humpehs\${NUM}${MQHS}\${LABELI}
fileqsan=humpesan\${NUM}${MQSAN}\${LABELI}
fileqsas=humpesas\${NUM}${MQSAS}\${LABELI}
echo 'fileqhn= '\${fileqhn} 
echo 'fileqtr= '\${fileqtr} 
echo 'fileqhs= '\${fileqhs} 
echo 'fileqsan='\${fileqsan} 
echo 'fileqsas='\${fileqsas} 
fileuhn=winpehn\${NUM}${MUHN}\${LABELI}
fileutr=winpetr\${NUM}${MUTR}\${LABELI}
fileuhs=winpehs\${NUM}${MUHS}\${LABELI}
fileusan=winpesan\${NUM}${MUSAN}\${LABELI}
fileusas=winpesas\${NUM}${MUSAS}\${LABELI}
echo 'fileuhn= '\${fileuhn} 
echo 'fileutr= '\${fileutr} 
echo 'fileuhs= '\${fileuhs} 
echo 'fileusan='\${fileusan} 
echo 'fileusas='\${fileusas} 
filevhn=winpehn\${NUM}${MVHN}\${LABELI}
filevtr=winpetr\${NUM}${MVTR}\${LABELI}
filevhs=winpehs\${NUM}${MVHS}\${LABELI}
filevsan=winpesan\${NUM}${MVSAN}\${LABELI}
filevsas=winpesas\${NUM}${MVSAS}\${LABELI}
echo 'filevhn= '\${filevhn} 
echo 'filevtr= '\${filevtr} 
echo 'filevhs= '\${filevhs} 
echo 'filevsan='\${filevsan} 
echo 'filevsas='\${filevsas} 
#
rm -f  \${SC1_suite}/deceof/datain/\${GNAMEL}
cat <<EOT2 > \${SC1_suite}/deceof/datain/\${GNAMEL}
\${GNAME}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
\${filephn}
\${fileptr}
\${filephs}
\${filepsan} 
\${filepsas}
\${filethn}
\${filettr}
\${fileths}
\${filetsan}
\${filetsas}
\${fileqhn}
\${fileqtr}
\${fileqhs}
\${fileqsan}
\${fileqsas}
\${fileuhn}
\${fileutr}
\${fileuhs}
\${fileusan}
\${fileusas}
\${filevhn}
\${filevtr}
\${filevhs}
\${filevsan}
\${filevsas}
\${NAMES1}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
EOT2
#
#   Run Decomposition
#
\${HOME_suite}/deceof/scripts/deceof.scr
#
#
#
filephn=prssnhn\${NUM}${MPHN}\${LABELI}
fileptr=prssntr\${NUM}${MPTR}\${LABELI}
filephs=prssnhs\${NUM}${MPHS}\${LABELI}
filepsan=prssnsan\${NUM}${MPSAN}\${LABELI}
filepsas=prssnsas\${NUM}${MPSAS}\${LABELI}
echo 'filephn= '\${filephn} 
echo 'fileptr= '\${fileptr} 
echo 'filephs= '\${filephs} 
echo 'filepsan='\${filepsan} 
echo 'filepsas='\${filepsas} 
filethn=tempnhn\${NUM}${MTHN}\${LABELI}
filettr=tempntr\${NUM}${MTTR}\${LABELI}
fileths=tempnhs\${NUM}${MTHS}\${LABELI}
filetsan=tempnsan\${NUM}${MTSAN}\${LABELI}
filetsas=tempnsas\${NUM}${MTSAS}\${LABELI}
echo 'filethn= '\${filethn} 
echo 'filettr= '\${filettr} 
echo 'fileths= '\${fileths} 
echo 'filetsan='\${filetsan} 
echo 'filetsas='\${filetsas} 
fileqhn=humpnhn\${NUM}${MQHN}\${LABELI}
fileqtr=humpntr\${NUM}${MQTR}\${LABELI}
fileqhs=humpnhs\${NUM}${MQHS}\${LABELI}
fileqsan=humpnsan\${NUM}${MQSAN}\${LABELI}
fileqsas=humpnsas\${NUM}${MQSAS}\${LABELI}
echo 'fileqhn= '\${fileqhn} 
echo 'fileqtr= '\${fileqtr} 
echo 'fileqhs= '\${fileqhs} 
echo 'fileqsan='\${fileqsan} 
echo 'fileqsas='\${fileqsas} 
fileuhn=winpnhn\${NUM}${MUHN}\${LABELI}
fileutr=winpntr\${NUM}${MUTR}\${LABELI}
fileuhs=winpnhs\${NUM}${MUHS}\${LABELI}
fileusan=winpnsan\${NUM}${MUSAN}\${LABELI}
fileusas=winpnsas\${NUM}${MUSAS}\${LABELI}
echo 'fileuhn= '\${fileuhn} 
echo 'fileutr= '\${fileutr} 
echo 'fileuhs= '\${fileuhs} 
echo 'fileusan='\${fileusan} 
echo 'fileusas='\${fileusas} 
filevhn=winpnhn\${NUM}${MVHN}\${LABELI}
filevtr=winpntr\${NUM}${MVTR}\${LABELI}
filevhs=winpnhs\${NUM}${MVHS}\${LABELI}
filevsan=winpnsan\${NUM}${MVSAN}\${LABELI}
filevsas=winpnsas\${NUM}${MVSAS}\${LABELI}
echo 'filevhn= '\${filevhn} 
echo 'filevtr= '\${filevtr} 
echo 'filevhs= '\${filevhs} 
echo 'filevsan='\${filevsan} 
echo 'filevsas='\${filevsas} 
#
rm -f  \${SC1_suite}/deceof/datain/\${GNAMEL}
cat <<EOT4 > \${SC1_suite}/deceof/datain/\${GNAMEL}
\${GNAME}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
\${filephn}
\${fileptr}
\${filephs}
\${filepsan}
\${filepsas}
\${filethn}
\${filettr}
\${fileths}
\${filetsan}
\${filetsas}
\${fileqhn}
\${fileqtr}
\${fileqhs}
\${fileqsan}
\${fileqsas}
\${fileuhn}
\${fileutr}
\${fileuhs}
\${fileusan}
\${fileusas}
\${filevhn}
\${filevtr}
\${filevhs}
\${filevsan}
\${filevsas}
\${NAMES3}\${LABELI}\${EXTG}.\${TRUNC}\${LEV}
EOT4
#
#   Run Decomposition
#
\${HOME_suite}/deceof/scripts/deceof.scr

EOT0
#
#   Change mode to be executable
#
chmod 744 ${HOME_suite}/run/${SCRIPTSFILES}
#
#   Submit Decomposition scripts to Batch
#
echo "${HOME_suite}/run/${SCRIPTSFILES}"
${HOME_suite}/run/${SCRIPTSFILES}
#

exit 0
