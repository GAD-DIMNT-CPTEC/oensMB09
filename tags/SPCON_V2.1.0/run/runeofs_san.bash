#!/bin/bash -x
#help#
#******************************************************************#
#                                                                  #
#     Name:           runeofs.sx6                                  #
#                                                                  #
#     Function:       This script submits the                      #
#                     temperature and wind eof                     #
#                                                                  #
#                     It runs in Korn Shell.                       #
#                                                                  #
#     Date:           June      02th, 2003.                        #
#     Last change:    June      02th, 2003.                        #
#                                                                  #
#     Valid Arguments for runeofs.sx6                              #
#                                                                  #
#     First  :   HELP: help or nothing for getting help            #
#     First  :COMPILE: help, make, clean or run                    #
#     Second :    TRC: three-digit triangular truncation           #
#     Third  :     LV: two-digit number of vertical sigma-layers   #
#     Fourth :  HUMID: YES or NO (humidity will be perturbed)      #
#     Fifth  :    NUM: pertubation number                          #
#     Sixth  : LABELI: initial forecasting label                   #
#     Seventh: NFDAYS: number of forecasting days                  #
#     Eigth  :NUMPERT: number of random perturbations              #
#     hold   :Total  : number of random perturbations              #
#                                                                  #
#              LABELx: yyyymmddhh                                  #
#                      yyyy = four digit year                      #
#                      mm = two digit month                        #
#                      dd = two digit day                          #
#                      hh = two digit hour                         #
#                                                                  #
#******************************************************************#
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

#
#   Set directories
#
#   HOME_suite - HOME DA SUITE
#   SC1_suite - /scratchin
#   SC2_suite - /scratchout
#

if [ -z "${2}" ]; then
   echo "Second argument is not set: PREFIC"
   exit
else
   PREFIC=${2}
fi

if [ -z "${3}" ]; then
   echo "Third argument is not set: TYPES"
   exit
else
   HUMID=${3}
fi
if [ -z "${4}" ]; then
   echo "Fourth argument is not set (LABELI: yyyymmddhh)"
   exit
else
   LABELI=${4}
fi

export NUM=${PREFIC:0:2}
export PT=${PREFIC:2:1}
export NUMPERT=${NPERT}

case ${TRC} in
21) MR=21 ; IR=64 ; JR=32 ; JI=11 ; JS=24 ; NJ=14 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=11 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
30) MR=30 ; IR=96 ; JR=48 ; JI=16 ; JS=36 ; NJ=21 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
42) MR=42 ; IR=128 ; JR=64 ; JI=22 ; JS=48 ; NJ=27 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=11 ;;
     28) KR=28 ; LR=11 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
47) MR=47 ; IR=144 ; JR=72 ; JI=24 ; JS=54 ; NJ=31 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
62) MR=62 ; IR=192 ; JR=96 ; JI=32 ; JS=72 ; NJ=41 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=11 ;;
     28) KR=28 ; LR=11 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
79) MR=79 ; IR=240 ; JR=120 ; JI=40 ; JS=90 ; NJ=51 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
95) MR=95 ; IR=288 ; JR=144 ; JI=48 ; JS=108 ; NJ=61 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
106) MR=106 ; IR=320 ; JR=160 ; JI=54 ; JS=120 ; NJ=67 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
126) MR=126 ; IR=384 ; JR=192 ; II=276 ; IS=372 ; NI=97 ; JI=75 ; JS=108 ; NJ=34 ; 
     IIPS=276 ; ISPS=316 ; JIPS=75 ; JSPS=108 ; REG=san ; 
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=11 ;;
     28) KR=28 ; LR=11 ;;
     42) KR=42 ; LR=11 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
159) MR=159 ; IR=480 ; JR=240 ; JI=80 ; JS=180 ; NJ=101 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
170) MR=170 ; IR=512 ; JR=256 ; JI=86 ; JS=192 ; NJ=107
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
213) MR=213 ; IR=640 ; JR=320 ; JI=107 ; JS=240 ; NJ=134 ;
     case ${LV} in
     09) KR=09 ; LR=11 ;;
     18) KR=18 ; LR=13 ;;
     28) KR=28 ; LR=17 ;;
     42) KR=42 ; LR=18 ;;
     *) echo "Wrong request for vertical resolution: ${LV}" ; exit 1 ;;
     esac
;;
319) MR=319 ; IR=960 ; JR=480 ; JI=160 ; JS=360 ; NJ=201 ;
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
cd ${HOME_suite}/run
#
mkdir -p ${SC2_suite}/eof/output
mkdir -p ${SC2_suite}/eof/dataout/${RESOL}${NIVEL}/

SCRIPTNAME=seteofs_san.${PREFIC}${RESOL}${NIVEL}.${LABELI}
export PBS_SERVER=aux20-eth4
RUNTM=$(date +"%s")

export PBS_SERVER=aux20-eth4
cat <<EOT0 > ${HOME_suite}/run/${SCRIPTNAME}
#!/bin/bash -x
#
#************************************************************#
#                                                            #
#     Name:        seteofs${PREFIC}${RESOL}${NIVEL}.${MACHINE} #
#                                                            #
#************************************************************#
#
#PBS -o ${SC2_suite}/eof/output/seteofs_hn.${PREFIC}.${LABELI}.${RUNTM}.out
#PBS -e ${SC2_suite}/eof/output/seteofs_hn.${PREFIC}.${LABELI}.${RUNTM}.err
#PBS -l select=1:ncpus=1
#PBS -V
#PBS -A CPTEC
#PBS -S /bin/bash
#PBS -N ENSEOFS_SAN${NUM}
#PBS -q ${QUEUE1}

#
#   Set date (year,month,day) and hour (hour:minute) 
#
#   DATE=yyyymmdd
#   HOUR=hh:mn
#
#export PATH=".:/usr/local/sxbin:~/bin:/usr/local/nec/tools:/usr/psuite:/SX/usr/bin:$PATH"

. ${FILEENV} ${1} ${2}

NUM=${NUM}
echo \${NUM}
export NUM
TRUNC=${RESOL}
LEV=${NIVEL}
export TRUNC LEV
REGE=${REG}
export REGE
#
#
#  Set labels (date, UTC hour, ...)
#
DATE=`date +'%Y'``date +'%m'``date +'%d'`
HOUR=`date +'%H:%M'`
echo 'Date: '\$DATE
echo 'Hour: '\$HOUR
export DATE HOUR
#
#   LABELI = yyyymmddhh
#   LABELI = input file start label
LABELI=${LABELI}
echo \${LABELI}
export LABELI

#ANLDATAPREFIX=${PREFIXANLDATA}
ANLDATAPREFIX=${PREFXI}
echo \${ANLDATAPREFIX}
#
cd ${HOME_suite}/run
#
#   Set Horizontal Truncation and Vertical Layers
#
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
export COMPILE=run
echo \${COMPILE}
export COMPILE
#
#   Set FORTRAN compilation flags
#
#   -integer_size 64 sets the integer basic numeric size to 8 bytes
#   -real_size 64    sets the real basic numeric size to 8 bytes
#
#
#FTNFLAG='-C vsafe -float0 -ew -Wf" -pvctl noaltcode nomatmul -O nodiv nomove " '
#CHWFLAG='-C vsafe -float0 -ew -Wf" -pvctl noaltcode nomatmul -O nodiv nomove " '
#FTNFLAG='  -h byteswapio -s real64  -s integer64 '
#CHWFLAG='  -h byteswapio -s real64  -s integer64 '
FTNFLAG='  -byteswapio -i8 -r8 '
CHWFLAG='  -byteswapio -i8 -r8 '

export FTNFLAG CHWFLAG
#
#   Set C pre-processing flags
#
mkdir -p \${HOME_suite}/include/${RESOL}${NIVEL}
INC=\${HOME_suite}/include/${RESOL}${NIVEL}
CPP=" -I\${INC}"
export INC CPP
#
#   Set FORTRAN compiler name
#
#F77="sxf90 -V -float0 -ew "
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
#  Now, build the necessary INCLUDE for the choosen truncation and 
#       vertical resolution.. 
#
  if [ "\${COMPILE}" != "run" ]
  then
#
cd \${INC}
#
cat <<EOT > reseofes.n
      INTEGER IMAX,JMAX,MEND,KMAX,LMAX
      PARAMETER (IMAX=${IR},JMAX=${JR},KMAX=${KR})
      PARAMETER (MEND=${MR},LMAX=${LR})
EOT
if (diff reseofes.n reseofes.inc > /dev/null)
then
    echo "reseofes.n and reseofes.inc are the same"
    rm -f reseofes.n
else
    echo "reseofes.n and reseofes.inc are different"
    mv reseofes.n reseofes.inc
fi
cat <<EOT > nvector.n
      INTEGER LMAX
      PARAMETER (LMAX=${LR})
EOT
if (diff nvector.n nvector.inc > /dev/null)
then
    echo "nvector.n and nvector.inc are the same"
    rm -f nvector.n
else
    echo "nvector.n and nvector.inc are different"
    mv nvector.n nvector.inc
fi
#
#  End of includes
#
fi
#
#  Now, build the necessary NAMELIST input:
# 
#
cd ${SC2_suite}/recfct/dataout/${RESOL}${NIVEL}/\${LABELI}
ext=R.fct.${RESOL}${NIVEL}
echo ${ext}

rm -f ${SC1_suite}/eof/datain/templ${REG}\${NUM}\${LABELI} ${SC1_suite}/eof/datain/templ\${NUM}\${LABELI}
for i in \$( ls -1 GFCT\${NUM}R\${LABELI}*fct* | cut -c18-27); do
   echo \$i
cat <<EOT1 >> ${SC1_suite}/eof/datain/templ${REG}\${NUM}\${LABELI}
${SC2_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}/GFCTCTR\${LABELI}\${i}\${ext}
${SC2_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}/GFCT\${NUM}R\${LABELI}\${i}\${ext}
EOT1
done

echo ${SC1_suite}/eof/datain/templ${REG}\${NUM}\${LABELI}
cp ${SC1_suite}/eof/datain/templ${REG}\${NUM}\${LABELI} ${SC1_suite}/eof/datain/templ\${NUM}\${LABELI}

cd \${SC1_suite}/eof/datain
ext=R.unf
cat <<EOT1 > eofpres${REG}\${NUM}.nml
 &DATAIN
  DIRI='\${SC1_suite}/eof/datain/ '
  DIRA='\${SC1_suite}/model/datain/ '
  DIRO='\${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ\${NUM}\${LABELI}'
  ANAME='GANL${PREFXI}\${LABELI}\${ext}.${RESOL}${NIVEL} '
  PRSOUT='prsout${REG}\${NUM}\${LABELI} '
  PRSSCM='prsscm${REG}\${NUM}\${LABELI} '
 &END
 &PRSSER
  PRSSER1(1)='prsse${REG}\${NUM}1\${LABELI} ',
  PRSSER1(2)='prsse${REG}\${NUM}2\${LABELI} ',
  PRSSER1(3)='prsse${REG}\${NUM}3\${LABELI} ',
  PRSSER1(4)='prsse${REG}\${NUM}4\${LABELI} ',
  PRSSER1(5)='prsse${REG}\${NUM}5\${LABELI} ',
  PRSSER1(6)='prsse${REG}\${NUM}6\${LABELI} ',
  PRSSER1(7)='prsse${REG}\${NUM}7\${LABELI} ',
  PRSSER1(8)='prsse${REG}\${NUM}8\${LABELI} ',
  PRSSER1(9)='prsse${REG}\${NUM}9\${LABELI} ',
  PRSSER1(10)='prsse${REG}\${NUM}10\${LABELI} ',
  PRSSER1(11)='prsse${REG}\${NUM}11\${LABELI} '
 &END
 &PRSSEN
  PRSSEN1(1)='prssn${REG}\${NUM}1\${LABELI} ',
  PRSSEN1(2)='prssn${REG}\${NUM}2\${LABELI} ',
  PRSSEN1(3)='prssn${REG}\${NUM}3\${LABELI} ',
  PRSSEN1(4)='prssn${REG}\${NUM}4\${LABELI} ',
  PRSSEN1(5)='prssn${REG}\${NUM}5\${LABELI} ',
  PRSSEN1(6)='prssn${REG}\${NUM}6\${LABELI} ',
  PRSSEN1(7)='prssn${REG}\${NUM}7\${LABELI} ',
  PRSSEN1(8)='prssn${REG}\${NUM}8\${LABELI} ',
  PRSSEN1(9)='prssn${REG}\${NUM}9\${LABELI} ',
  PRSSEN1(10)='prssn${REG}\${NUM}10\${LABELI} ',
  PRSSEN1(11)='prssn${REG}\${NUM}11\${LABELI} '
 &END
 &STPRES
  STDP=1.00
 &END
 &PARMET
  IINF=${II},ISUP=${IS},IMAX0=${NI},
  JINF=${JI},JSUP=${JS},JMAX0=${NJ},
  IIPS=${IIPS},ISPS=${ISPS},JIPS=${JIPS},JSPS=${JSPS},
 &END
EOT1
echo 'Vai chamar eofp'
\${HOME_suite}/eofpres/scripts/eofpres.scr


cd \${SC1_suite}/eof/datain
ext=R.unf
cat <<EOT1 > eoftem${REG}\${NUM}.nml
 &DATAIN
  DIRI='\${SC1_suite}/eof/datain/ '
  DIRA='\${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRO='\${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ${REG}\${NUM}\${LABELI} '
  ANAME='GANL${PREFXI}\${LABELI}\${ext}.${RESOL}${NIVEL} '
  TEMOUT='temout${REG}\${NUM}\${LABELI} '
  TEMPCM='tempcm${REG}\${NUM}\${LABELI} '
 &END
 &TEMPER
  TEMPER1(1)='tempe${REG}\${NUM}1\${LABELI} ',
  TEMPER1(2)='tempe${REG}\${NUM}2\${LABELI} ',
  TEMPER1(3)='tempe${REG}\${NUM}3\${LABELI} ',
  TEMPER1(4)='tempe${REG}\${NUM}4\${LABELI} ',
  TEMPER1(5)='tempe${REG}\${NUM}5\${LABELI} ',
  TEMPER1(6)='tempe${REG}\${NUM}6\${LABELI} ',
  TEMPER1(7)='tempe${REG}\${NUM}7\${LABELI} ',
  TEMPER1(8)='tempe${REG}\${NUM}8\${LABELI} ',
  TEMPER1(9)='tempe${REG}\${NUM}9\${LABELI} ',
  TEMPER1(10)='tempe${REG}\${NUM}10\${LABELI} ',
  TEMPER1(11)='tempe${REG}\${NUM}11\${LABELI} '
 &END
 &TEMPEN
  TEMPEN1(1)='tempn${REG}\${NUM}1\${LABELI} ',
  TEMPEN1(2)='tempn${REG}\${NUM}2\${LABELI} ',
  TEMPEN1(3)='tempn${REG}\${NUM}3\${LABELI} ',
  TEMPEN1(4)='tempn${REG}\${NUM}4\${LABELI} ',
  TEMPEN1(5)='tempn${REG}\${NUM}5\${LABELI} ',
  TEMPEN1(6)='tempn${REG}\${NUM}6\${LABELI} ',
  TEMPEN1(7)='tempn${REG}\${NUM}7\${LABELI} ',
  TEMPEN1(8)='tempn${REG}\${NUM}8\${LABELI} ',
  TEMPEN1(9)='tempn${REG}\${NUM}9\${LABELI} ',
  TEMPEN1(10)='tempn${REG}\${NUM}10\${LABELI} ',
  TEMPEN1(11)='tempn${REG}\${NUM}11\${LABELI} '
 &END
 &STTEMP
  STDT( 1)=1.50,STDT( 2)=1.50,STDT( 3)=1.50,STDT( 4)=1.50,STDT( 5)=1.50,
  STDT( 6)=1.50,STDT( 7)=1.50,STDT( 8)=1.50,STDT( 9)=1.50,STDT(10)=1.50,
  STDT(11)=1.50,STDT(12)=1.50,STDT(13)=1.50,STDT(14)=1.50,STDT(15)=1.50,
  STDT(16)=1.50,STDT(17)=1.50,STDT(18)=1.50,STDT(19)=1.50,STDT(20)=1.50,
  STDT(21)=1.50,STDT(22)=1.50,STDT(23)=1.50,STDT(24)=1.50,STDT(25)=1.50,
  STDT(26)=1.50,STDT(27)=1.50,STDT(28)=1.50
 &END
 &PARMET
  IINF=${II},ISUP=${IS},IMAX0=${NI},
  JINF=${JI},JSUP=${JS},JMAX0=${NJ},
  IIPS=${IIPS},ISPS=${ISPS},JIPS=${JIPS},JSPS=${JSPS}
 &END
EOT1

echo 'Vai chamar eoft'

\${HOME_suite}/eoftemp/scripts/eoftem.scr

#

if [ "${HUMID}" = "YES" ] 
then
#  Now, build the necessary NAMELIST input:
#
#
cd \${SC1_suite}/eof/datain
ext=R.unf
cat <<EOT1 > eofhum${REG}\${NUM}.nml
 &DATAIN
  DIRI='\${SC1_suite}/eof/datain/ '
  DIRA='\${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRO='\${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ${REG}\${NUM}\${LABELI} '
  ANAME='GANL${PREFXI}\${LABELI}\${ext}.${RESOL}${NIVEL} '
  HUMOUT='humout${REG}\${NUM}\${LABELI} '
  HUMPCM='humpcm${REG}\${NUM}\${LABELI} '
 &END
 &HUMPER
  HUMPER1(1)='humpe${REG}\${NUM}1\${LABELI} ',
  HUMPER1(2)='humpe${REG}\${NUM}2\${LABELI} ',
  HUMPER1(3)='humpe${REG}\${NUM}3\${LABELI} ',
  HUMPER1(4)='humpe${REG}\${NUM}4\${LABELI} ',
  HUMPER1(5)='humpe${REG}\${NUM}5\${LABELI} ',
  HUMPER1(6)='humpe${REG}\${NUM}6\${LABELI} ',
  HUMPER1(7)='humpe${REG}\${NUM}7\${LABELI} ',
  HUMPER1(8)='humpe${REG}\${NUM}8\${LABELI} ',
  HUMPER1(9)='humpe${REG}\${NUM}9\${LABELI} ',
  HUMPER1(10)='humpe${REG}\${NUM}10\${LABELI} ',
  HUMPER1(11)='humpe${REG}\${NUM}11\${LABELI} '
 &END
 &HUMPEN
  HUMPEN1(1)='humpn${REG}\${NUM}1\${LABELI} ',
  HUMPEN1(2)='humpn${REG}\${NUM}2\${LABELI} ',
  HUMPEN1(3)='humpn${REG}\${NUM}3\${LABELI} ',
  HUMPEN1(4)='humpn${REG}\${NUM}4\${LABELI} ',
  HUMPEN1(5)='humpn${REG}\${NUM}5\${LABELI} ',
  HUMPEN1(6)='humpn${REG}\${NUM}6\${LABELI} ',
  HUMPEN1(7)='humpn${REG}\${NUM}7\${LABELI} ',
  HUMPEN1(8)='humpn${REG}\${NUM}8\${LABELI} ',
  HUMPEN1(9)='humpn${REG}\${NUM}9\${LABELI} ',
  HUMPEN1(10)='humpn${REG}\${NUM}10\${LABELI} ',
  HUMPEN1(11)='humpn${REG}\${NUM}11\${LABELI} '
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
 &PARMET
  IINF=${II},ISUP=${IS},IMAX0=${NI},
  JINF=${JI},JSUP=${JS},JMAX0=${NJ},
  IIPS=${IIPS},ISPS=${ISPS},JIPS=${JIPS},JSPS=${JSPS}
 &END
EOT1
#
echo 'Vai chamar eofh'
#
\${HOME_suite}/eofhumi/scripts/eofhum.scr
#
fi

#
#  Now, build the necessary NAMELIST input:
#
#

cd \${SC1_suite}/eof/datain
ext=R.unf

cat <<EOT1 > eofwin${REG}\${NUM}.nml
 &DATAIN
  DIRI='\${SC1_suite}/eof/datain/ '
  DIRA='\${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRO='\${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ${REG}\${NUM}\${LABELI} '
  ANAME='GANL${PREFXI}\${LABELI}\${ext}.${RESOL}${NIVEL} '
  WINOUT='winout${REG}\${NUM}\${LABELI} '
  WINPCM='winpcm${REG}\${NUM}\${LABELI} '
 &END
 &WINPER
  WINPER1(1)='winpe${REG}\${NUM}1\${LABELI} ',
  WINPER1(2)='winpe${REG}\${NUM}2\${LABELI} ',
  WINPER1(3)='winpe${REG}\${NUM}3\${LABELI} ',
  WINPER1(4)='winpe${REG}\${NUM}4\${LABELI} ',
  WINPER1(5)='winpe${REG}\${NUM}5\${LABELI} ',
  WINPER1(6)='winpe${REG}\${NUM}6\${LABELI} ',
  WINPER1(7)='winpe${REG}\${NUM}7\${LABELI} ',
  WINPER1(8)='winpe${REG}\${NUM}8\${LABELI} ',
  WINPER1(9)='winpe${REG}\${NUM}9\${LABELI} ',
  WINPER1(10)='winpe${REG}\${NUM}10\${LABELI} ',
  WINPER1(11)='winpe${REG}\${NUM}11\${LABELI} '
 &END
 &WINPEN
  WINPEN1(1)='winpn${REG}\${NUM}1\${LABELI} ',
  WINPEN1(2)='winpn${REG}\${NUM}2\${LABELI} ',
  WINPEN1(3)='winpn${REG}\${NUM}3\${LABELI} ',
  WINPEN1(4)='winpn${REG}\${NUM}4\${LABELI} ',
  WINPEN1(5)='winpn${REG}\${NUM}5\${LABELI} ',
  WINPEN1(6)='winpn${REG}\${NUM}6\${LABELI} ',
  WINPEN1(7)='winpn${REG}\${NUM}7\${LABELI} ',
  WINPEN1(8)='winpn${REG}\${NUM}8\${LABELI} ',
  WINPEN1(9)='winpn${REG}\${NUM}9\${LABELI} ',
  WINPEN1(10)='winpn${REG}\${NUM}10\${LABELI} ',
  WINPEN1(11)='winpn${REG}\${NUM}11\${LABELI} '
 &END
 &PRSSER
  PRSSER1(1)='prsse${REG}\${NUM}1\${LABELI} ',
  PRSSER1(2)='prsse${REG}\${NUM}2\${LABELI} ',
  PRSSER1(3)='prsse${REG}\${NUM}3\${LABELI} ',
  PRSSER1(4)='prsse${REG}\${NUM}4\${LABELI} ',
  PRSSER1(5)='prsse${REG}\${NUM}5\${LABELI} ',
  PRSSER1(6)='prsse${REG}\${NUM}6\${LABELI} ',
  PRSSER1(7)='prsse${REG}\${NUM}7\${LABELI} ',
  PRSSER1(8)='prsse${REG}\${NUM}8\${LABELI} ',
  PRSSER1(9)='prsse${REG}\${NUM}9\${LABELI} ',
  PRSSER1(10)='prsse${REG}\${NUM}10\${LABELI} ',
  PRSSER1(11)='prsse${REG}\${NUM}11\${LABELI} '
 &END
 &PRSSEN
  PRSSEN1(1)='prssn${REG}\${NUM}1\${LABELI} ',
  PRSSEN1(2)='prssn${REG}\${NUM}2\${LABELI} ',
  PRSSEN1(3)='prssn${REG}\${NUM}3\${LABELI} ',
  PRSSEN1(4)='prssn${REG}\${NUM}4\${LABELI} ',
  PRSSEN1(5)='prssn${REG}\${NUM}5\${LABELI} ',
  PRSSEN1(6)='prssn${REG}\${NUM}6\${LABELI} ',
  PRSSEN1(7)='prssn${REG}\${NUM}7\${LABELI} ',
  PRSSEN1(8)='prssn${REG}\${NUM}8\${LABELI} ',
  PRSSEN1(9)='prssn${REG}\${NUM}9\${LABELI} ',
  PRSSEN1(10)='prssn${REG}\${NUM}10\${LABELI} ',
  PRSSEN1(11)='prssn${REG}\${NUM}11\${LABELI} '
 &END
 &TEMPER
  TEMPER1(1)='tempe${REG}\${NUM}1\${LABELI} ',
  TEMPER1(2)='tempe${REG}\${NUM}2\${LABELI} ',
  TEMPER1(3)='tempe${REG}\${NUM}3\${LABELI} ',
  TEMPER1(4)='tempe${REG}\${NUM}4\${LABELI} ',
  TEMPER1(5)='tempe${REG}\${NUM}5\${LABELI} ',
  TEMPER1(6)='tempe${REG}\${NUM}6\${LABELI} ',
  TEMPER1(7)='tempe${REG}\${NUM}7\${LABELI} ',
  TEMPER1(8)='tempe${REG}\${NUM}8\${LABELI} ',
  TEMPER1(9)='tempe${REG}\${NUM}9\${LABELI} ',
  TEMPER1(10)='tempe${REG}\${NUM}10\${LABELI} ',
  TEMPER1(11)='tempe${REG}\${NUM}11\${LABELI} '
 &END
 &TEMPEN
  TEMPEN1(1)='tempn${REG}\${NUM}1\${LABELI} ',
  TEMPEN1(2)='tempn${REG}\${NUM}2\${LABELI} ',
  TEMPEN1(3)='tempn${REG}\${NUM}3\${LABELI} ',
  TEMPEN1(4)='tempn${REG}\${NUM}4\${LABELI} ',
  TEMPEN1(5)='tempn${REG}\${NUM}5\${LABELI} ',
  TEMPEN1(6)='tempn${REG}\${NUM}6\${LABELI} ',
  TEMPEN1(7)='tempn${REG}\${NUM}7\${LABELI} ',
  TEMPEN1(8)='tempn${REG}\${NUM}8\${LABELI} ',
  TEMPEN1(9)='tempn${REG}\${NUM}9\${LABELI} ',
  TEMPEN1(10)='tempn${REG}\${NUM}10\${LABELI} ',
  TEMPEN1(11)='tempn${REG}\${NUM}11\${LABELI} '
 &END
 &HUMPER
  HUMPER1(1)='humpe${REG}\${NUM}1\${LABELI} ',
  HUMPER1(2)='humpe${REG}\${NUM}2\${LABELI} ',
  HUMPER1(3)='humpe${REG}\${NUM}3\${LABELI} ',
  HUMPER1(4)='humpe${REG}\${NUM}4\${LABELI} ',
  HUMPER1(5)='humpe${REG}\${NUM}5\${LABELI} ',
  HUMPER1(6)='humpe${REG}\${NUM}6\${LABELI} ',
  HUMPER1(7)='humpe${REG}\${NUM}7\${LABELI} ',
  HUMPER1(8)='humpe${REG}\${NUM}8\${LABELI} ',
  HUMPER1(9)='humpe${REG}\${NUM}9\${LABELI} ',
  HUMPER1(10)='humpe${REG}\${NUM}10\${LABELI} ',
  HUMPER1(11)='humpe${REG}\${NUM}11\${LABELI} '
 &END
 &HUMPEN
  HUMPEN1(1)='humpn${REG}\${NUM}1\${LABELI} ',
  HUMPEN1(2)='humpn${REG}\${NUM}2\${LABELI} ',
  HUMPEN1(3)='humpn${REG}\${NUM}3\${LABELI} ',
  HUMPEN1(4)='humpn${REG}\${NUM}4\${LABELI} ',
  HUMPEN1(5)='humpn${REG}\${NUM}5\${LABELI} ',
  HUMPEN1(6)='humpn${REG}\${NUM}6\${LABELI} ',
  HUMPEN1(7)='humpn${REG}\${NUM}7\${LABELI} ',
  HUMPEN1(8)='humpn${REG}\${NUM}8\${LABELI} ',
  HUMPEN1(9)='humpn${REG}\${NUM}9\${LABELI} ',
  HUMPEN1(10)='humpn${REG}\${NUM}10\${LABELI} ',
  HUMPEN1(11)='humpn${REG}\${NUM}11\${LABELI} '
 &END
 &STZWIN
  STDU( 1)=5.00,STDU( 2)=5.00,STDU( 3)=5.00,STDU( 4)=5.00,STDU( 5)=5.00,
  STDU( 6)=5.00,STDU( 7)=5.00,STDU( 8)=5.00,STDU( 9)=5.00,STDU(10)=5.00,
  STDU(11)=5.00,STDU(12)=5.00,STDU(13)=5.00,STDU(14)=5.00,STDU(15)=5.00,
  STDU(16)=5.00,STDU(17)=5.00,STDU(18)=5.00,STDU(19)=5.00,STDU(20)=5.00,
  STDU(21)=5.00,STDU(22)=5.00,STDU(23)=5.00,STDU(24)=5.00,STDU(25)=5.00,
  STDU(26)=5.00,STDU(27)=5.00,STDU(28)=5.00
 &END
 &STMWIN
  STDV( 1)=5.00,STDV( 2)=5.00,STDV( 3)=5.00,STDV( 4)=5.00,STDV( 5)=5.00,
  STDV( 6)=5.00,STDV( 7)=5.00,STDV( 8)=5.00,STDV( 9)=5.00,STDV(10)=5.00,
  STDV(11)=5.00,STDV(12)=5.00,STDV(13)=5.00,STDV(14)=5.00,STDV(15)=5.00,
  STDV(16)=5.00,STDV(17)=5.00,STDV(18)=5.00,STDV(19)=5.00,STDV(20)=5.00,
  STDV(21)=5.00,STDV(22)=5.00,STDV(23)=5.00,STDV(24)=5.00,STDV(25)=5.00,
  STDV(26)=5.00,STDV(27)=5.00,STDV(28)=5.00
 &END
 &HUMIDI
  HUM='${HUMID}'
 &END
 &PARMET
  IINF=${II},ISUP=${IS},IMAX0=${NI},
  JINF=${JI},JSUP=${JS},JMAX0=${NJ},
  IIPS=${IIPS},ISPS=${ISPS},JIPS=${JIPS},JSPS=${JSPS}
 &END
EOT1
#

echo 'Vai chamar eofw'
#
\${HOME_suite}/eofwind/scripts/eofwin.scr
#
EOT0
#
#   Change mode to be executable
#
chmod 744 ${HOME_suite}/run/${SCRIPTNAME}
#
${HOME_suite}/run/${SCRIPTNAME}

exit 0

