#!/bin/bash -x

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

MR=126; IR=384; JR=192; II=276; IS=372; NI=97; JI=75; JS=108; NJ=34; 
IIPS=276; ISPS=316; JIPS=75; JSPS=108; REG=san; KR=28; LR=11

. ${FILEENV} ${1} ${2}

NUM=${NUM}
echo ${NUM}
export NUM

TRUNC=${RESOL}
LEV=${NIVEL}
export TRUNC LEV

REGE=${REG}
export REGE

DATE=`date +'%Y'``date +'%m'``date +'%d'`
HOUR=`date +'%H:%M'`
echo 'Date: '$DATE
echo 'Hour: '$HOUR

export DATE HOUR

LABELI=${LABELI}
echo ${LABELI}
export LABELI

ANLDATAPREFIX=${PREFXI}
echo ${ANLDATAPREFIX}

cd ${SC2_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}

ext=R.fct.${RESOL}${NIVEL}

echo ${ext}

rm -f ${SC1_suite}/eof/datain/templ${REG}${NUM}${LABELI} ${SC1_suite}/eof/datain/templ${NUM}${LABELI}

for i in `ls -1 GFCT${NUM}R${LABELI}*fct* | cut -c 18-27`
do

   echo $i

cat <<EOT1 >> ${SC1_suite}/eof/datain/templ${REG}${NUM}${LABELI}
${SC2_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}/GFCTCTR${LABELI}${i}${ext}
${SC2_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}/GFCT${NUM}R${LABELI}${i}${ext}
EOT1

done

echo ${SC1_suite}/eof/datain/templ${REG}${NUM}${LABELI}

cp ${SC1_suite}/eof/datain/templ${REG}${NUM}${LABELI} ${SC1_suite}/eof/datain/templ${NUM}${LABELI}

# Pressao

cd ${SC1_suite}/eof/datain

ext=R.unf

cat <<EOT1 > eofpres${REG}${NUM}${LABELI}.nml
 &DATAIN
  DIRI='${SC1_suite}/eof/datain/ '
  DIRA='${SC1_suite}/model/datain/ '
  DIRO='${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ${NUM}${LABELI}'
  ANAME='GANL${PREFXI}${LABELI}${ext}.${RESOL}${NIVEL} '
  PRSOUT='prsout${REG}${NUM}${LABELI} '
  PRSSCM='prsscm${REG}${NUM}${LABELI} '
 &END
 &PRSSER
  PRSSER1(1)='prsse${REG}${NUM}1${LABELI} ',
  PRSSER1(2)='prsse${REG}${NUM}2${LABELI} ',
  PRSSER1(3)='prsse${REG}${NUM}3${LABELI} ',
  PRSSER1(4)='prsse${REG}${NUM}4${LABELI} ',
  PRSSER1(5)='prsse${REG}${NUM}5${LABELI} ',
  PRSSER1(6)='prsse${REG}${NUM}6${LABELI} ',
  PRSSER1(7)='prsse${REG}${NUM}7${LABELI} ',
  PRSSER1(8)='prsse${REG}${NUM}8${LABELI} ',
  PRSSER1(9)='prsse${REG}${NUM}9${LABELI} ',
  PRSSER1(10)='prsse${REG}${NUM}10${LABELI} ',
  PRSSER1(11)='prsse${REG}${NUM}11${LABELI} '
 &END
 &PRSSEN
  PRSSEN1(1)='prssn${REG}${NUM}1${LABELI} ',
  PRSSEN1(2)='prssn${REG}${NUM}2${LABELI} ',
  PRSSEN1(3)='prssn${REG}${NUM}3${LABELI} ',
  PRSSEN1(4)='prssn${REG}${NUM}4${LABELI} ',
  PRSSEN1(5)='prssn${REG}${NUM}5${LABELI} ',
  PRSSEN1(6)='prssn${REG}${NUM}6${LABELI} ',
  PRSSEN1(7)='prssn${REG}${NUM}7${LABELI} ',
  PRSSEN1(8)='prssn${REG}${NUM}8${LABELI} ',
  PRSSEN1(9)='prssn${REG}${NUM}9${LABELI} ',
  PRSSEN1(10)='prssn${REG}${NUM}10${LABELI} ',
  PRSSEN1(11)='prssn${REG}${NUM}11${LABELI} '
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

source=${HOME_suite}/eofpres/source

bin=$SC1_suite/eof/bin/$TRUNC$LEV;mkdir -p ${bin}
inp=$SC1_suite/eof/datain;mkdir -p ${inp}
outp=$SC1_suite/eofpres/output;mkdir -p ${outp}

cd $bin

cp -v eofpres.$TRUNC$LEV eofpres.$TRUNC$LEV.${NUM}${LABELI}

echo "./eofpres.$TRUNC$LEV.${NUM}${LABELI} < $inp/eofpres${REGE}${NUM}${LABELI}.nml > $outp/eofpres.$LABELI.$HOUR.$TRUNC$LEV"

./eofpres.$TRUNC$LEV.${NUM}${LABELI} < $inp/eofpres${REGE}${NUM}${LABELI}.nml > $outp/eofpres${REGE}.$LABELI.$HOUR.$TRUNC$LEV

# Temperatura

cd ${SC1_suite}/eof/datain

ext=R.unf

cat <<EOT1 > eoftem${REG}${NUM}${LABELI}.nml
 &DATAIN
  DIRI='${SC1_suite}/eof/datain/ '
  DIRA='${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRO='${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ${REG}${NUM}${LABELI} '
  ANAME='GANL${PREFXI}${LABELI}${ext}.${RESOL}${NIVEL} '
  TEMOUT='temout${REG}${NUM}${LABELI} '
  TEMPCM='tempcm${REG}${NUM}${LABELI} '
 &END
 &TEMPER
  TEMPER1(1)='tempe${REG}${NUM}1${LABELI} ',
  TEMPER1(2)='tempe${REG}${NUM}2${LABELI} ',
  TEMPER1(3)='tempe${REG}${NUM}3${LABELI} ',
  TEMPER1(4)='tempe${REG}${NUM}4${LABELI} ',
  TEMPER1(5)='tempe${REG}${NUM}5${LABELI} ',
  TEMPER1(6)='tempe${REG}${NUM}6${LABELI} ',
  TEMPER1(7)='tempe${REG}${NUM}7${LABELI} ',
  TEMPER1(8)='tempe${REG}${NUM}8${LABELI} ',
  TEMPER1(9)='tempe${REG}${NUM}9${LABELI} ',
  TEMPER1(10)='tempe${REG}${NUM}10${LABELI} ',
  TEMPER1(11)='tempe${REG}${NUM}11${LABELI} '
 &END
 &TEMPEN
  TEMPEN1(1)='tempn${REG}${NUM}1${LABELI} ',
  TEMPEN1(2)='tempn${REG}${NUM}2${LABELI} ',
  TEMPEN1(3)='tempn${REG}${NUM}3${LABELI} ',
  TEMPEN1(4)='tempn${REG}${NUM}4${LABELI} ',
  TEMPEN1(5)='tempn${REG}${NUM}5${LABELI} ',
  TEMPEN1(6)='tempn${REG}${NUM}6${LABELI} ',
  TEMPEN1(7)='tempn${REG}${NUM}7${LABELI} ',
  TEMPEN1(8)='tempn${REG}${NUM}8${LABELI} ',
  TEMPEN1(9)='tempn${REG}${NUM}9${LABELI} ',
  TEMPEN1(10)='tempn${REG}${NUM}10${LABELI} ',
  TEMPEN1(11)='tempn${REG}${NUM}11${LABELI} '
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

source=$HOME_suite/eoftemp/source

bin=$SC1_suite/eof/bin/$TRUNC$LEV;mkdir -p ${bin}
inp=$SC1_suite/eof/datain;mkdir -p ${inp}
outp=$SC1_suite/eoftemp/output;mkdir -p ${outp}

cd $bin

cp -v eoftem.$TRUNC$LEV eoftem.$TRUNC$LEV.${NUM}${LABELI}

echo "./eoftem.$TRUNC$LEV.${NUM}${LABELI} < $inp/eoftem${REGE}${NUM}${LABELI}.nml > $outp/eoftem.$LABELI.$HOUR.$TRUNC$LEV"
 
./eoftem.$TRUNC$LEV.${NUM}${LABELI} < $inp/eoftem${REGE}${NUM}${LABELI}.nml > $outp/eoftem${REGE}.$LABELI.$HOUR.$TRUNC$LEV

# Umidade

#if [ "${HUMID}" = "YES" ] 
#then

cd ${SC1_suite}/eof/datain

ext=R.unf

cat <<EOT1 > eofhum${REG}${NUM}${LABELI}.nml
 &DATAIN
  DIRI='${SC1_suite}/eof/datain/ '
  DIRA='${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRO='${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ${REG}${NUM}${LABELI} '
  ANAME='GANL${PREFXI}${LABELI}${ext}.${RESOL}${NIVEL} '
  HUMOUT='humout${REG}${NUM}${LABELI} '
  HUMPCM='humpcm${REG}${NUM}${LABELI} '
 &END
 &HUMPER
  HUMPER1(1)='humpe${REG}${NUM}1${LABELI} ',
  HUMPER1(2)='humpe${REG}${NUM}2${LABELI} ',
  HUMPER1(3)='humpe${REG}${NUM}3${LABELI} ',
  HUMPER1(4)='humpe${REG}${NUM}4${LABELI} ',
  HUMPER1(5)='humpe${REG}${NUM}5${LABELI} ',
  HUMPER1(6)='humpe${REG}${NUM}6${LABELI} ',
  HUMPER1(7)='humpe${REG}${NUM}7${LABELI} ',
  HUMPER1(8)='humpe${REG}${NUM}8${LABELI} ',
  HUMPER1(9)='humpe${REG}${NUM}9${LABELI} ',
  HUMPER1(10)='humpe${REG}${NUM}10${LABELI} ',
  HUMPER1(11)='humpe${REG}${NUM}11${LABELI} '
 &END
 &HUMPEN
  HUMPEN1(1)='humpn${REG}${NUM}1${LABELI} ',
  HUMPEN1(2)='humpn${REG}${NUM}2${LABELI} ',
  HUMPEN1(3)='humpn${REG}${NUM}3${LABELI} ',
  HUMPEN1(4)='humpn${REG}${NUM}4${LABELI} ',
  HUMPEN1(5)='humpn${REG}${NUM}5${LABELI} ',
  HUMPEN1(6)='humpn${REG}${NUM}6${LABELI} ',
  HUMPEN1(7)='humpn${REG}${NUM}7${LABELI} ',
  HUMPEN1(8)='humpn${REG}${NUM}8${LABELI} ',
  HUMPEN1(9)='humpn${REG}${NUM}9${LABELI} ',
  HUMPEN1(10)='humpn${REG}${NUM}10${LABELI} ',
  HUMPEN1(11)='humpn${REG}${NUM}11${LABELI} '
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

  echo 'Vai chamar eofh'

source=HOME_suite/eofhumi/source

bin=$SC1_suite/eof/bin/$TRUNC$LEV;mkdir -p ${bin}
input=$SC2_suite/eof/datain;mkdir -p ${input}
out=$HOME_suite/eof/dataout/$TRUNC$LEV;mkdir -p ${out}
inp=$SC1_suite/eof/datain;mkdir -p ${inp}
outp=$SC1_suite/eofhumi/output;mkdir -p ${outp}

cd $bin

cp -v eofhum.$TRUNC$LEV eofhum.$TRUNC$LEV.${NUM}${LABELI}

echo "./eofhum.$TRUNC$LEV.${NUM}${LABELI} < $inp/eofhum${REGE}${NUM}${LABELI}.nml > $outp/eofhum.$LABELI.$HOUR.$TRUNC$LEV"

./eofhum.$TRUNC$LEV.${NUM}${LABELI} < $inp/eofhum${REGE}${NUM}${LABELI}.nml > $outp/eofhum${REGE}.$LABELI.$HOUR.$TRUNC$LEV

#fi

# Vento

cd ${SC1_suite}/eof/datain

ext=R.unf

cat <<EOT1 > eofwin${REG}${NUM}${LABELI}.nml
 &DATAIN
  DIRI='${SC1_suite}/eof/datain/ '
  DIRA='${SC2_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRO='${SC2_suite}/eof/dataout/${RESOL}${NIVEL}// '
  NAMEL='templ${REG}${NUM}${LABELI} '
  ANAME='GANL${PREFXI}${LABELI}${ext}.${RESOL}${NIVEL} '
  WINOUT='winout${REG}${NUM}${LABELI} '
  WINPCM='winpcm${REG}${NUM}${LABELI} '
 &END
 &WINPER
  WINPER1(1)='winpe${REG}${NUM}1${LABELI} ',
  WINPER1(2)='winpe${REG}${NUM}2${LABELI} ',
  WINPER1(3)='winpe${REG}${NUM}3${LABELI} ',
  WINPER1(4)='winpe${REG}${NUM}4${LABELI} ',
  WINPER1(5)='winpe${REG}${NUM}5${LABELI} ',
  WINPER1(6)='winpe${REG}${NUM}6${LABELI} ',
  WINPER1(7)='winpe${REG}${NUM}7${LABELI} ',
  WINPER1(8)='winpe${REG}${NUM}8${LABELI} ',
  WINPER1(9)='winpe${REG}${NUM}9${LABELI} ',
  WINPER1(10)='winpe${REG}${NUM}10${LABELI} ',
  WINPER1(11)='winpe${REG}${NUM}11${LABELI} '
 &END
 &WINPEN
  WINPEN1(1)='winpn${REG}${NUM}1${LABELI} ',
  WINPEN1(2)='winpn${REG}${NUM}2${LABELI} ',
  WINPEN1(3)='winpn${REG}${NUM}3${LABELI} ',
  WINPEN1(4)='winpn${REG}${NUM}4${LABELI} ',
  WINPEN1(5)='winpn${REG}${NUM}5${LABELI} ',
  WINPEN1(6)='winpn${REG}${NUM}6${LABELI} ',
  WINPEN1(7)='winpn${REG}${NUM}7${LABELI} ',
  WINPEN1(8)='winpn${REG}${NUM}8${LABELI} ',
  WINPEN1(9)='winpn${REG}${NUM}9${LABELI} ',
  WINPEN1(10)='winpn${REG}${NUM}10${LABELI} ',
  WINPEN1(11)='winpn${REG}${NUM}11${LABELI} '
 &END
 &PRSSER
  PRSSER1(1)='prsse${REG}${NUM}1${LABELI} ',
  PRSSER1(2)='prsse${REG}${NUM}2${LABELI} ',
  PRSSER1(3)='prsse${REG}${NUM}3${LABELI} ',
  PRSSER1(4)='prsse${REG}${NUM}4${LABELI} ',
  PRSSER1(5)='prsse${REG}${NUM}5${LABELI} ',
  PRSSER1(6)='prsse${REG}${NUM}6${LABELI} ',
  PRSSER1(7)='prsse${REG}${NUM}7${LABELI} ',
  PRSSER1(8)='prsse${REG}${NUM}8${LABELI} ',
  PRSSER1(9)='prsse${REG}${NUM}9${LABELI} ',
  PRSSER1(10)='prsse${REG}${NUM}10${LABELI} ',
  PRSSER1(11)='prsse${REG}${NUM}11${LABELI} '
 &END
 &PRSSEN
  PRSSEN1(1)='prssn${REG}${NUM}1${LABELI} ',
  PRSSEN1(2)='prssn${REG}${NUM}2${LABELI} ',
  PRSSEN1(3)='prssn${REG}${NUM}3${LABELI} ',
  PRSSEN1(4)='prssn${REG}${NUM}4${LABELI} ',
  PRSSEN1(5)='prssn${REG}${NUM}5${LABELI} ',
  PRSSEN1(6)='prssn${REG}${NUM}6${LABELI} ',
  PRSSEN1(7)='prssn${REG}${NUM}7${LABELI} ',
  PRSSEN1(8)='prssn${REG}${NUM}8${LABELI} ',
  PRSSEN1(9)='prssn${REG}${NUM}9${LABELI} ',
  PRSSEN1(10)='prssn${REG}${NUM}10${LABELI} ',
  PRSSEN1(11)='prssn${REG}${NUM}11${LABELI} '
 &END
 &TEMPER
  TEMPER1(1)='tempe${REG}${NUM}1${LABELI} ',
  TEMPER1(2)='tempe${REG}${NUM}2${LABELI} ',
  TEMPER1(3)='tempe${REG}${NUM}3${LABELI} ',
  TEMPER1(4)='tempe${REG}${NUM}4${LABELI} ',
  TEMPER1(5)='tempe${REG}${NUM}5${LABELI} ',
  TEMPER1(6)='tempe${REG}${NUM}6${LABELI} ',
  TEMPER1(7)='tempe${REG}${NUM}7${LABELI} ',
  TEMPER1(8)='tempe${REG}${NUM}8${LABELI} ',
  TEMPER1(9)='tempe${REG}${NUM}9${LABELI} ',
  TEMPER1(10)='tempe${REG}${NUM}10${LABELI} ',
  TEMPER1(11)='tempe${REG}${NUM}11${LABELI} '
 &END
 &TEMPEN
  TEMPEN1(1)='tempn${REG}${NUM}1${LABELI} ',
  TEMPEN1(2)='tempn${REG}${NUM}2${LABELI} ',
  TEMPEN1(3)='tempn${REG}${NUM}3${LABELI} ',
  TEMPEN1(4)='tempn${REG}${NUM}4${LABELI} ',
  TEMPEN1(5)='tempn${REG}${NUM}5${LABELI} ',
  TEMPEN1(6)='tempn${REG}${NUM}6${LABELI} ',
  TEMPEN1(7)='tempn${REG}${NUM}7${LABELI} ',
  TEMPEN1(8)='tempn${REG}${NUM}8${LABELI} ',
  TEMPEN1(9)='tempn${REG}${NUM}9${LABELI} ',
  TEMPEN1(10)='tempn${REG}${NUM}10${LABELI} ',
  TEMPEN1(11)='tempn${REG}${NUM}11${LABELI} '
 &END
 &HUMPER
  HUMPER1(1)='humpe${REG}${NUM}1${LABELI} ',
  HUMPER1(2)='humpe${REG}${NUM}2${LABELI} ',
  HUMPER1(3)='humpe${REG}${NUM}3${LABELI} ',
  HUMPER1(4)='humpe${REG}${NUM}4${LABELI} ',
  HUMPER1(5)='humpe${REG}${NUM}5${LABELI} ',
  HUMPER1(6)='humpe${REG}${NUM}6${LABELI} ',
  HUMPER1(7)='humpe${REG}${NUM}7${LABELI} ',
  HUMPER1(8)='humpe${REG}${NUM}8${LABELI} ',
  HUMPER1(9)='humpe${REG}${NUM}9${LABELI} ',
  HUMPER1(10)='humpe${REG}${NUM}10${LABELI} ',
  HUMPER1(11)='humpe${REG}${NUM}11${LABELI} '
 &END
 &HUMPEN
  HUMPEN1(1)='humpn${REG}${NUM}1${LABELI} ',
  HUMPEN1(2)='humpn${REG}${NUM}2${LABELI} ',
  HUMPEN1(3)='humpn${REG}${NUM}3${LABELI} ',
  HUMPEN1(4)='humpn${REG}${NUM}4${LABELI} ',
  HUMPEN1(5)='humpn${REG}${NUM}5${LABELI} ',
  HUMPEN1(6)='humpn${REG}${NUM}6${LABELI} ',
  HUMPEN1(7)='humpn${REG}${NUM}7${LABELI} ',
  HUMPEN1(8)='humpn${REG}${NUM}8${LABELI} ',
  HUMPEN1(9)='humpn${REG}${NUM}9${LABELI} ',
  HUMPEN1(10)='humpn${REG}${NUM}10${LABELI} ',
  HUMPEN1(11)='humpn${REG}${NUM}11${LABELI} '
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

echo 'Vai chamar eofw'

source=${HOME_suite}/eofwind/source

bin=${SC1_suite}/eof/bin/$TRUNC$LEV;mkdir -p ${bin}
out=${SC2_suite}/eof/dataout/${RESOL}${NIVEL};mkdir -p ${out}
inp=${SC1_suite}/eof/datain;mkdir -p ${inp}
outp=${SC1_suite}/eofwind/output;mkdir -p ${outp}

cd ${bin}

cp -v eofwin.${TRUNC}${LEV} eofwin.${TRUNC}${LEV}.${NUM}${LABELI}

echo "./eofwin.${TRUNC}${LEV}.${NUM}${LABELI} < ${inp}/eofwin${REGE}${NUM}${LABELI}.nml > $outp/eofwin.${LABELI}.${HOUR}.${TRUNC}${LEV}"

./eofwin.${TRUNC}${LEV}.${NUM}${LABELI} < ${inp}/eofwin${REGE}${NUM}${LABELI}.nml > ${outp}/eofwin${REGE}.${LABELI}.${HOUR}.${TRUNC}${LEV}
