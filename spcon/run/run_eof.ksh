# !/bin/ksh
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para calcular as EOFs a partir da série de diferenças entre
# as previsões controle e as previsões geradas a partir do conjunto de
# análises perturbadas randomicamente para o Sistema de 
# Previsão por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_eof.ksh <opcao1> <opcao2> <opcao3> <opcao4>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao -> resolução espectral do modelo
#                                
#            <opcao2> membro    -> tamanho do conjunto 
#
#            <opcao3> moist_opt -> opção lógica (YES/NO) para
#                                  perturbar ou não a umidade
#            <opcao4> data      -> data da análise corrente 
#            
#  Uso/Exemplos: ./run_eof.ksh TQ0126L028 7 YES 2012123118
#                (calcular as perturbações por EOF a partir de um
#                conjunto de 7 análises peturbadas randomicamente 
#                válidas às 2012123118 na resolução TQ0126L028)
# 
# !REVISION HISTORY:
#
# XX Julho de 2017 - C. F. Bastarz - Versão inicial.  
# 16 Agosto de 2017 - C. F. Bastarz - Inclusão comentários.
#
# !REMARKS:
#
# !BUGS:
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
#set -o xtrace

# Menu de opções/ajuda
if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

# Verificação dos argumentos de entrada
if [ -z "${1}" ]
then
  echo "TRCLV is not set"
  exit
else
  TRCLV=${1}
fi
if [ -z "${2}" ]
then
  echo "PERT: NMC, AVN CTR 01N"
  exit
else
  NMEM=${2}
fi
if [ -z "${3}" ]
then
  echo "Argument is not set (H)"
  exit
else
  HUMID=${3}
fi
if [ -z "${4}" ]
then
  echo "Fifth argument is not set (LABELI: yyyymmddhh)"
  exit
else
  LABELI=${4}
fi

# Diretórios principais
export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ../; pwd)

. ${FILEENV} ${TRCLV} ${NMEM}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

LABELF=$(date -d "${LABELI:0:8} ${LABELI:8:2}:00 ${NFDAYS} days" +"%Y%m%d%H")
export LABELI LABELF NFDAYS

cd ${HOME_suite}/run

# Variáveis utilizadas no script de submissão
export PBS_SERVER=aux20-eth4
mkdir -p ${DK_suite}/model/exec/setout

export MAQUI=$(hostname -s)

# Script de submissão
SCRIPTSFILE=set${NMEM}perpntg.${TRCLV}.${LABELI}.${MAQUI}

MONITORID=${RANDOM}

RUNTM=$(date +"%s")

cat <<EOT0 > ${HOME_suite}/run/${SCRIPTSFILE}
#!/bin/bash -x
#PBS -o ${DK_suite}/eof/output/${SCRIPTSFILE}.${RUNTM}.out
#PBS -e ${DK_suite}/eof/output/${SCRIPTSFILE}.${RUNTM}.err
#PBS -l walltime=0:10:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N EOFPERT
#PBS -q ${AUX_QUEUE}
#PBS -J 1-${NMEM}

export MEM=\$(printf %02g \${PBS_ARRAY_INDEX})

#
#  Change directory to run
#

cd ${HOME_suite}/run

#
#  Run scripts of recomposition         
#

Regs=(hn hs san sas tr)
#Regs=(hn)

for REG in \${Regs[@]}
do

#
# Set region limits
#

if [ \${REG} == hn ]
then
  export MR=126
  export IR=384
  export JR=192
  export II=1
  export IS=384
  export NI=384
  export JI=1
  export JS=75
  export NJ=75
  export IIPS=192
  export ISPS=299
  export JIPS=40
  export JSPS=75
  export KR=28
  export LR=11
elif [ \${REG} == hs ] 
then
  export MR=126
  export IR=384
  export JR=192
  export II=1
  export IS=384
  export NI=384
  export JI=118
  export JS=192 
  export NJ=75
  export IIPS=192
  export ISPS=299
  export JIPS=118
  export JSPS=153
  export KR=28
  export LR=11
elif [ \${REG} == san ] 
then
  export MR=126
  export IR=384
  export JR=192
  export II=276
  export IS=372
  export NI=97
  export JI=75
  export JS=108
  export NJ=34
  export IIPS=276
  export ISPS=316
  export JIPS=75
  export JSPS=108
  export KR=28
  export LR=11
elif [ \${REG} == sas ] 
then
  export MR=126
  export IR=384
  export JR=192
  export II=266
  export IS=362
  export NI=97
  export JI=109
  export JS=161
  export NJ=53
  export IIPS=266
  export ISPS=316
  export JIPS=109
  export JSPS=160 
  export KR=28
  export LR=11
elif [ \${REG} == tr ] 
then
  export MR=126
  export IR=384
  export JR=192
  export II=1
  export IS=384
  export NI=384
  export JI=76
  export JS=117 
  export NJ=42
  export IIPS=192
  export ISPS=299
  export JIPS=76
  export JSPS=117
  export KR=28
  export LR=11
else
  echo "Region undefined."
  exit 1
fi

export TRUNC=${RESOL}
export LEV=${NIVEL}

#
#  Set date (year,month,day) and hour (hour:minute) 
#
#  DATE=yyyymmdd
#  HOUR=hh:mn
#

export DATE=\$(date +'%Y')\$(date +'%m')\$(date +'%d')
export HOUR=\$(date +'%H:%M')
echo "Date: "\$DATE
echo "Hour: "\$HOUR

#
#  Now, build the necessary NAMELIST input:
#

export ext=R.fct.${RESOL}${NIVEL}

cd ${DK_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}/

for LABELF in \$( ls -1 GFCTCTR${LABELI}*fct* | cut -c18-27)
do
cat <<EOT1 >> ${DK_suite}/eof/datain/templ\${REG}\${MEM}${LABELI}
${DK_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}/GFCTCTR${LABELI}\${LABELF}\${ext}
${DK_suite}/recfct/dataout/${RESOL}${NIVEL}/${LABELI}/GFCT\${MEM}R${LABELI}\${LABELF}\${ext}
EOT1
done

echo ${DK_suite}/eof/datain/templ\${REG}\${MEM}${LABELI}

cp ${DK_suite}/eof/datain/templ\${REG}\${MEM}${LABELI} ${DK_suite}/eof/datain/templ\${MEM}${LABELI}

cd \${DK_suite}/eof/datain

export ext=R.unf

cat <<EOT1 > eofpres\${REG}\${MEM}.nml
 &DATAIN
  DIRI='\${DK_suite}/eof/datain/ '
!  DIRA='\${DK_suite}/model/datain/ '
  DIRA='\${DK_suite}/rdpert/dataout/${RESOL}${NIVEL}/ '
  DIRO='\${DK_suite}/eof/dataout/${RESOL}${NIVEL}/ '
  NAMEL='templ\${MEM}${LABELI} '
  ANAME='GANL\${MEM}R${LABELI}\${ext}.${RESOL}${NIVEL} '
  PRSOUT='prsout\${REG}\${MEM}${LABELI} '
  PRSSCM='prsscm\${REG}\${MEM}${LABELI} '
 &END
 &PRSSER
  PRSSER1(1)='prsse\${REG}\${MEM}1${LABELI} ',
  PRSSER1(2)='prsse\${REG}\${MEM}2${LABELI} ',
  PRSSER1(3)='prsse\${REG}\${MEM}3${LABELI} ',
  PRSSER1(4)='prsse\${REG}\${MEM}4${LABELI} ',
  PRSSER1(5)='prsse\${REG}\${MEM}5${LABELI} ',
  PRSSER1(6)='prsse\${REG}\${MEM}6${LABELI} ',
  PRSSER1(7)='prsse\${REG}\${MEM}7${LABELI} ',
  PRSSER1(8)='prsse\${REG}\${MEM}8${LABELI} ',
  PRSSER1(9)='prsse\${REG}\${MEM}9${LABELI} ',
  PRSSER1(10)='prsse\${REG}\${MEM}10${LABELI} ',
  PRSSER1(11)='prsse\${REG}\${MEM}11${LABELI} '
 &END
 &PRSSEN
  PRSSEN1(1)='prssn\${REG}\${MEM}1${LABELI} ',
  PRSSEN1(2)='prssn\${REG}\${MEM}2${LABELI} ',
  PRSSEN1(3)='prssn\${REG}\${MEM}3${LABELI} ',
  PRSSEN1(4)='prssn\${REG}\${MEM}4${LABELI} ',
  PRSSEN1(5)='prssn\${REG}\${MEM}5${LABELI} ',
  PRSSEN1(6)='prssn\${REG}\${MEM}6${LABELI} ',
  PRSSEN1(7)='prssn\${REG}\${MEM}7${LABELI} ',
  PRSSEN1(8)='prssn\${REG}\${MEM}8${LABELI} ',
  PRSSEN1(9)='prssn\${REG}\${MEM}9${LABELI} ',
  PRSSEN1(10)='prssn\${REG}\${MEM}10${LABELI} ',
  PRSSEN1(11)='prssn\${REG}\${MEM}11${LABELI} '
 &END
 &STPRES
  STDP=1.00
 &END
 &PARMET
  IINF=\${II},ISUP=\${IS},IMAX0=\${NI},
  JINF=\${JI},JSUP=\${JS},JMAX0=\${NJ},
  IIPS=\${IIPS},ISPS=\${ISPS},JIPS=\${JIPS},JSPS=\${JSPS},
 &END
EOT1

cd \${DK_suite}/eof/bin/\${TRUNC}\${LEV}/

./eofpres.\${TRUNC}\${LEV} < ${DK_suite}/eof/datain/eofpres\${REG}\${MEM}.nml > ${DK_suite}/eof/dataout/eofpres-\${MEM}.\${REG}.${LABELI}.\${HOUR}.\${TRUNC}\${LEV}
touch ${DK_suite}/eof/bin/\${RESOL}\${NIVEL}/monitor-pres.\${REG}.${MONITORID}

cd \${DK_suite}/eof/datain

export ext=R.unf

cat <<EOT1 > eoftem\${REG}\${MEM}.nml
 &DATAIN
  DIRI='\${DK_suite}/eof/datain/ '
!  DIRA='\${DK_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRA='\${DK_suite}/rdpert/dataout/${RESOL}${NIVEL}/ '
  DIRO='\${DK_suite}/eof/dataout/${RESOL}${NIVEL}/ '
  NAMEL='templ\${REG}\${MEM}\${LABELI} '
  ANAME='GANL\${MEM}R\${LABELI}\${ext}.${RESOL}${NIVEL} '
  TEMOUT='temout\${REG}\${MEM}\${LABELI} '
  TEMPCM='tempcm\${REG}\${MEM}\${LABELI} '
 &END
 &TEMPER
  TEMPER1(1)='tempe\${REG}\${MEM}1\${LABELI} ',
  TEMPER1(2)='tempe\${REG}\${MEM}2\${LABELI} ',
  TEMPER1(3)='tempe\${REG}\${MEM}3\${LABELI} ',
  TEMPER1(4)='tempe\${REG}\${MEM}4\${LABELI} ',
  TEMPER1(5)='tempe\${REG}\${MEM}5\${LABELI} ',
  TEMPER1(6)='tempe\${REG}\${MEM}6\${LABELI} ',
  TEMPER1(7)='tempe\${REG}\${MEM}7\${LABELI} ',
  TEMPER1(8)='tempe\${REG}\${MEM}8\${LABELI} ',
  TEMPER1(9)='tempe\${REG}\${MEM}9\${LABELI} ',
  TEMPER1(10)='tempe\${REG}\${MEM}10\${LABELI} ',
  TEMPER1(11)='tempe\${REG}\${MEM}11\${LABELI} '
 &END
 &TEMPEN
  TEMPEN1(1)='tempn\${REG}\${MEM}1\${LABELI} ',
  TEMPEN1(2)='tempn\${REG}\${MEM}2\${LABELI} ',
  TEMPEN1(3)='tempn\${REG}\${MEM}3\${LABELI} ',
  TEMPEN1(4)='tempn\${REG}\${MEM}4\${LABELI} ',
  TEMPEN1(5)='tempn\${REG}\${MEM}5\${LABELI} ',
  TEMPEN1(6)='tempn\${REG}\${MEM}6\${LABELI} ',
  TEMPEN1(7)='tempn\${REG}\${MEM}7\${LABELI} ',
  TEMPEN1(8)='tempn\${REG}\${MEM}8\${LABELI} ',
  TEMPEN1(9)='tempn\${REG}\${MEM}9\${LABELI} ',
  TEMPEN1(10)='tempn\${REG}\${MEM}10\${LABELI} ',
  TEMPEN1(11)='tempn\${REG}\${MEM}11\${LABELI} '
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
  IINF=\${II},ISUP=\${IS},IMAX0=\${NI},
  JINF=\${JI},JSUP=\${JS},JMAX0=\${NJ},
  IIPS=\${IIPS},ISPS=\${ISPS},JIPS=\${JIPS},JSPS=\${JSPS}
 &END
EOT1

cd \${DK_suite}/eof/bin/\${TRUNC}\${LEV}/

./eoftem.\${TRUNC}\${LEV} < ${DK_suite}/eof/datain/eoftem\${REG}\${MEM}.nml > ${DK_suite}/eof/dataout/eoftem-\${MEM}.\${REG}.${LABELI}.\${HOUR}.\${TRUNC}\${LEV}
touch ${DK_suite}/eof/bin/\${RESOL}\${NIVEL}/monitor-temp.\${REG}.${MONITORID}

if [ ${HUMID} = YES ] 
then

  #
  #  Now, build the necessary NAMELIST input:
  #

  cd \${DK_suite}/eof/datain

  export ext=R.unf

cat <<EOT1 > eofhum\${REG}\${MEM}.nml
 &DATAIN
  DIRI='\${DK_suite}/eof/datain/ '
!  DIRA='\${DK_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRA='\${DK_suite}/rdpert/dataout/${RESOL}${NIVEL}/ '
  DIRO='\${DK_suite}/eof/dataout/${RESOL}${NIVEL}/ '
  NAMEL='templ\${REG}\${MEM}\${LABELI} '
  ANAME='GANL\${MEM}R\${LABELI}\${ext}.${RESOL}${NIVEL} '
  HUMOUT='humout\${REG}\${MEM}\${LABELI} '
  HUMPCM='humpcm\${REG}\${MEM}\${LABELI} '
 &END
 &HUMPER
  HUMPER1(1)='humpe\${REG}\${MEM}1\${LABELI} ',
  HUMPER1(2)='humpe\${REG}\${MEM}2\${LABELI} ',
  HUMPER1(3)='humpe\${REG}\${MEM}3\${LABELI} ',
  HUMPER1(4)='humpe\${REG}\${MEM}4\${LABELI} ',
  HUMPER1(5)='humpe\${REG}\${MEM}5\${LABELI} ',
  HUMPER1(6)='humpe\${REG}\${MEM}6\${LABELI} ',
  HUMPER1(7)='humpe\${REG}\${MEM}7\${LABELI} ',
  HUMPER1(8)='humpe\${REG}\${MEM}8\${LABELI} ',
  HUMPER1(9)='humpe\${REG}\${MEM}9\${LABELI} ',
  HUMPER1(10)='humpe\${REG}\${MEM}10\${LABELI} ',
  HUMPER1(11)='humpe\${REG}\${MEM}11\${LABELI} '
 &END
 &HUMPEN
  HUMPEN1(1)='humpn\${REG}\${MEM}1\${LABELI} ',
  HUMPEN1(2)='humpn\${REG}\${MEM}2\${LABELI} ',
  HUMPEN1(3)='humpn\${REG}\${MEM}3\${LABELI} ',
  HUMPEN1(4)='humpn\${REG}\${MEM}4\${LABELI} ',
  HUMPEN1(5)='humpn\${REG}\${MEM}5\${LABELI} ',
  HUMPEN1(6)='humpn\${REG}\${MEM}6\${LABELI} ',
  HUMPEN1(7)='humpn\${REG}\${MEM}7\${LABELI} ',
  HUMPEN1(8)='humpn\${REG}\${MEM}8\${LABELI} ',
  HUMPEN1(9)='humpn\${REG}\${MEM}9\${LABELI} ',
  HUMPEN1(10)='humpn\${REG}\${MEM}10\${LABELI} ',
  HUMPEN1(11)='humpn\${REG}\${MEM}11\${LABELI} '
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
  IINF=\${II},ISUP=\${IS},IMAX0=\${NI},
  JINF=\${JI},JSUP=\${JS},JMAX0=\${NJ},
  IIPS=\${IIPS},ISPS=\${ISPS},JIPS=\${JIPS},JSPS=\${JSPS}
 &END
EOT1

  cd \${HOME_suite}/eof/bin/\${TRUNC}\${LEV}/

  ./eofhum.\${TRUNC}\${LEV} < ${DK_suite}/eof/datain/eofhum\${REG}\${MEM}.nml > ${DK_suite}/eof/dataout/eofhum-\${MEM}.\${REG}.${LABELI}.\${HOUR}.\${TRUNC}\${LEV}
  touch ${DK_suite}/eof/bin/\${RESOL}\${NIVEL}/monitor-hum.\${REG}.${MONITORID}

fi

cd \${DK_suite}/eof/datain

export ext=R.unf

cat <<EOT1 > eofwin\${REG}\${MEM}.nml
 &DATAIN
  DIRI='\${DK_suite}/eof/datain/ '
!  DIRA='\${DK_suite}/recanl/dataout/${RESOL}${NIVEL}/ '
  DIRA='\${DK_suite}/rdpert/dataout/${RESOL}${NIVEL}/ '
  DIRO='\${DK_suite}/eof/dataout/${RESOL}${NIVEL}/ '
  NAMEL='templ\${REG}\${MEM}\${LABELI} '
  ANAME='GANL\${MEM}R\${LABELI}\${ext}.${RESOL}${NIVEL} '
  WINOUT='winout\${REG}\${MEM}\${LABELI} '
  WINPCM='winpcm\${REG}\${MEM}\${LABELI} '
 &END
 &WINPER
  WINPER1(1)='winpe\${REG}\${MEM}1\${LABELI} ',
  WINPER1(2)='winpe\${REG}\${MEM}2\${LABELI} ',
  WINPER1(3)='winpe\${REG}\${MEM}3\${LABELI} ',
  WINPER1(4)='winpe\${REG}\${MEM}4\${LABELI} ',
  WINPER1(5)='winpe\${REG}\${MEM}5\${LABELI} ',
  WINPER1(6)='winpe\${REG}\${MEM}6\${LABELI} ',
  WINPER1(7)='winpe\${REG}\${MEM}7\${LABELI} ',
  WINPER1(8)='winpe\${REG}\${MEM}8\${LABELI} ',
  WINPER1(9)='winpe\${REG}\${MEM}9\${LABELI} ',
  WINPER1(10)='winpe\${REG}\${MEM}10\${LABELI} ',
  WINPER1(11)='winpe\${REG}\${MEM}11\${LABELI} '
 &END
 &WINPEN
  WINPEN1(1)='winpn\${REG}\${MEM}1\${LABELI} ',
  WINPEN1(2)='winpn\${REG}\${MEM}2\${LABELI} ',
  WINPEN1(3)='winpn\${REG}\${MEM}3\${LABELI} ',
  WINPEN1(4)='winpn\${REG}\${MEM}4\${LABELI} ',
  WINPEN1(5)='winpn\${REG}\${MEM}5\${LABELI} ',
  WINPEN1(6)='winpn\${REG}\${MEM}6\${LABELI} ',
  WINPEN1(7)='winpn\${REG}\${MEM}7\${LABELI} ',
  WINPEN1(8)='winpn\${REG}\${MEM}8\${LABELI} ',
  WINPEN1(9)='winpn\${REG}\${MEM}9\${LABELI} ',
  WINPEN1(10)='winpn\${REG}\${MEM}10\${LABELI} ',
  WINPEN1(11)='winpn\${REG}\${MEM}11\${LABELI} '
 &END
 &PRSSER
  PRSSER1(1)='prsse\${REG}\${MEM}1\${LABELI} ',
  PRSSER1(2)='prsse\${REG}\${MEM}2\${LABELI} ',
  PRSSER1(3)='prsse\${REG}\${MEM}3\${LABELI} ',
  PRSSER1(4)='prsse\${REG}\${MEM}4\${LABELI} ',
  PRSSER1(5)='prsse\${REG}\${MEM}5\${LABELI} ',
  PRSSER1(6)='prsse\${REG}\${MEM}6\${LABELI} ',
  PRSSER1(7)='prsse\${REG}\${MEM}7\${LABELI} ',
  PRSSER1(8)='prsse\${REG}\${MEM}8\${LABELI} ',
  PRSSER1(9)='prsse\${REG}\${MEM}9\${LABELI} ',
  PRSSER1(10)='prsse\${REG}\${MEM}10\${LABELI} ',
  PRSSER1(11)='prsse\${REG}\${MEM}11\${LABELI} '
 &END
 &PRSSEN
  PRSSEN1(1)='prssn\${REG}\${MEM}1\${LABELI} ',
  PRSSEN1(2)='prssn\${REG}\${MEM}2\${LABELI} ',
  PRSSEN1(3)='prssn\${REG}\${MEM}3\${LABELI} ',
  PRSSEN1(4)='prssn\${REG}\${MEM}4\${LABELI} ',
  PRSSEN1(5)='prssn\${REG}\${MEM}5\${LABELI} ',
  PRSSEN1(6)='prssn\${REG}\${MEM}6\${LABELI} ',
  PRSSEN1(7)='prssn\${REG}\${MEM}7\${LABELI} ',
  PRSSEN1(8)='prssn\${REG}\${MEM}8\${LABELI} ',
  PRSSEN1(9)='prssn\${REG}\${MEM}9\${LABELI} ',
  PRSSEN1(10)='prssn\${REG}\${MEM}10\${LABELI} ',
  PRSSEN1(11)='prssn\${REG}\${MEM}11\${LABELI} '
 &END
 &TEMPER
  TEMPER1(1)='tempe\${REG}\${MEM}1\${LABELI} ',
  TEMPER1(2)='tempe\${REG}\${MEM}2\${LABELI} ',
  TEMPER1(3)='tempe\${REG}\${MEM}3\${LABELI} ',
  TEMPER1(4)='tempe\${REG}\${MEM}4\${LABELI} ',
  TEMPER1(5)='tempe\${REG}\${MEM}5\${LABELI} ',
  TEMPER1(6)='tempe\${REG}\${MEM}6\${LABELI} ',
  TEMPER1(7)='tempe\${REG}\${MEM}7\${LABELI} ',
  TEMPER1(8)='tempe\${REG}\${MEM}8\${LABELI} ',
  TEMPER1(9)='tempe\${REG}\${MEM}9\${LABELI} ',
  TEMPER1(10)='tempe\${REG}\${MEM}10\${LABELI} ',
  TEMPER1(11)='tempe\${REG}\${MEM}11\${LABELI} '
 &END
 &TEMPEN
  TEMPEN1(1)='tempn\${REG}\${MEM}1\${LABELI} ',
  TEMPEN1(2)='tempn\${REG}\${MEM}2\${LABELI} ',
  TEMPEN1(3)='tempn\${REG}\${MEM}3\${LABELI} ',
  TEMPEN1(4)='tempn\${REG}\${MEM}4\${LABELI} ',
  TEMPEN1(5)='tempn\${REG}\${MEM}5\${LABELI} ',
  TEMPEN1(6)='tempn\${REG}\${MEM}6\${LABELI} ',
  TEMPEN1(7)='tempn\${REG}\${MEM}7\${LABELI} ',
  TEMPEN1(8)='tempn\${REG}\${MEM}8\${LABELI} ',
  TEMPEN1(9)='tempn\${REG}\${MEM}9\${LABELI} ',
  TEMPEN1(10)='tempn\${REG}\${MEM}10\${LABELI} ',
  TEMPEN1(11)='tempn\${REG}\${MEM}11\${LABELI} '
 &END
 &HUMPER
  HUMPER1(1)='humpe\${REG}\${MEM}1\${LABELI} ',
  HUMPER1(2)='humpe\${REG}\${MEM}2\${LABELI} ',
  HUMPER1(3)='humpe\${REG}\${MEM}3\${LABELI} ',
  HUMPER1(4)='humpe\${REG}\${MEM}4\${LABELI} ',
  HUMPER1(5)='humpe\${REG}\${MEM}5\${LABELI} ',
  HUMPER1(6)='humpe\${REG}\${MEM}6\${LABELI} ',
  HUMPER1(7)='humpe\${REG}\${MEM}7\${LABELI} ',
  HUMPER1(8)='humpe\${REG}\${MEM}8\${LABELI} ',
  HUMPER1(9)='humpe\${REG}\${MEM}9\${LABELI} ',
  HUMPER1(10)='humpe\${REG}\${MEM}10\${LABELI} ',
  HUMPER1(11)='humpe\${REG}\${MEM}11\${LABELI} '
 &END
 &HUMPEN
  HUMPEN1(1)='humpn\${REG}\${MEM}1\${LABELI} ',
  HUMPEN1(2)='humpn\${REG}\${MEM}2\${LABELI} ',
  HUMPEN1(3)='humpn\${REG}\${MEM}3\${LABELI} ',
  HUMPEN1(4)='humpn\${REG}\${MEM}4\${LABELI} ',
  HUMPEN1(5)='humpn\${REG}\${MEM}5\${LABELI} ',
  HUMPEN1(6)='humpn\${REG}\${MEM}6\${LABELI} ',
  HUMPEN1(7)='humpn\${REG}\${MEM}7\${LABELI} ',
  HUMPEN1(8)='humpn\${REG}\${MEM}8\${LABELI} ',
  HUMPEN1(9)='humpn\${REG}\${MEM}9\${LABELI} ',
  HUMPEN1(10)='humpn\${REG}\${MEM}10\${LABELI} ',
  HUMPEN1(11)='humpn\${REG}\${MEM}11\${LABELI} '
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
  IINF=\${II},ISUP=\${IS},IMAX0=\${NI},
  JINF=\${JI},JSUP=\${JS},JMAX0=\${NJ},
  IIPS=\${IIPS},ISPS=\${ISPS},JIPS=\${JIPS},JSPS=\${JSPS}
 &END
EOT1

cd \${HOME_suite}/eof/bin/\${TRUNC}\${LEV}/

./eofwin.\${TRUNC}\${LEV} < ${DK_suite}/eof/datain/eofwin\${REG}\${MEM}.nml > \${DK_suite}/eof/dataout/eofwin-\${MEM}.\${REG}.${LABELI}.\${HOUR}.\${TRUNC}\${LEV}
touch ${DK_suite}/eof/bin/\${RESOL}\${NIVEL}/monitor-win.\${REG}.${MONITORID}

done

touch ${DK_suite}/eof/bin/\${RESOL}\${NIVEL}/monitor.${MONITORID}
EOT0

# Submete o script e aguarda o fim da execução
chmod +x ${HOME_suite}/run/${SCRIPTSFILE}

qsub ${HOME_suite}/run/${SCRIPTSFILE}

until [ -e "${DK_suite}/eof/bin/${RESOL}${NIVEL}/monitor.${MONITORID}" ]; do sleep 1s; done
rm ${DK_suite}/eof/bin/${RESOL}${NIVEL}/monitor*.${MONITORID}

exit 0
