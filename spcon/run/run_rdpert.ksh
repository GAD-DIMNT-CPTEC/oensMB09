#! /bin/ksh
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a gerar um conjunto inicial de n análises randomicamente
# perturbadas a partir da análise controle do Sistema de Previsão por 
# Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_rdpert.ksh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao -> resolução espectral do modelo
#                                
#            <opcao2> prefixo   -> prefixo que identifica o tipo de
#                                  análise
#
#            <opcao3> moist_opt -> opção lógica (YES/NO) para
#                                  perturbar ou não a umidade
#
#            <opcao4> data      -> data da análise corrente 
#
#
#            <opcao5> membro    -> tamanho do conjunto 
#            
#  Uso/Exemplos: ./run_rdpert.ksh TQ0126L028 NMC YES 2012111200 7
#                (perturba randomicamente um conjunto inicial de 7
#                membros a partir de uma análise controle na resolução
#                TQ0126L028; inclui a perturbação da umidade)
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

# Diretórios principais
export FILEENV=$(find ./ -name EnvironmentalVariablesMCGA -print)
export PATHENV=$(dirname ${FILEENV})
export PATHBASE=$(cd ${PATHENV}; cd ../; pwd)

. ${FILEENV} ${1} ${2}

cd ${HOME_suite}/run

TRC=$(echo ${TRCLV} | cut -c 1-6 | tr -d "TQ0")
LV=$(echo ${TRCLV} | cut -c 7-11 | tr -d "L0")

export RESOL=${TRCLV:0:6}
export NIVEL=${TRCLV:6:4}

# Verificação dos argumentos de entrada
if [ -z "${2}" ]
then
  echo "PERT: NMC, AVN CTR 01N" 
  exit
else
  PREFIC=${2}
fi

if [ -z "${3}" ]
then
  echo "Third argument is not set (HUMID)"
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
if [ -z "${5}" ]; then
  echo "Fifth argument is not set (NPERT)"
  exit
else
  NPERT=${5}
fi

# Variáveis utilizadas no script de submissão
HSTMAQ=$(hostname)

RUNTM=$(date +'%Y')$(date +'%m')$(date +'%d')$(date +'%H:%M')
EXT=out
CASE=${TRCLV}

echo ${MAQUI}
echo ${AUX_QUEUE}
echo ${RUNTM}
echo ${EXT}

export PBS_SERVER=aux20-eth4
cd ${HOME_suite}/run

mkdir -p ${DK_suite}/rdpert/output

# Script de submissão
SCRIPTSFILE=setrdpt.${RESOL}${NIVEL}.${LABELI}.${MAQUI}

MONITORID=${RANDOM}

cat <<EOT0 > ${SCRIPTSFILE}
#!/bin/bash -x
#PBS -o ${DK_suite}/rdpert/output/${SCRIPTSFILE}.${RUNTM}.out
#PBS -e ${DK_suite}/rdpert/output/${SCRIPTSFILE}.${RUNTM}.err
#PBS -S /bin/bash
#PBS -l walltime=0:01:00
#PBS -l select=1:ncpus=1
#PBS -A CPTEC
#PBS -V
#PBS -N RDPT${PREFIC}
#PBS -q ${AUX_QUEUE}

export PBS_SERVER=aux20-eth4

cd ${HOME_suite}/run
. ${FILEENV} ${1} ${2}

#
#  Set date (year,month,day) and hour (hour:minute) 
#
#  DATE=yyyymmdd
#  HOUR=hh:mn
#

export DATE=\$(date +'%Y')\$(date +'%m')\$(date +'%d')
export HOUR=\$(date +'%H:%M')

echo "Date: "\${DATE}
echo "Hour: "\${HOUR}

#
#  LABELI = yyyymmddhh
#  LABELI = input file label
#

export NUMPERT=${NPERT}
export LABELI=${LABELI}

#
#  Prefix names for the FORTRAN files
#
#  NAMER - Recomposed input file prefix
#  NAMEP - Recomposed perturbed file prefix
#

export NAMER=GANL${PREFIC}

#
#  Suffix names for the FORTRAN files
#  EXTR - Recomposed input file extension
#

export EXTR=R.unf

#
#  Set directories
#
#  HOME_suite  is the directory for sources, scripts and
#              printouts files.
#   DK_suite is the directory for input and output data
#            and bin files.
#   DK_suite is the directory for big selected output files.
#   IHOME_suite is the directory for input file.
#

export HOME_suite DK_suite DK_suite IHOME_suite

echo \${HOME_suite}
echo \${DK_suite}
echo \${DK_suite}
echo \${DK_suite}/model/datain

echo "cp ${DK_suite}/recanl/dataout/${RESOL}${NIVEL}/\${NAMER}\${LABELI}\${EXTR}.${RESOL}${NIVEL} ${DK_suite}/model/datain"
cp ${DK_suite}/recanl/dataout/${RESOL}${NIVEL}/\${NAMER}\${LABELI}\${EXTR}.${RESOL}${NIVEL} ${DK_suite}/model/datain

cd ${HOME_suite}/run

#
#  Set Horizontal Truncation and Vertical Layers
#

LEV=${NIVEL}
TRUNC=${RESOL}
export TRUNC LEV

###
###  Now, build the necessary INCLUDE for the choosen truncation and 
###       vertical resolution.. 
###
##
##if [ "\${COMPILE}" != "run" ]
##then
##
##  cd \${INC}
##
##cat <<EOT1 > rdpert.n
##      INTEGER IMAX,JMAX,MEND,KMAX,IDM
##      PARAMETER (IMAX=${IR},JMAX=${JR},MEND=${MR},KMAX=${KR},IDM=\${NUMPERT})
##EOT1
##
##if (diff rdpert.n rdpert.h > /dev/null)
##then
##  echo "rdpert.n and rdpert.h are the same"
##  rm -f rdpert.n
##else
##  echo "rdpert.n and rdpert.h are different"
##  mv rdpert.n rdpert.h
##fi
##
###
###  End of includes
###
##
##fi

#
#  Now, build the necessary NAMELIST input:
#  Mariane (1999) stdt=0.6 K, stdu=3 m/s
#

cat <<EOT2 > \${DK_suite}/rdpert/datain/rdpert.nml
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
  DIRO='\${DK_suite}/recanl/dataout/\${TRUNC}\${LEV}/ '
  DIRP='\${DK_suite}/rdpert/dataout/\${TRUNC}\${LEV}/ '
  GNAMEO='\${NAMER}\${LABELI}\${EXTR}.\${TRUNC}\${LEV} '
EOT2

mkdir -p \${DK_suite}/rdpert/dataout/\${TRUNC}\${LEV}/ 

i=1

while [ \${i} -le \${NUMPERT} ]
do

  if [ \${i}  -le 9 ]
  then
cat <<EOT3 >> \${DK_suite}/rdpert/datain/rdpert.nml
  GNAMEP(\${i})='GANL0\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}'
EOT3
  else
cat <<EOT3 >> \${DK_suite}/rdpert/datain/rdpert.nml
  GNAMEP(\${i})='GANL\${i}R\${LABELI}\${EXTR}.\${TRUNC}\${LEV}'
EOT3
  fi

  i=\$((\${i}+1))

done

cat <<EOT4 >> \${DK_suite}/rdpert/datain/rdpert.nml
 &END
EOT4

cd ${HOME_suite}/run

#
#  Run Random Perturbation
#

cd ${DK_suite}/rdpert/bin/\${TRUNC}\${LEV}

./rdpert.\${TRUNC}\${LEV} < ${DK_suite}/rdpert/datain/rdpert.nml > ${DK_suite}/rdpert/output/rdpert.out.\${LABELI}.\${HOUR}.\${RESOL}\${NIVEL}

touch ${DK_suite}/rdpert/bin/\${RESOL}\${NIVEL}/monitor.${MONITORID}
EOT0

# Submete o script e aguarda o fim da execução
chmod +x ${SCRIPTSFILE} 

qsub ${SCRIPTSFILE}

until [ -e "${DK_suite}/rdpert/bin/${RESOL}${NIVEL}/monitor.${MONITORID}" ]; do sleep 1s; done

rm ${DK_suite}/rdpert/bin/${RESOL}${NIVEL}/monitor.${MONITORID}

exit 0
