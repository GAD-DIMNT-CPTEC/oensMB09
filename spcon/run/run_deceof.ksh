#! /bin/ksh
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a decomposição em coeficientes espectrais das análises
# perturbadas por EOF em ponto de grade para o Sistema de Previsão por 
# Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_deceof.ksh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5>
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
#            <opcao4> data      -> data da análise corrente (a partir
#                                  da qual as previsões foram feitas)
#
#            <opcao5> membro    -> tamanho do conjunto
#
#  Uso/Exemplos: ./run_deceof.bash TQ0126L028 EOF YES 2012123118 7
#                (decompõe o conjunto de 7+7 análises perturbadas por EOF
#                válidas para 2012123118 na resolução TQ0126L028; serão
#                criadas 7 análises com o sufixo N e 7 análises com o
#                sufixo P)
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

. ${FILEENV} ${1}

cd ${HOME_suite}/../run

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
  echo "Third argument is not set (H)"
  exit
else
  HUMID=${3}
fi

if [ -z "${4}" ]
then
  echo "Fourth argument is not set (LABELI: yyyymmddhh)"
  exit
else
  LABELI=${4}
fi

if [ -z "${5}" ]
then
  echo "Fifth argument is not set (NPERT)"
  exit
else
  NPERT=${5}
fi

export NUMPERT=${NPERT}

# As variáveis a seguir são utilizadas na composição dos nomes dos arquivos com as perturbações por EOF
MPHN=1; MPTR=1; MPHS=1; MPSAN=1; MPSAS=1
MTHN=1; MTTR=1; MTHS=1; MTSAN=1; MTSAS=1
MQHN=1; MQTR=1; MQHS=1; MQSAN=1; MQSAS=1
MUHN=1; MUTR=1; MUHS=1; MUSAN=1; MUSAS=1
MVHN=1; MVTR=1; MVHS=1; MVSAN=1; MVSAS=1

# Variáveis utilizadas no script de submissão
export PBS_SERVER=aux20-eth4

cd ${HOME_suite}/../run

RUNTM=$(date +"%s")

# Script de submissão
SCRIPTSFILES=setdec${2}.${RESOL}.${LABELI}.${MAQUI}

MONITORID=${RANDOM}

cat <<EOT0 > ${SCRIPTSFILES}
#! /bin/bash -x
#PBS -o ${DK_suite}/../deceof/output/setdeceof${2}${RESOL}${LABELI}.${MAQUI}.${RUNTM}.out
#PBS -e ${DK_suite}/../deceof/output/setdeceof${2}${RESOL}${LABELI}.${MAQUI}.${RUNTM}.err
#PBS -l walltime=0:15:00
#PBS -l mppnppn=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -J 1-${NUMPERT}
#PBS -N  ENSEOF
#PBS -q ${AUX_QUEUE}

export NUM=\$(printf %02g \${PBS_ARRAY_INDEX})
export PREFXI=\${NUM}

#
#  Set date (year,month,day) and hour (hour:minute) 
#
#  DATE=yymmdd
#  HOUR=hh:mn
#

DATE=\$(date +'%Y')\$(date +'%m')\$(date +'%d')
HOUR=\$(date +'%H:%M')

echo "Date: "\${DATE}
echo "Hour: "\${HOUR}
export DATE HOUR

#
#  Set labels (date, UTC hour, ...)
#
#  LABELI = yyyymmddhh
#  LABELI = input file label
#

export LABELI=${LABELI}

#
#  Prefix names for the FORTRAN files
#
#  NAMEL - List file name prefix
#  GNAME - Initial condition file name prefix
#  NAMER - Input gridded file name prefix
#  NAMES - Output spectral file name prefix
#

export NAMEL=GEOFPE\${NUM}
export GNAME=GANL\${PREFXI}R
export NAMER=GANL\${PREFXI}R
export NAMES1=GANL\${NUM}P
export NAMES3=GANL\${NUM}N

#
#  Suffix names for the FORTRAN files
#
#  EXTL - List file name suffix
#  EXTG - Initial condition file name suffix
#  ERRi - Input gridded file name suffix
#  ERSi - Output spectral file name suffix
#

export EXTL=P.rpt
export EXTG=S.unf
export ERR1=P.rp1
export ERR2=P.rp2
export ERR3=P.rp3
export ERS1=S.rp1
export ERS2=S.rp2
export ERS3=S.rp3

#
#  Set directories
#

echo \${HOME_suite}
echo \${DK_suite}
echo \${DK_suite}/model/datain

#
#  Set Horizontal Truncation and Vertical Layers
#

export TRUNC=$(echo ${TRC} |awk '{ printf("TQ%4.4d\n",$1)  }' )
export LEV=$(echo ${LV} |awk '{ printf("L%3.3d\n",$1)  }' )

#
#  Set machine
#

export MACH=${MAQUI}

#
#  Now, build the necessary NAMELIST input:
#

GNAMEL=\${NAMEL}\${LABELI}\${EXTL}.\${TRUNC}\${LEV}

cat <<EOT3 > \${DK_suite}/../deceof/datain/deceof\${NUM}.nml
 &DATAIN
  GNAMEL='\${GNAMEL} '
  DIRL='\${DK_suite}/../deceof/datain/ '
  DIRI='\${DK_suite}/model/datain/ '
  DIRG='\${DK_suite}/../eof/dataout/\${TRUNC}\${LEV}/ '
  DIRS='\${DK_suite}/model/datain/ '
 &END
 &HUMIDI
  HUM='${HUMID}'
 &END
EOT3

filephn=prssehn\${NUM}${MPHN}\${LABELI}
fileptr=prssetr\${NUM}${MPTR}\${LABELI}
filephs=prssehs\${NUM}${MPHS}\${LABELI}
filepsan=prssesan\${NUM}${MPSAN}\${LABELI}
filepsas=prssesas\${NUM}${MPSAS}\${LABELI}

echo "filephn= "\${filephn} 
echo "fileptr= "\${fileptr} 
echo "filephs= "\${filephs} 
echo "filepsan="\${filepsan} 
echo "filepsas="\${filepsas} 

filethn=tempehn\${NUM}${MTHN}\${LABELI}
filettr=tempetr\${NUM}${MTTR}\${LABELI}
fileths=tempehs\${NUM}${MTHS}\${LABELI}
filetsan=tempesan\${NUM}${MTSAN}\${LABELI}
filetsas=tempesas\${NUM}${MTSAS}\${LABELI}

echo "filethn= "\${filethn} 
echo "filettr= "\${filettr} 
echo "fileths= "\${fileths} 
echo "filetsan="\${filetsan} 
echo "filetsas="\${filetsas} 

fileqhn=humpehn\${NUM}${MQHN}\${LABELI}
fileqtr=humpetr\${NUM}${MQTR}\${LABELI}
fileqhs=humpehs\${NUM}${MQHS}\${LABELI}
fileqsan=humpesan\${NUM}${MQSAN}\${LABELI}
fileqsas=humpesas\${NUM}${MQSAS}\${LABELI}

echo "fileqhn= "\${fileqhn} 
echo "fileqtr= "\${fileqtr} 
echo "fileqhs= "\${fileqhs} 
echo "fileqsan="\${fileqsan} 
echo "fileqsas="\${fileqsas} 

fileuhn=winpehn\${NUM}${MUHN}\${LABELI}
fileutr=winpetr\${NUM}${MUTR}\${LABELI}
fileuhs=winpehs\${NUM}${MUHS}\${LABELI}
fileusan=winpesan\${NUM}${MUSAN}\${LABELI}
fileusas=winpesas\${NUM}${MUSAS}\${LABELI}

echo "fileuhn= "\${fileuhn} 
echo "fileutr= "\${fileutr} 
echo "fileuhs= "\${fileuhs} 
echo "fileusan="\${fileusan} 
echo "fileusas="\${fileusas} 

filevhn=winpehn\${NUM}${MVHN}\${LABELI}
filevtr=winpetr\${NUM}${MVTR}\${LABELI}
filevhs=winpehs\${NUM}${MVHS}\${LABELI}
filevsan=winpesan\${NUM}${MVSAN}\${LABELI}
filevsas=winpesas\${NUM}${MVSAS}\${LABELI}

echo "filevhn= "\${filevhn} 
echo "filevtr= "\${filevtr} 
echo "filevhs= "\${filevhs} 
echo "filevsan="\${filevsan} 
echo "filevsas="\${filevsas} 

rm -f \${DK_suite}/../deceof/datain/\${GNAMEL}

cat <<EOT2 > \${DK_suite}/../deceof/datain/\${GNAMEL}
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
#  Run Decomposition
#

cd \${HOME_suite}/../deceof/bin/\${TRUNC}\${LEV}

./deceof.\${TRUNC}\${LEV} < \${HOME_suite}/../deceof/datain/deceof\${NUM}.nml > \${HOME_suite}/../deceof/output/deceof.\${NUM}.${LABELI}.\${HOUR}.\${TRUNC}\${LEV}

filephn=prssnhn\${NUM}${MPHN}\${LABELI}
fileptr=prssntr\${NUM}${MPTR}\${LABELI}
filephs=prssnhs\${NUM}${MPHS}\${LABELI}
filepsan=prssnsan\${NUM}${MPSAN}\${LABELI}
filepsas=prssnsas\${NUM}${MPSAS}\${LABELI}

echo "filephn= "\${filephn} 
echo "fileptr= "\${fileptr} 
echo "filephs= "\${filephs} 
echo "filepsan="\${filepsan} 
echo "filepsas="\${filepsas} 

filethn=tempnhn\${NUM}${MTHN}\${LABELI}
filettr=tempntr\${NUM}${MTTR}\${LABELI}
fileths=tempnhs\${NUM}${MTHS}\${LABELI}
filetsan=tempnsan\${NUM}${MTSAN}\${LABELI}
filetsas=tempnsas\${NUM}${MTSAS}\${LABELI}

echo "filethn= "\${filethn} 
echo "filettr= "\${filettr} 
echo "fileths= "\${fileths} 
echo "filetsan="\${filetsan} 
echo "filetsas="\${filetsas} 

fileqhn=humpnhn\${NUM}${MQHN}\${LABELI}
fileqtr=humpntr\${NUM}${MQTR}\${LABELI}
fileqhs=humpnhs\${NUM}${MQHS}\${LABELI}
fileqsan=humpnsan\${NUM}${MQSAN}\${LABELI}
fileqsas=humpnsas\${NUM}${MQSAS}\${LABELI}

echo "fileqhn= "\${fileqhn} 
echo "fileqtr= "\${fileqtr} 
echo "fileqhs= "\${fileqhs} 
echo "fileqsan="\${fileqsan} 
echo "fileqsas="\${fileqsas} 

fileuhn=winpnhn\${NUM}${MUHN}\${LABELI}
fileutr=winpntr\${NUM}${MUTR}\${LABELI}
fileuhs=winpnhs\${NUM}${MUHS}\${LABELI}
fileusan=winpnsan\${NUM}${MUSAN}\${LABELI}
fileusas=winpnsas\${NUM}${MUSAS}\${LABELI}

echo "fileuhn= "\${fileuhn} 
echo "fileutr= "\${fileutr} 
echo "fileuhs= "\${fileuhs} 
echo "fileusan="\${fileusan} 
echo "fileusas="\${fileusas} 

filevhn=winpnhn\${NUM}${MVHN}\${LABELI}
filevtr=winpntr\${NUM}${MVTR}\${LABELI}
filevhs=winpnhs\${NUM}${MVHS}\${LABELI}
filevsan=winpnsan\${NUM}${MVSAN}\${LABELI}
filevsas=winpnsas\${NUM}${MVSAS}\${LABELI}

echo "filevhn= "\${filevhn} 
echo "filevtr= "\${filevtr} 
echo "filevhs= "\${filevhs} 
echo "filevsan="\${filevsan} 
echo "filevsas="\${filevsas} 

rm -f \${DK_suite}/../deceof/datain/\${GNAMEL}

cat <<EOT4 > \${DK_suite}/../deceof/datain/\${GNAMEL}
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
#  Run Decomposition
#

cd \${HOME_suite}/../deceof/bin/\${TRUNC}\${LEV}

./deceof.\${TRUNC}\${LEV} < \${HOME_suite}/../deceof/datain/deceof\${NUM}.nml > \${HOME_suite}/../deceof/output/deceof.\${NUM}.${LABELI}.\${HOUR}.\${TRUNC}\${LEV}

#echo "" >> ${DK_suite}/../deceof/bin/\${RESOL}\${NIVEL}/monitor.${MONITORID}
EOT0

# Submete o script e aguarda o fim da execução
chmod +x ${HOME_suite}/../run/${SCRIPTSFILES}

qsub -W block=true ${SCRIPTSFILES}

#until [ -e "${DK_suite}/../deceof/bin/${RESOL}${NIVEL}/monitor.${MONITORID}" ]; do sleep 1s; done

echo "SUBMIT: ${HOME_suite}/../run/${SCRIPTSFILES}"

#rm ${DK_suite}/../deceof/bin/${RESOL}${NIVEL}/monitor.${MONITORID}

exit 0
