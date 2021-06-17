#! /bin/bash -x
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 220  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para converter os arquivos binários com os campos de espalhamento
# em tabelas no estilo do SCANTEC, com as médias dos campos para cada 
# tempo de previsão e variáveis de interesse.
#
# !INTERFACE:
#      ./run_spread_to_table.sh <opcao1> <opcao2> <opcao3> <opcao4>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> resolucao    -> resolução espectral do modelo
#                                
#            <opcao2> data_inicial -> data da análise corrente (a partir
#                                     da qual as previsões foram feitas)
#
#            <opcao3> data_final   -> data da ultima previsao (a partir
#                                     da análise
#
#            <opcao4> regiao       -> (opcional) nome da regiao a ser avaliada
#                                     (ge, gl, hn, tr, hs, as)
#                                     se este argumento nao for passado,
#                                     todas as regioes serao consideradas
#
#  Uso/Exemplos: ./run_spread_to_table.sh TQ0126L028 2020060100 2020061600 GE
#
# !REVISION HISTORY:
#
# 12 Novembro de 2020 - C. F. Bastarz - Versão inicial.  
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

if [ -z "${1}" ]
then
  echo "First argument is not set (RES: TQXXXXLXXX)"
  exit
else
  RES=${1}
fi

if [ -z "${2}" ]
then
  echo "Second argument is not set (LABELI: yyyymmddhh)"
  exit
else
  LABELI=${2}
fi

if [ -z "${3}" ]
then
  echo "Third argument is not set (LABELF: yyyymmddhh)"
  exit
else
  LABELF=${3}
fi

if [ -z "${4}" ]
then
  echo "Fourth argument is not set (NREG)"
else
  NREG=${4}
fi

export inctime=${HOME}/bin/inctime

export HOME_suite=/lustre_xc50/carlos_bastarz/spread2table
export SPREADOUT=/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/produtos/spread/dataout/${RES}

cd ${HOME_suite}/

# Cria a lista de arquivos de entrada
ls ${SPREADOUT}/${LABELI}/*.bin > ${HOME_suite}/lista_${LABELI}${LABELF}.txt
sed -i '/inz/d' ${HOME_suite}/lista_${LABELI}${LABELF}.txt

RUNTM=$(date +"%s")

export MAQUI=$(uname -s)

rm ${HOME_suite}/${NREG,,}/monitor.t

# Script de submissão
SCRIPTSFILES=setspread2table.${RES}.${LABELI}.${MAQUI}

cat <<EOT0 > ${SCRIPTSFILES}
#! /bin/bash 
#PBS -o ${HOME_suite}/setspread2table${RES}${LABELI}.${MAQUI}.${RUNTM}.out
#PBS -e ${HOME_suite}/setspread2table${RES}${LABELI}.${MAQUI}.${RUNTM}.err
#PBS -l walltime=0:15:00
#PBS -l mppnppn=1
#PBS -A CPTEC
#PBS -V
#PBS -S /bin/bash
#PBS -N SPREAD2TABLE
#PBS -q pesq

module swap PrgEnv-cray/6.0.4 PrgEnv-gnu
module unload craype-x86-skylake

mkdir -p ${HOME_suite}/${NREG,,}/

cd ${HOME_suite}

cat <<EOT > ${HOME_suite}/inputparameters.nml
&InputParams
  reg='${NREG,,}',                            ! regiao: ge, gl, hn, tr, hs, as
  verif=F,                                    ! escreve em disco os arquivos lidos (para verificacao)
!  nxpts=384,                                 ! total de pontos de grade em x
!  nypts=192,                                 ! total de pontos de grade em y
!  nzpts=3,                                   ! niveis verticais
!  nvars=7,                                   ! numero de variaveis
  dirin='${HOME_suite}/',                     ! diretorio com a lista de arquivos a serem lidos
  filein='lista_${LABELI}${LABELF}.txt',      ! nome da lista dos arquivos a serem lidos
  dirout='${HOME_suite}/${NREG,,}/',          ! diretorio de saida das tabelas
  fileout='SPRDENS_${LABELI}${LABELF}T.scam', ! nome da tabela com as medias calculadas
/
EOT

aprun -n 1 -N 1 -d 1 ${HOME_suite}/spread2table.x

echo "" > ${HOME_suite}/${NREG,,}/monitor.t
EOT0

# Submete o script e aguarda o fim da execução
chmod +x ${HOME_suite}/${SCRIPTSFILES}

#export PBS_SERVER=${pbs_server2}

qsub -W block=true ${SCRIPTSFILES}

echo "SUBMIT: ${HOME_suite}/${SCRIPTSFILES}"

until [ -e "${HOME_suite}/${NREG,,}/monitor.t" ]; do sleep 1s; done


exit 0
