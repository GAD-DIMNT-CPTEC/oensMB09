#! /bin/bash
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script simples para a criar a estrutura de diretórios e a resolução
# do Sistema de Previsão por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./config_spcon.sh <opcao1> <opcao2>
#
# !INPUT PARAMETERS:
#
#  Uso/Exemplos: ./config_spcon.sh TQ0126L028 pgi
#                ./config_spcon.sh TQ0126L028 gnu
#                ./config_spcon.sh TQ0126L028 cray
#                ./config_spcon.sh TQ0213L042 pgi
#                ./config_spcon.sh TQ0213L042 gnu
#                ./config_spcon.sh TQ0213L042 cray
# 
# !REVISION HISTORY:
#
# 10 Agosto de 2018   - C. F. Bastarz - Documentação da versõa inicial.  
# 21 Outubro de 2019  - C. F. Bastarz - Inclusão da escolha do compilador.
# 21 Julho de 2021    - C. F. Bastarz - Alterada declaração do vetor Procs;
#                                       Incluída alterações nos config/Makefile.conf.*
# 05 Agosto de 2021   - C. F. Bastarz - Simplificação do script para a versão 2.2.0.
#
# !REMARKS:
#
# !BUGS:
#
#   Nenhum até o momento.
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
#set -o xtrace

#
# Menu de opções/ajuda
#

if [ "${1}" = "help" -o -z "${1}" ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
fi

res=${1}
comp=${2}

home_spcon=${PWD}
spcon_include=${home_spcon}/include
spcon_produtos=${home_spcon}/produtos

TRUNC=$(echo ${res} | awk -F "L" '{print $1}')
LEV=L$(echo ${res} | awk -F "L" '{print $2}')

sed -i "s,^TRUNC=.*,TRUNC=${TRUNC},g" ./config/Makefile.conf.${comp}
sed -i "s,^LEV=.*,LEV=${LEV},g" ./config/Makefile.conf.${comp}

# Processos do método de perturbação MB09

Procs=(decanl deceof rdpert recanl recfct eofhumi eofpres eoftemp eofwind fftpln ensmed)

for proc in ${Procs[@]}
do

  dir_proc=${home_spcon}/${proc}

  if [ ${proc} == "fftpln" ]
  then
    mkdir -p ${dir_proc}/lib/${res}
  elif [ ${proc} == "decanl" -o ${proc} == "deceof" -o ${proc} == "rdpert" -o ${proc} == "recanl" -o ${proc} == "recfct" ]
  then
    mkdir -p ${dir_proc}/bin/${res}
    mkdir -p ${dir_proc}/output
    mkdir -p ${dir_proc}/datain
    mkdir -p ${dir_proc}/dataout
  fi

  if [ ${proc} == "ensmed" ]
  then
    mkdir -p ${dir_proc}/bin
    mkdir -p ${dir_proc}/output
    mkdir -p ${dir_proc}/dataout
  fi

  mkdir -p ${home_spcon}/eof/bin/${res}
  mkdir -p ${home_spcon}/eof/output
  mkdir -p ${home_spcon}/eof/datain
  mkdir -p ${home_spcon}/eof/dataout

  cd ${dir_proc}

  ln -sfn ${spcon_include}/${res} include
  ln -sfn ${dir_proc}/source/Makefile.${comp} ${dir_proc}/source/Makefile

done

# Produtos do SPCON

cd ${spcon_produtos}/libs/w3lib-1.4
ln -sfn ${spcon_produtos}/libs/w3lib-1.4/Makefile.${comp}* ${spcon_produtos}/libs/w3lib-1.4/Makefile

Prods=(cluster chievol spread spaguetti probagr probability plumes perturbations grh)

for prod in ${Prods[@]}
do

  dir_prod=${spcon_produtos}/${prod}

  if [ ${prod} == "cluster" -o ${prod} == "spread" -o ${prod} == "probagr" -o ${prod} == "probability" -o ${prod} == "plumes" ]
  then
    mkdir -p ${dir_prod}/bin
    mkdir -p ${dir_prod}/gif
    mkdir -p ${dir_prod}/output
    mkdir -p ${dir_prod}/dataout

    ln -sfn ${dir_prod}/source/Makefile.${comp} ${dir_prod}/source/Makefile
  elif [ ${prod} == "chievol" -o ${prod} == "spaguetti" -o ${prod} == "perturbations" ] 
  then
    mkdir -p ${dir_prod}/gif
  elif [ ${prod} == "grh" ]
  then
    mkdir -p ${dir_prod}/exec
    mkdir -p ${dir_prod}/datain
    mkdir -p ${dir_prod}/dataout
  fi

done

exit 0
