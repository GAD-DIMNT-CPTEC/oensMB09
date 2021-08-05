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

TRUNC=$(echo ${res} | awk -F "L" '{print $1}')
LEV=L$(echo ${res} | awk -F "L" '{print $2}')

sed -i "s,^TRUNC=.*,TRUNC=${TRUNC},g" ./config/Makefile.conf.${comp}
sed -i "s,^LEV=.*,LEV=${LEV},g" ./config/Makefile.conf.${comp}

Procs=(decanl deceof rdpert recanl recfct eofhumi eofpres eoftemp eofwind fftpln)

for proc in ${Procs[@]}
do

  dir_proc=${home_spcon}/${proc}

  if [ ${proc} == "fftpln" ]
  then
    mkdir -p ${dir_proc}/lib/${res}
  else
    mkdir -p ${dir_proc}/bin/${res}
  fi

  mkdir -p ${home_spcon}/eof/bin/${res}

  cd ${dir_proc}

  ln -sfn ${spcon_include}/${res} include
  ln -sfn ${dir_proc}/source/Makefile.${comp} ${dir_proc}/source/Makefile

done

exit 0
