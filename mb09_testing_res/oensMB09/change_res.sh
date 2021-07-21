#! /bin/bash
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a alterar a resolução do Sistema de Previsão
# por Conjunto Global (SPCON) do CPTEC, após a sua configuração.
#
# !INTERFACE:
#      ./config_spcon.sh <opcao1> <opcao2>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> testcase   -> aloca os dados necessário para testar
#                                   a instalação
#
#  Uso/Exemplos: ./change_res.sh TQ0213L042 pgi
#                ./change_res.sh TQ0213L042 cray
#                ./change_res.sh TQ0126L028 pgi
#                ./change_res.sh TQ0126L028 cray
# 
# !REVISION HISTORY:
#
# 10 Agosto de 2018   - C. F. Bastarz - Documentação da versõa inicial.  
# 21 Outubro de 2019  - C. F. Bastarz - Inclusão da escolha do compilador.
# 21 Julho de 2021    - C. F. Bastarz - Alterada declaração do vetor Procs;
#                                       Incluída alterações nos config/Makefile.conf.*
#
# !REMARKS:
#
#  Em algum momento este script será incorporado como uma função
#  do script config_spcon.sh
#
# !BUGS:
#
#   Nenhum ate o momento.
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
#set -o xtrace

if [ $# -ne 2 ]
then
  echo "Uso: ./change_res.sh TQXXXXLXXX comp"
  echo "./change_res.sh TQ0126L028 cray"
  exit 1
else
  res=${1}
  comp=${2}
fi

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
