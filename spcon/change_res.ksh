#! /bin/ksh
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
#      ./config_spcon.ksh <opcao1> <opcao2>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> testcase   -> aloca os dados necessário para testar
#                                   a instalação
#
#  Uso/Exemplos: ./change_res.ksh TQ0213L042 pgi
#                ./change_res.ksh TQ0213L042 cray
#                ./change_res.ksh TQ0126L028 pgi
#                ./change_res.ksh TQ0126L028 cray
# 
# !REVISION HISTORY:
#
# 10 Agosto de 2018   - C. F. Bastarz - Documentação da versõa inicial.  
# 21 Outubro de 2019  - C. F. Bastarz - Inclusão da escolha do compilador.
#
# !REMARKS:
#
#  Em algum momento este script será incorporado como uma função
#  do script config_spcon.ksh
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
  echo "Uso: ./change_res.ksh TQXXXXLXXX comp"
  echo "./change_res.ksh TQ0126L028 cray"
  exit 1
else
  res=${1}
  comp=${2}
fi

home_spcon=${PWD}
spcon_include=${home_spcon}/include

set -A Procs decanl deceof rdpert recanl recfct eofhumi eofpres eoftemp eofwind fftpln

for proc in ${Procs[@]}
do

  dir_proc=${home_spcon}/${proc}

  cd ${dir_proc}

  ln -sfn ${spcon_include}/${res} include
  ln -sfn ${dir_proc}/source/Makefile.${comp} ${dir_proc}/source/Makefile

done

exit 0
