#! /bin/bash -e 
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
#      ./config_spcon.sh <opcao1> <opcao2> <opcao3>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> ação       -> configura ou limpa a instalação
#                                   configure: cria diretórios e links
#                                              simbólicos
#                                   cleanall: remove os diretórios e os
#                                             links simbólicos criados
#
#            <opcao2> resolucao  -> resolução espectral do modelo
#                                
#            <opcao3> compilador -> mnemônico do compilador a ser
#                                   utilizado (gnu, cray ou pgi)
#                                   Obs.: na presente versão (2.2.0)
#                                   apenas o compilador gnu foi testado
#
#  Uso/Exemplos: ./config_spcon.sh configure TQ0126L028 pgi
#                ./config_spcon.sh configure TQ0126L028 gnu
#                ./config_spcon.sh configure TQ0126L028 cray
#                ./config_spcon.sh configure TQ0213L042 pgi
#                ./config_spcon.sh configure TQ0213L042 gnu
#                ./config_spcon.sh configure TQ0213L042 cray
#                ou
#                ./config_spcon.sh cleanall TQ0126L028 gnu
#                etc
# 
# !REVISION HISTORY:
#
# 10 Agosto de 2018   - C. F. Bastarz - Documentação da versõa inicial.  
# 21 Outubro de 2019  - C. F. Bastarz - Inclusão da escolha do compilador.
# 21 Julho de 2021    - C. F. Bastarz - Alterada declaração do vetor Procs;
#                                       Incluída alterações nos config/Makefile.conf.*
# 05 Agosto de 2021   - C. F. Bastarz - Simplificação do script para a versão 2.2.0.
# 31 Maio de 2022     - C. F. Bastarz - Adaptações para a máquina EGEON.
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

if [ "${1}" = "help" -o -z "${1}" -o ${#} != 3 ]
then
  cat < ${0} | sed -n '/^#BOP/,/^#EOP/p'
  exit 0
else
  action=${1}
  res=${2}
  comp=${3}
fi

home_spcon=${PWD}
spcon_include=${home_spcon}/include
spcon_produtos=${home_spcon}/produtos

host=$(hostname)

if [ ${action} == "configure" ]
then
  TRUNC=$(echo ${res} | awk -F "L" '{print $1}')
  LEV=L$(echo ${res} | awk -F "L" '{print $2}')

  sed -i "s,^TRUNC=.*,TRUNC=${TRUNC},g" ./config/Makefile.conf.${comp}
  sed -i "s,^LEV=.*,LEV=${LEV},g" ./config/Makefile.conf.${comp}
  if [ $(echo ${host} | cut -c 1-3) == "hea" ] # EGEON
  then
    sed -i "s,^F90 =.*,F90 = mpif90,g" ./config/Makefile.conf.${comp}
    sed -i "s,^FC  =.*,FC  = mpif90,g" ./config/Makefile.conf.${comp}
    sed -i "s,^F77 =.*,F77 = mpif90,g" ./config/Makefile.conf.${comp}
    sed -i "s,^LD  =.*,LD  = mpif90,g" ./config/Makefile.conf.${comp}
  else # Cray XE6/XC50
    sed -i "s,^F90 =.*,F90 = ftn,g" ./config/Makefile.conf.${comp}
    sed -i "s,^FC  =.*,FC  = ftn,g" ./config/Makefile.conf.${comp}
    sed -i "s,^F77 =.*,F77 = ftn,g" ./config/Makefile.conf.${comp}
    sed -i "s,^LD  =.*,LD  = ftn,g" ./config/Makefile.conf.${comp}
  fi
elif [ ${action} == "cleanall" ]
then
  cd ${home_spcon}
  make comp=${comp} clean
fi

# Processos do método de perturbação MB09

Procs=(decanl deceof rdpert recanl recfct eofhumi eofpres eoftemp eofwind fftpln ensmed)

for proc in ${Procs[@]}
do
  dir_proc=${home_spcon}/${proc}

  if [ ${action} == "configure" ]
  then
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
  elif [ ${action} == "cleanall" ]
  then
    if [ ${proc} == "fftpln" ]
    then
      rm -rf ${dir_proc}/lib/${res}
    elif [ ${proc} == "decanl" -o ${proc} == "deceof" -o ${proc} == "rdpert" -o ${proc} == "recanl" -o ${proc} == "recfct" ]
    then
      rm -rf ${dir_proc}/bin/${res}
      rm -rf ${dir_proc}/output
      rm -rf ${dir_proc}/datain
      rm -rf ${dir_proc}/dataout
    fi
  
    if [ ${proc} == "ensmed" ]
    then
      rm -rf ${dir_proc}/bin
      rm -rf ${dir_proc}/output
      rm -rf ${dir_proc}/dataout
    fi
  
    rm -rf ${home_spcon}/eof/bin/${res}
    rm -rf ${home_spcon}/eof/output
    rm -rf ${home_spcon}/eof/datain
    rm -rf ${home_spcon}/eof/dataout
  
    cd ${dir_proc}
  
    unlink ${dir_proc}/include
    unlink ${dir_proc}/source/Makefile
  fi
done

# Produtos do SPCON

if [ ${action} == "configure" ]
then
  cd ${spcon_produtos}/libs/w3lib-1.4
  ln -sfn ${spcon_produtos}/libs/w3lib-1.4/Makefile.${comp}* ${spcon_produtos}/libs/w3lib-1.4/Makefile
  #ln -sfn ${spcon_produtos}/libs/w3lib-1.4/Makefile.${comp} ${spcon_produtos}/libs/w3lib-1.4/Makefile
elif [ ${action} == "cleanall" ] 
then
  cd ${spcon_produtos}
  make clean
  unlink ${spcon_produtos}/libs/w3lib-1.4/Makefile
fi

Prods=(cluster chievol spread spaguetti probagr probability plumes perturbations grh)

for prod in ${Prods[@]}
do
  dir_prod=${spcon_produtos}/${prod}

  if [ ${action} == "configure" ]
  then
    if [ ${prod} == "cluster" -o ${prod} == "spread" -o ${prod} == "probagr" -o ${prod} == "probability" -o ${prod} == "plumes" ]
    then
      mkdir -p ${dir_prod}/bin
      if [ ${prod} != "spread" -o ${prod} != "plumes" ]; then mkdir -p ${dir_prod}/gif; fi
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
  elif [ ${action} == "cleanall" ]
  then
    if [ ${prod} == "cluster" -o ${prod} == "spread" -o ${prod} == "probagr" -o ${prod} == "probability" -o ${prod} == "plumes" ]
    then
      rm -rf ${dir_prod}/bin
      rm -rf ${dir_prod}/gif
      rm -rf ${dir_prod}/output
      rm -rf ${dir_prod}/dataout
  
      unlink ${dir_prod}/source/Makefile
    elif [ ${prod} == "chievol" -o ${prod} == "spaguetti" -o ${prod} == "perturbations" ] 
    then
      rm -rf ${dir_prod}/gif
    elif [ ${prod} == "grh" ]
    then
      rm -rf ${dir_prod}/exec
      rm -rf ${dir_prod}/datain
      rm -rf ${dir_prod}/dataout
    fi
  fi
done

exit 0
