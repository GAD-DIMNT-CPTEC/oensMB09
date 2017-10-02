#! /bin/ksh
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a instalação da estrutura básica do Sistema de Previsão
# por Conjunto Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./config_spcon.ksh <opcao1> <opcao2>
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> testcase   -> aloca os dados necessário para testar
#                                   a instalação
#
#                     model      -> faz checkout de uma revisão do 
#                                   modelo atmosférico BAM
#
#                     inctime    -> faz checkout de uma revisão do
#                                   utilitário inctime
#
#                     compilar   -> compila o SPCON (módulos de 
#                                   perturbação e BAM)
#
#                     configurar -> compila o SPCON (módulos de 
#                                   perturbação e BAM)
#
#                     ajuda      -> mostra esta ajuda
#
#            <opcao2> rev        -> funciona apenas com as opções model e inctime
#                                   (indica o número da revisão a ser
#                                   baixada)
#                                   para escolher uma revisão do BAM e/ou inctime,
#                                   acesse as páginas:
# https://projetos.cptec.inpe.br/projects/smg/repository/show/trunk/SMG/cptec/bam
# https://projetos.cptec.inpe.br/projects/smg/repository/show/trunk/SMG/util/inctime
#
#  Uso/Exemplos: ./config_spcon.ksh model 
#                ./config_spcon.ksh model 200 (alternativo)   
#                ./config_spcon.ksh inctime
#                ./config_spcon.ksh inctime 200 (alternativo)
#                ./config_spcon.ksh compilar
#                ./config_spcon.ksh testcase
#                ./config_spcon.ksh configurar
#                ./config_spcon.ksh ajuda  
# 
# !REVISION HISTORY:
#
# 14 Agosto de 2017 - C. F. Bastarz   - Versão inicial.  
# 15 Agosto de 2017 - C. F. Bastarz   - Inclusão comentários.
# 17 Agosto de 2017 - C. F. Bastarz   - Inclusão da compilação do inctime.
# 20 Setembro de 2017 - C. F. Bastarz - Inclusão da função configurar
# 21 Setembro de 2017 - C. F. Bastarz - Incrementada a função configurar
# 02 Outubro de 2017 - C. F. Bastarz  - Incluído comando para criar o arquivo VARIAVEIS
#                                       a partir da função "configurar"; modificada a função
#                                       de export do modelo BAM; incluida a opção de export
#                                       do inctime
#           
#
# !REMARKS:
#
# !BUGS:
#
#   Nenhum ate o momento.
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
set -o xtrace

# Função vars_export (contém as variáveis utilizadas na configuração
# e instalação do SPCON)
vars_export() {

  export spcon_name=spcon_bam_mb09

  export home_spcon=${SUBMIT_HOME}/${spcon_name}
  export work_spcon=${WORK_HOME}/${spcon_name}

  export spcon_run=${home_spcon}/run

  export home_bam=${home_spcon}/bam

  export bam_run=${home_bam}/run

  export bam_pre=${home_bam}/pre
  export bam_model=${home_bam}/model
  export bam_pos=${home_bam}/pos

  export model_datain=${bam_model}/datain

  export pre_source=${bam_pre}/sources
  export model_source=${bam_model}/source
  export pos_source=${bam_pos}/source

  export pre_exec=${bam_pre}/exec
  export model_exec=${bam_model}/exec
  export pos_exec=${bam_pos}/exec

  export util_spcon=${home_spcon}/util

  export util_inctime=${util_spcon}/inctime

  export spcon_testcase=/scratchin/grupos/assim_dados/home/carlos.bastarz/testcase_spcon

}

# Função testcase (copia os dados de teste da instalação do SPCON)
testcase() {

  vars_export

  echo "Testcase"

  cp -pvfr ${spcon_testcase}/pre/* ${bam_pre}/
  cp -pvfr ${spcon_testcase}/model/* ${bam_model}/
  cp -pvfr ${spcon_testcase}/pos/* ${bam_pos}/

}

# Função configurar (cria links simbólicos de bam/run para ../../run)
configurar() {

  vars_export

  echo "Configurar"

  if [ -d "${bam_run}" ]
  then

    cd ${spcon_run}/

    ln -svf ${bam_run}/* .

  else

    echo "Diretório ${bam_run} não existe!"
    exit 1

  fi

  # Cria os links simbólicos dos diretórios do SPCON
  set -A Procs decanl deceof eof rdpert recanl recfct

  for proc in ${Procs[@]}
  do

    dir_proc=${home_spcon}/${proc}

    if [ ${proc} == "decanl" -o ${proc} == "deceof" ]
    then

      proc_dataout=${model_datain} # As análises espectrais perturbadas devem ser encontradas no model/datain
      proc_output=${work_spcon}/${proc}/output

    else

      proc_dataout=${work_spcon}/${proc}/dataout
      proc_output=${work_spcon}/${proc}/output

    fi

    cd ${dir_proc}

    if [ ! -d "${proc_dataout}" ]; then mkdir -p ${proc_dataout}; fi
    ln -sf ${proc_dataout}

    if [ ! -d "${proc_output}" ]; then mkdir -p ${proc_output}; fi
    ln -sf ${proc_output}

  done

  # Cria um arquivo texto com os valores das variáveis da função "vars_export"
  cd ${home_spcon}

  nohup /bin/bash -x ./config_spcon.ksh vars_export > .VARIAVEIS1 2> VARIAVEIS < /dev/null &
  rm .VARIAVEIS1

  # Altera os arquivos de configuração do BAM para a instalação corrente
  sed -i "s,HOMEBASE=/scratchin/grupos/assim_dados/home/\${USER}/SMG/cptec/bam,HOMEBASE=${home_bam},g" ${bam_run}/EnvironmentalVariables
  sed -i "s,SUBTBASE=/scratchin/grupos/assim_dados/home/\${USER}/SMG/datainout/bam,SUBTBASE=${home_bam},g" ${bam_run}/EnvironmentalVariables
  sed -i "s,WORKBASE=/scratchout/grupos/assim_dados/home/\${USER}/SMG/datainout/bam,WORKBASE=${home_bam},g" ${bam_run}/EnvironmentalVariables

  sed -i "s,PATHBASE=/scratchin/grupos/assim_dados/home/\${USER}/SMG/cptec/bam,PATHBASE=${home_bam},g" ${bam_run}/EnvironmentalVariablesMCGA
  sed -i "s,DK=/scratchin/grupos/assim_dados/home/\${USER}/SMG/datainout/bam,DK=${home_bam},g" ${bam_run}/EnvironmentalVariablesMCGA
  sed -i "s,DK2=/scratchin/grupos/assim_dados/home/\${USER}/SMG/datainout/bam,DK2=${home_bam},g" ${bam_run}/EnvironmentalVariablesMCGA

  sed -i "s,/scratchin/grupos/assim_dados/home/carlos.bastarz/oensMB09_bam/bam/pre/datasst/oiv2monthly/,${bam_pre}/datasst/oiv2monthly/,g" ${bam_pre}/datasst/oiv2monthly/sstmtd.nml

}

# Função model (realiza uma retirada de uma revisão do BAM 
# a partir do SVN do Sistema de Modelagem Global)
model() {

  vars_export

  if [ -z ${1} ]
  then

    echo "Model"

    svn export https://svn.cptec.inpe.br/smg/trunk/SMG/cptec/bam 

  else

    echo "Model r${1}"

    svn export -r${1} https://svn.cptec.inpe.br/smg/trunk/SMG/cptec/bam 

  fi

}

# Função inctime (realiza uma retirada de uma revisão do inctime
# a partir do SVN do Sistema de Modelagem Global)
inctime() {

  vars_export

  mkdir -p ${util_spcon}

  cd ${util_spcon}

  if [ -z ${1} ]
  then

    echo "inctime"

    svn export https://svn.cptec.inpe.br/smg/trunk/SMG/util/inctime 

  else

    echo "inctime r${1}"

    svn export -r${1} https://svn.cptec.inpe.br/smg/trunk/SMG/util/inctime 

  fi

  cd ${spcon_home}

}

# Função ajuda (mostra o menu de ajuda)
ajuda() {

  echo ""

  echo "Sistema de Previsão por Conjunto Global (SPCON) v1.5 - Agosto de 2017"

  echo ""

  echo "> Principais detalhes desta versão:"
  echo "  * Método de perturbação MB09"
  echo "  * Modelo atmosférico BAM"
  echo "  * https://projetos.cptec.inpe.br/projects/spconcptec/wiki/V15"

  echo ""

  echo "> Uso/Exemplos:"
  echo "  1) ./config_spcon.ksh model 200"
  echo "     * faz checkout da revisão número 200 do BAM"
  echo "  2) ./config_spcon.ksh model"
  echo "     * faz checkout da última revisão do BAM"
  echo "  3) ./config_spcon.ksh inctime"
  echo "     * faz checkout da última revisão do inctime"
  echo "  4) ./config_spcon.ksh compilar"
  echo "     * compila os módulos de perturbação e o modelo BAM"
  echo "  5) ./config_spcon.ksh configurar"
  echo "     * cria diretórios e links simbólicos da instalação"
  echo "  6) ./config_spcon.ksh testcase"
  echo "     * aloca os dados necessários para testar a instalação"
  echo "  7) ./config_spcon.ksh ajuda"
  echo "     * mostra este menu de ajuda"

  echo ""

  echo "> Dúvidas e sugestões: carlos.bastarz@inpe.br"

  echo ""

}

# Função compilar (compila todas as componentes do sistema)
compilar() {

  vars_export

  echo "Compilar"

  hostname=$(echo ${HOSTNAME})

  if [ ${hostname} != "eslogin01" -a ${hostname} != "eslogin02" ]
  then

    echo "Para compilar, é necessário logar na eslogin01 ou eslogin02"
    exit 3

  else

    # Compilação do inctime
    if [ -d ${util_inctime} ]
    then

      cd ${util_inctime}

      nohup make > make_inctime.log &

      wait

    else

      echo "Diretório ${util_inctime} não existe!"
      exit 1

    fi

    # Compilação do método de perturbação

    # Substitui a linha que começa com a palavra "HOME=" pela valor da variável 
    # HOME=${home_spcon}, no arquivo ${home_spcon}/config/Makefile.conf.pgi
    sed -i "s,^HOME\=.*$,HOME\=${home_spcon},g" ${home_spcon}/config/Makefile.conf.pgi

    cd ${home_spcon}

    nohup make comp=pgi > make_spcon.log &

    wait

    # Compilação do modelo atmosférico

    if [ -d ${home_bam} ]
    then

      # Substitui as linhas que começam com "PATH2=" pelo valor da variável 
      # PATH2=${pre_exec}, nos arquivos ${pre_source}/Makefile e ${pre_source}/Makefile.in
      sed -i "s,^PATH2\=.*$,PATH2\=${pre_exec},g" ${pre_source}/Makefile
      sed -i "s,^PATH2\=.*$,PATH2\=${pre_exec},g" ${pre_source}/Makefile.in
  
      cd ${home_spcon}/bam/pre/sources
      nohup make pgi_cray > make_pre.log &
  
      # Substitui as linhas que começam com "PATH2=" pelo valor da variável 
      # PATH2=${model_exec}, nos arquivos ${model_source}/Makefile e ${model_source}/Makefile.in
      sed -i "s,^PATH2\=.*$,PATH2\=${model_exec},g" ${model_source}/Makefile
      sed -i "s,^PATH2\=.*$,PATH2\=${model_exec},g" ${model_source}/Makefile.in
  
      cd ${home_spcon}/bam/model/source
      nohup make pgi_cray > make_model.log &
  
      # Substitui as linhas que começam com "PATH2=" pelo valor da variável
      # PATH2=${pos_exec} nos arquivos ${pos_source}/Makefile e ${pos_source}/Makefile.in
      sed -i "s,^PATH2\=.*$,PATH2\=${pos_exec},g" ${pos_source}/Makefile
      sed -i "s,^PATH2\=.*$,PATH2\=${pos_exec},g" ${pos_source}/Makefile.in
  
      cd ${home_spcon}/bam/pos/source
      nohup make pgi_cray > make_pos.log &
  
      # Aguarda a finalização dos últimos três comandos nohups
      wait %1 %2 %3

    else

      echo "Diretório ${home_bam} não existe!"
      exit 2

    fi

  fi

}

# Verificação dos argumentos da linha de comando (decide quais funções
# serão chamadas)
if [ ${#} -eq 0 ]
then

  ajuda
  exit 1

else

  if [ ${1} == "testcase" ]
  then
  
    testcase
 
  elif [ ${1} == "configurar" ]
  then

    configurar
 
  elif [ ${1} == "compilar" ]
  then

    compilar
 
  elif [ ${1} == "vars_export" ]
  then

    vars_export
 
  elif [ ${1} == "ajuda" ]
  then

    ajuda
 
  elif [ ${1} == "inctime" ]
  then
  
    if [ ${#} -eq 2 ]
    then
  
      inctime ${2}
  
    elif [ ${#} -eq 1 ] 
    then
  
      inctime

    else

      ajuda
      exit 2
  
    fi
  
  elif [ ${1} == "model" ]
  then
  
    if [ ${#} -eq 2 ]
    then
  
      model ${2}
  
    elif [ ${#} -eq 1 ] 
    then
  
      model

    else

      ajuda
      exit 3
  
    fi
  
  fi

fi
