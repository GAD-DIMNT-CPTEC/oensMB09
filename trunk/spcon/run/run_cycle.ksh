#! /bin/ksh
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2017  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para a realização cíclica do Sistema de Previsão por Conjunto 
# Global (SPCON) do CPTEC.
#
# !INTERFACE:
#      ./run_cycle.ksh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5> 
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> YYYYMMDDHH -> data da primeira análise
#                                
#            <opcao2> YYYYMMDDHH -> data da última análise
#
#            <opcao3> moist_opt  -> opção lógica (YES/NO) para
#                                   perturbar ou não a umidade
#            
#            <opcao4> fcth       -> intervalo entre as análises
#
#            <opcao5> num_pert   -> número de perturbações
#
#  Uso/Exemplos: ./run_cycle.ksh 
#                (realiza o testcase padrão)
#                ./run_cycle.ksh 2013010100 2013010200   
#                (realiza o SPCON para as análises 2013010100, 2013010112 
#                e 2013010200; assume moist_opt=YES, fcth=12 e num_pert=7)
#                ./run_cycle.ksh 2013010100 2013010112 YES
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010112 - inclusive; assume fcth=12 e num_pert=7)
#                ./run_cycle.ksh 2013010100 2013010312 YES 12
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010312 - inclusive; assume num_pert=7)
#                ./run_cycle.ksh 2013010100 2013010200 NO 6
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010200 - inclusive, com moist_opt=NO e fcth=6; neste
#                caso, serão integradas as análises 2013010100, 2013010106
#                2013010112, 2013010118 e 2013010200; assume num_pert=7)
#                ./run_cycle.ksh 2013010100 2013010200 NO 6 10
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010200 - inclusive, mas utiliza 10 perturbações 
#                e gera um conjunto total de 21 previsões)
#
# 
# !REVISION HISTORY:
#
# 14 Agosto de 2017 - C. F. Bastarz - Versão inicial.  
# 15 Agosto de 2017 - C. F. Bastarz - Inclusão comentários.
#
# !REMARKS:
#
# !BUGS:
#
# O processo deceof apresenta alguns problemas na escrita do conjunto
# de análises perturbadas por EOF. Para remediar isto, foi incluída
# uma função para verificar se todas as análises foram geradas, antes
# de se integrar o modelo atmosférico. Caso alguma análise não tenha 
# sido criada, o processo é repetido até que todas as análises estejam
# presentes no model/datain.
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
#set -o xtrace

inctime=${HOME}/bin/inctime # incluir compilação do inctime na compilação do SPCON

oens_name=oensMB09_bam

run_scripts=${SUBMIT_HOME}/${oens_name}/run

model_res=TQ0126L028

# Tratamento das opções da linha de comando
if [ ${#} -eq 0 ]
then

  datai=2013010200
  dataf=2013010212
  
  moist_opt=YES

  fcth=12

  num_pert=7

  echo ""

  echo "> Realizando o SPCON Global com o dados do testcase"

  echo ""

  echo "* Data da Primeira Análise...: ${datai}"
  echo "* Data da Última Análise.....: ${dataf}"
  echo "* Intervalo entre as Análises: ${fcth} horas"
  echo "* Perturbação da Umidade.....: ${moist_opt}"
  echo "* Quantidade de Perturbações.: ${num_pert}"
  echo "* Tamanho Total do Conjunto..: $(echo $(($((${num_pert}*2))+1))) membros"

  echo ""

else

  if [ -z ${1} ]
  then
    datai=2013010200
  else
    datai=${1}
  fi

  if [ -z ${2} ]
  then
   dataf=2013010212
  else
   dataf=${2}
  fi
  
  if [ -z ${3} ]
  then
    moist_opt=YES
  else
    moist_opt=${3}
  fi

  if [ -z ${4} ]
  then
    fcth=12
  else
   fcth=${4}
  fi

  if [ -z ${5} ]
  then
    num_pert=7
  else
    num_pert=${5}
  fi

  echo ""

  echo "> Realizando o SPCON Global com o dados do testcase"

  echo ""

  echo "* Data da Primeira Análise...: ${datai}"
  echo "* Data da Última Análise.....: ${dataf}"
  echo "* Intervalo entre as Análises: ${fcth} horas"
  echo "* Perturbação da Umidade.....: ${moist_opt}"
  echo "* Quantidade de Perturbações.: ${num_pert}"
  echo "* Tamanho Total do Conjunto..: $(echo $(($((${num_pert}*2))+1))) membros"

  echo ""

fi

# Função específica para verificar o conjunto de análises final
verifica_eof_anls() {

  set -A Perts N P

  for mem in $(seq -f %02g 1 ${1})
  do

    for pert in ${Perts[@]}
    do

      anl_file=${SUBMIT_HOME}/oensMB09_bam/bam/model/datain/GANL${mem}${pert}${2}S.unf.${3}

      if [ ! -e ${anl_file} ]
      then

        run_deceof

      fi

    done

  done

}

# A descrição das funções a seguir está dentro do escopo do loop principal

run_deceof() {

  echo "* ANL EOF TOSPEC (${data})"
  nohup ${run_scripts}/run_deceof.ksh ${1} EOF ${2} ${3} ${4} > deceof_${3}.log &
  wait

}

run_pre() {

  echo "* PRE CTR (${1})"
  nohup ${run_scripts}/runPre 126 28 ${1} NMC 1 T F 574 64 > pre_${1}.log &
  wait 

}

run_model_ctr() {

  echo "* MODEL CTR (${2})"
  nohup ${run_scripts}/run_model.ksh 48 24 1 ${1} SMT ${2} CTR 2 > modelCTR_${2}.log &
  wait 

}

run_recanl() {

  echo "* ANL TOGRID CTR (${2})"
  nohup ${run_scripts}/run_recanl.ksh ${1} SMT ANLSMT ${2} > recanl_${2}.log &
  wait 

}

run_rdpert() {

  echo "* ANL RDPERT (${3})"
  nohup ${run_scripts}/run_rdpert.ksh ${1} SMT ${2} ${3} ${4} > rdpert_${3}.log &
  wait

}

run_decanl() {

  echo "* ANL TOSPEC (${3})"
  nohup ${run_scripts}/run_decanl.ksh ${1} SMT ${2} ${3} ${4} > decanl_${3}.log &
  wait

}

run_model_rdpert() {

  echo "* MODEL RDPERT (${2})"
  nohup ${run_scripts}/run_model.ksh 48 24 1 ${1} SMT ${2} ${3} 2 R > modelRDPERT_${2}.log &
  wait

}

run_recfct_ctr() {

  echo "* MODEL TOGRID CTRL (${2})"
  nohup ${run_scripts}/run_recfct.ksh ${1} CTR ${2} > recfctCTR_${2}.log &
  wait

}

run_recfct_rdpert() {

  echo "* MODEL TOSPEC RDPERT (${3})"
  nohup ${run_scripts}/run_recfct.ksh ${1} ${2} ${3} > recfctRPT_${3}.log &
  
  wait

}

run_eof() {

  echo "* ANL EOF (${4})"
  nohup ${run_scripts}/run_eof.ksh ${1} ${2} ${3} ${4} > eof_${4}.log &
  wait

}

run_model_eof() {

  echo "* MODEL EOF N (${2})"
  nohup ${run_scripts}/run_model.ksh 48 24 1 ${1} SMT ${2} ${3} 2 N > modelN_${2}.log &
  echo "* MODEL EOF P (${2})"
  nohup ${run_scripts}/run_model.ksh 48 24 1 ${1} SMT ${2} ${3} 2 P > modelP_${2}.log &
  wait %1 %2

}

run_pos_eof() {

  echo "* POS EOF N (${2})"
  nohup ${run_scripts}/run_pos.ksh 48 24 1 ${1} ${2} ${3} N > posN_${2}.log &
  echo "* POS EOF P (${2})"
  nohup ${run_scripts}/run_pos.ksh 48 24 1 ${1} ${2} ${3} P > posP_${2}.log &
  wait %1 %2

}

data=${datai}

while [ ${data} -le ${dataf} ]
do

  echo ""

  echo "> Realizando o SPCON Global para a data ${data}"

  echo ""

  # 1) Realização do pré-processamento da primeira análise controle
  run_pre ${data}
  
  # 2) Realização do membro controle a partir da primeira análise (nesta primeira integração, são apenas 48 horas 3/3h - deverão haver também previsões para até 15 dias a partir do membro controle)
  run_model_ctr ${model_res} ${data}
  
  # 3) Recomposição dos coeficientes espectrais da análise para ponto de grade
  #run_recanl.ksh ${model_res} SMT ANLNMC ${data}
  run_recanl ${model_res} ${data}
  
  # 4) Gera e soma as perturbações randômicas à análise controle
  run_rdpert ${model_res} ${moist_opt} ${data} ${num_pert} 
 
  # 5) Decomposição das análises perturbadas em ponto de grade para coeficientes espectrais
  run_decanl ${model_res} ${moist_opt} ${data} ${num_pert}

  # 6) Realização das previsões a partir das análises perturbadas para uso na análise de EOF
  run_model_rdpert ${model_res} ${data} ${num_pert}

  # 7) Recomposição para ponto de grade das previsões realizadas a partir da análise controle
  run_recfct_ctr ${model_res} ${data}

  # 8) Recomposição para ponto de grade das previsões realizadas a partir das análises perturbadas randomicamente
  run_recfct_rdpert ${model_res} ${num_pert} ${data}

  # 9) Realização da análise de EOF para gerar as perturbações ótimas a serem utilizadas na composição final dos membros do conjunto
  run_eof ${model_res} ${num_pert} ${moist_opt} ${data}

  # 10) Composição do arquivo de análise a partir das perturbações EOF
  run_deceof ${model_res} ${moist_opt} ${data} ${num_pert}

  verifica_eof_anls ${num_pert} ${data} ${model_res}

  # 11) Realização das previsões a partir do conjunto de análises com perturbações ótimas
  run_model_eof ${model_res} ${data} ${num_pert}  

  # 12) Realização do pós-processamento do conjunto de previsões realizado a partir do conjunto de análises com perturbações ótimas
  run_pos_eof ${model_res} ${data} ${num_pert}

  data=$(${inctime} ${data} +${fcth}hr %y4%m2%d2%h2)

  echo ""

done

exit 0
