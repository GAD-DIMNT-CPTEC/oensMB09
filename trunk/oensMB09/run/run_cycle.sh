#! /bin/bash
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
#      ./run_cycle.sh <opcao1> <opcao2> <opcao3> <opcao4> <opcao5>
#                      <opcao6> 
#
# !INPUT PARAMETERS:
#  Opcoes..: <opcao1> datai     -> data da primeira análise
#                                
#            <opcao2> dataf     -> data da última análise
#
#            <opcao3> moist_opt -> opção lógica (YES/NO) para
#                                  perturbar ou não a umidade
#            
#            <opcao4> fcth      -> intervalo entre as análises
#
#            <opcao5> num_pert  -> número de perturbações
#
#            <opcao6> run_pos   -> opção lógica (YES/NO) para realizar
#                                  o pós-processamento das previsoes 
#                                  ao final da iteração de cada loop
#
#  Uso/Exemplos: ./run_cycle.sh 
#                (realiza o testcase padrão)
#                ./run_cycle.sh 2013010100 2013010200   
#                (realiza o SPCON para as análises 2013010100, 2013010112 
#                e 2013010200; assume moist_opt=YES, fcth=12, num_pert=7
#                e run_pos=NO)
#                ./run_cycle.sh 2013010100 2013010112 YES
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010112 - inclusive; assume fcth=12, num_pert=7 e 
#                run_pos=NO)
#                ./run_cycle.sh 2013010100 2013010312 YES 12
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010312 - inclusive; assume num_pert=7 e run_pos=NO)
#                ./run_cycle.sh 2013010100 2013010200 NO 6
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010200 - inclusive, com moist_opt=NO e fcth=6; neste
#                caso, serão integradas as análises 2013010100, 2013010106
#                2013010112, 2013010118 e 2013010200; assume num_pert=7 e
#                run_pos=NO)
#                ./run_cycle.sh 2013010100 2013010200 NO 6 10 YES
#                (realiza o SPCON para as análises do intervalo 2013010100
#                e 2013010200 - inclusive, mas utiliza 10 perturbações 
#                e gera um conjunto total de 21 previsões, com o 
#                pós-processamento das previsões ao final de cada loop)
# 
# !REVISION HISTORY:
#
# 14 Agosto de 2017 - C. F. Bastarz - Versão inicial.  
# 15 Agosto de 2017 - C. F. Bastarz - Inclusão comentários.
# 17 Agosto de 2017 - C. F. Bastarz - Inclusão da realização das previsões
#                                     para 15 dias
# 18 Agosto de 2017 - C. F. Bastarz - Inclusão de opção para submeter ou não
#                                     o pós-processamento do modelo atmosférico
# 10 Janeiro de 2018 - C. F. Bastarz - Inclusão de variáveis referentes à
#                                      resolução da análise (e previsões)
#                                      número de processadores utilizados
#                                      pelo pré/model/pós
# 11 Janeiro de 2018 - C. F. Bastarz - Melhorada a verificação dos processos
# 25 Janeiro de 2018 - C. F. Bastarz - Ajustados os prefixos NMC (controle 48h) e CTR (controle 120h)
#
# !REMARKS:
#
# Após esta seção de comentários, as variáveis referentes à resolução da
# análise e número de processadores devem ser alteradas conforme as
# configurações do experimento.
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

# Resolução e escolha do número de processadores dos processos
#
# Resolução  model_nproc pos_nproc Observação
# ============================================
# TQ0126L028          48        48 
# TQ0213L042         192       192
# TQ0299L064         384       384 não testado
model_res=TQ0126L028

bam_trunc_tmp=$(echo ${model_res} | awk -F "TQ" '{print $2}' | awk -F "L" '{print $1}')
bam_lev_tmp=$(echo ${model_res} | awk -F "TQ" '{print $2}' | awk -F "L" '{print $2}')
bam_trunc="${bam_trunc_tmp//[!1-9]/}"
bam_lev="${bam_lev_tmp//[!1-9]/}"

# Utilize o comando "/stornext/home/carlos./bastarz/bin/rsig gdas1.T00Z.SAnl.2013013000"
# e verifique o valor das variáveis "jcap" e "lev"                                                       
#ncep_res=TQ0574L064 # NCEP SAnl (até 2017 - para ler o header dos arquivos de análise espectrais)
ncep_res=TQ1534L064 # NCEP NEMS (a partir de 2017)

anl_trunc_tmp=$(echo ${ncep_res} | awk -F "TQ" '{print $2}' | awk -F "L" '{print $1}')
anl_lev_tmp=$(echo ${ncep_res} | awk -F "TQ" '{print $2}' | awk -F "L" '{print $2}')
anl_trunc="${anl_trunc_tmp//[!1-9]/}"
anl_lev="${anl_lev_tmp//[!1-9]/}"

if [ ${model_res} == "TQ0126L028" ]
then

  model_nproc=48

elif [ ${model_res} == "TQ0213L042" ]
then

  model_nproc=192

elif [ ${model_res} == "TQ0299L064" ]
then

  model_nproc=384

else

  model_nproc=96

fi

pos_nproc=192

source ${PWD}/../config_spcon.sh vars_export

inctime=${util_inctime}/inctime

# Tratamento das opções da linha de comando
if [ ${#} -eq 0 ]
then

  datai=2013010200
  dataf=2013010312
  
  moist_opt=YES

  fcth=12

  num_pert=7

  run_pos=NO

  echo ""

  echo "> Realizando o SPCON Global com o dados do testcase"

  echo ""

  echo "* Data da Primeira Análise...: ${datai}"
  echo "* Data da Última Análise.....: ${dataf}"
  echo "* Intervalo entre as Análises: ${fcth} horas"
  echo "* Perturbação da Umidade.....: ${moist_opt}"
  echo "* Quantidade de Perturbações.: ${num_pert}"
  echo "* Tamanho Total do Conjunto..: $(echo $(($((${num_pert}*2))+1))) membros"
  echo "* Opção Pós-Processamento....: ${run_pos}"
  echo "* Resolução..................: ${model_res}"

else

  if [ -z ${1} ]
  then
    datai=2013010100
  else
    datai=${1}
  fi

  if [ -z ${2} ]
  then
   dataf=2013010112
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

  if [ -z ${6} ]
  then
    run_pos=NO
  else
    run_pos=${6}
  fi

  echo ""

  echo "> Realizando o SPCON Global com os parâmetros a seguir"

  echo ""

  echo "* Data da Primeira Análise...: ${datai}"
  echo "* Data da Última Análise.....: ${dataf}"
  echo "* Intervalo entre as Análises: ${fcth} horas"
  echo "* Perturbação da Umidade.....: ${moist_opt}"
  echo "* Quantidade de Perturbações.: ${num_pert}"
  echo "* Tamanho Total do Conjunto..: $(echo $(($((${num_pert}*2))+1))) membros"
  echo "* Opção Pós-Processamento....: ${run_pos}"
  echo "* Resolução..................: ${model_res}"

  echo ""

fi

# Função específica para verificar o conjunto de análises final
verifica_eof_anls() {

  set -A Perts N P

  for mem in $(seq -f %02g 1 ${1})
  do

    for pert in ${Perts[@]}
    do

      anl_file=${model_datain}/GANL${mem}${pert}${2}S.unf.${3}

      if [ ! -e ${anl_file} ]
      then

        run_deceof ${3} ${4} ${2} ${1}

      fi

    done

  done

}

# A descrição das funções a seguir está dentro do escopo do loop principal

run_deceof() {

  echo "* ANL EOF TOSPEC (${data})"
  nohup ${spcon_run}/run_deceof.sh ${1} EOF ${2} ${3} ${4} > deceof_${3}.log &
  check_status run_deceof
  await

}

run_pre() {

  echo "* PRE CTR (${1})"
  nohup ${spcon_run}/runPre ${bam_trunc} ${bam_lev} ${1} NMC 1 T F ${anl_trunc} ${anl_lev} > pre_${1}.log &
  check_status run_pre
  await

}

run_model_nmc() { # 2 dias, 3h

  echo "* MODEL NMC (${2})"
  nohup ${spcon_run}/run_model.sh ${model_nproc} 4 6 ${1} SMT ${2} ${3} NMC 2 1 > modelNMC_${2}.log &
  check_status run_model_nmc
  await

}

run_model_rdp() { # 2 dias, 3h

  echo "* MODEL RDP (${2})"
  nohup ${spcon_run}/run_model.sh ${model_nproc} 4 6 ${1} SMT ${2} ${3} RDP 2 ${4} > modelRDP_${2}.log &
  check_status run_model_rdp
  await 

}

run_model_ctr() { # 15 dias, 6h

  echo "* MODEL CTR (${2})"
  nohup ${spcon_run}/run_model.sh ${model_nproc} 4 6 ${1} SMT ${2} ${3} CTR 2 1 > modelCTR_${2}.log &
#  check_status run_model_ctr
#  await
#  pid_model_ctr=$!
#  wait ${pid_model_ctr}
#  wait # Neste caso, como o script deve aguardar a submissão das previsões a partir das análises
        # perturbadas por EOF, então esta submissão não precisa aguardar este processo
        # terminar.     

}

run_model_eof() { # 15 dias, 6h

  echo "* MODEL EOF N (${2})"
  nohup ${spcon_run}/run_model.sh ${model_nproc} 4 6 ${1} SMT ${2} ${4} NPT 2 ${3} > modelN_${2}.log &
  check_status run_model_npt
  pid_model_eofn=$!
  echo ${pid_model_eofn}  

  echo "* MODEL EOF P (${2})"
  nohup ${spcon_run}/run_model.sh ${model_nproc} 4 6 ${1} SMT ${2} ${4} PPT 2 ${3} > modelP_${2}.log &
  check_status run_model_ppt
  pid_model_eofp=$!
  echo ${pid_model_eofp}

  wait ${pid_model_ctr} ${pid_model_eofn} ${pid_model_eofp}

}

run_recanl() {

  echo "* ANL TOGRID CTR (${2})"
  nohup ${spcon_run}/run_recanl.sh ${1} SMT ANLSMT ${2} > recanl_${2}.log &
  check_status run_recanl
  await 

}

run_rdpert() {

  echo "* ANL RDP (${3})"
  nohup ${spcon_run}/run_rdpert.sh ${1} SMT ${2} ${3} ${4} > rdpert_${3}.log &
  check_status run_rdpert
  await

}

run_decanl() {

  echo "* ANL TOSPEC (${3})"
  nohup ${spcon_run}/run_decanl.sh ${1} SMT ${2} ${3} ${4} > decanl_${3}.log &
  check_status run_decanl
  await

}

run_recfct_nmc() {

  echo "* MODEL TOGRID NMC (${2})"
  nohup ${spcon_run}/run_recfct.sh ${1} NMC ${2} > recfctNMC_${2}.log &
  check_status run_recfct_nmc
  await 

}

run_recfct_rdp() {

  echo "* MODEL TOSPEC RDP (${3})"
  nohup ${spcon_run}/run_recfct.sh ${1} ${2} ${3} > recfctRPT_${3}.log &
  check_status run_recfct_rdp
  await 

}

run_eof() {

  echo "* ANL EOF (${4})"
  nohup ${spcon_run}/run_eof.sh ${1} ${2} ${3} ${4} > eof_${4}.log &
  check_status run_eof
  await

}

run_pos_ctr() {

  echo "* POS CTR (${2})"
  nohup ${spcon_run}/run_pos.sh ${pos_nproc} 4 6 ${1} ${2} ${3} CTR > posCTR_${2}.log &

}

run_pos_eof() {

  echo "* POS EOF N (${2})"
  nohup ${spcon_run}/run_pos.sh ${pos_nproc} 4 6 ${1} ${2} ${3} NPT ${4} > posN_${2}.log &
  echo "* POS EOF P (${2})"
  nohup ${spcon_run}/run_pos.sh ${pos_nproc} 4 6 ${1} ${2} ${3} PPT ${4} > posP_${2}.log &

}

await() {

  pid_proc=$!
  echo ${pid_proc}
  wait ${pid_proc}

}

check_status() {

  if [ "$(echo $?)" -ne "0" ]
  then

    echo "Processo ${1} falhou, abortando!!"
    kill -9 $$
    kill -9 $!
    exit 1

  fi

}

data=${datai}

while [ ${data} -le ${dataf} ]
do

  # Cálculo das datas das previsões
  data_fct_48h=$(${inctime} ${data} +48hr %y4%m2%d2%h2)
  data_fct_360h=$(${inctime} ${data} +360hr %y4%m2%d2%h2)


  echo ""

  echo "> Realizando o SPCON Global para a data ${data}"

  echo ""

  # 1) Realização do pré-processamento da primeira análise controle
  run_pre ${data} 

  # 2) Realização do membro controle a partir da primeira análise (nesta primeira integração, são apenas 48 horas 3/3h - deverão haver também previsões para até 15 dias a partir do membro controle)
  run_model_nmc ${model_res} ${data} ${data_fct_48h}

  # 3) Recomposição dos coeficientes espectrais da análise para ponto de grade
  run_recanl ${model_res} ${data}
  
  # 4) Gera e soma as perturbações randômicas à análise controle
  run_rdpert ${model_res} ${moist_opt} ${data} ${num_pert} 
 
  # 5) Decomposição das análises perturbadas em ponto de grade para coeficientes espectrais
  run_decanl ${model_res} ${moist_opt} ${data} ${num_pert}

  # 6) Realização das previsões a partir das análises perturbadas para uso na análise de EOF
  run_model_rdp ${model_res} ${data} ${data_fct_48h} ${num_pert} 

  # 7) Recomposição para ponto de grade das previsões realizadas a partir da análise controle
  run_recfct_nmc ${model_res} ${data}

  # 8) Recomposição para ponto de grade das previsões realizadas a partir das análises perturbadas randomicamente
  run_recfct_rdp ${model_res} ${num_pert} ${data}

  # 9) Realização da análise de EOF para gerar as perturbações ótimas a serem utilizadas na composição final dos membros do conjunto
  run_eof ${model_res} ${num_pert} ${moist_opt} ${data}

  # 10) Composição do arquivo de análise a partir das perturbações EOF
  run_deceof ${model_res} ${moist_opt} ${data} ${num_pert}

  verifica_eof_anls ${num_pert} ${data} ${model_res} ${moist_opt}

  # 11) Realização das previsões para até 15 dias a partir da análise controle (sem perturbações)
  run_model_ctr ${model_res} ${data} ${data_fct_360h}

  # 12) Realização das previsões para até 15 dias a partir do conjunto de análises com perturbações ótimas
  run_model_eof ${model_res} ${data} ${num_pert} ${data_fct_360h}

  if [ ${run_pos} == YES ]
  then

    # 13) Realização do pós-processamento das previsões de até 15 dias realizadas a partir da análise controle
    run_pos_ctr ${model_res} ${data} ${data_fct_360h}

    # 14) Realização do pós-processamento do conjunto de previsões de até 15 dias realizado a partir do conjunto de análises com perturbações ótimas
    run_pos_eof ${model_res} ${data} ${data_fct_360h} ${num_pert}

  fi

  data=$(${inctime} ${data} +${fcth}hr %y4%m2%d2%h2)

  echo ""

done

exit 0
