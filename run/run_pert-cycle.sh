#! /bin/bash 
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para submeter os processos do método de perturbações MB09 do 
# Sistema de Previsão por Conjunto Global (SPCON) do CPTEC, para uma
# data ou um período.
#
# !INTERFACE:
#      ./run_pert-cycle.sh
#
# !INPUT PARAMETERS:
#            
# !REVISION HISTORY:
#
# 10 Setembro de 2020 - C. F. Bastarz - Versão inicial.  
# 18 Junho de 2021    - C. F. Bastarz - Revisão geral.
# 01 Novembro de 2022 - C. F. Bastarz - Atualizado para uso no Egeon.
#
# !REMARKS:
# Antes de utilizar este script, revise as variáveis a seguir:
# - anltype: prefixo da análise (PPP)
# - datai: data inicial (YYYYMMDDHH)
# - dataf: data final (YYYYMMDDHH)
# - Procs: nomes os produtos a serem gerados
#
# Se as variáveis datai e dataf forem iguais, o script será executado
# apenas para a data escolhida.
#
# !BUGS:
#
#EOP  
#--------------------------------------------------------------------#
#BOC

# Descomentar para debugar
#set -o xtrace

export inctime=${HOME}/bin/inctime

export bpath=/mnt/beegfs/carlos.bastarz/oensMB09/run

#
# Prefixo da análise
#
# SMT: análise com suavização da topografia (padrão, depende de como o Chopping_parallel doi executado)
# NMC: análise sem suavização da topografia
#

export anltype=SMT

#
# Nomes dos processos
#
# recanl..: recomposição da análise espectral para ponto de grade
# rdpert..: gera perturbações randômicas
# decanl..: decomposição das análises em ponto de grade para o espaço espectral
# model2d.: executa o modelo BAM para previsões de 48 horas, com saídas a cada 3 horas
# recfct..: recomposição das previsões espcetrais para ponto de grade
# eof.....: cálculo das EOFs sobre as séries de diferenças entre as análises e previsõe controle e perturbadas randomicamente
# deceof..: decomposição das análise de prefixo N e P em ponto de grade para o espaço espectral 
# model15d: executa o modelo BAM para as previsões de 360 horas, com saídas a cada 6 horas
# pos15d..: executa o pós-processamento das previsões do modelo BAM
# gribamp.: executa o gribmap das previsões pós-processadas do modelo BAM
#

# Acrescentar ou remover os processos conforme a necessidade
Procs=(pre recanl rdpert decanl model2d recfct eof deceof model15d pos15d gribmap)

#
# Número de perturbações
#

export npert=7

#
# Datas de início e fim
#

export datai=2020120100
export dataf=2020121500

export data=${datai}

while [ ${data} -le ${dataf} ]
do
  
  datafct48h=$(${inctime} ${data} +48hr %y4%m2%d2%h2)
  datafct15d=$(${inctime} ${data} +15d %y4%m2%d2%h2)

  for proc in ${Procs[@]}
  do

    echo ${data} ${datafct48h} ${datafct15d} ${proc}

    if [ ${proc} == "pre" ]; then . runPre 126 28 ${data} ${anltype} 1 T F 574 64; wait; fi 

    if [ ${proc} == "recanl" ]; then . ${bpath}/run_recanl.sh TQ0126L028 ${anltype} ANL${anltype} ${data}; wait; fi
    if [ ${proc} == "rdpert" ]; then . ${bpath}/run_rdpert.sh TQ0126L028 ${anltype} YES ${data} ${npert}; wait; fi
    if [ ${proc} == "decanl" ]; then . ${bpath}/run_decanl.sh TQ0126L028 ${anltype} YES ${data} ${npert}; wait; fi

    if [ ${proc} == "model2d" ]
    then 
      . ${bpath}/run_model.sh 128 4 1 TQ0126L028 ${anltype} ${data} ${datafct48h} CTR 2 1 &
      . ${bpath}/run_model.sh 128 4 1 TQ0126L028 ${anltype} ${data} ${datafct48h} RDP 2 ${npert}
    fi
    wait

    if [ ${proc} == "recfct" ]
    then 
      . ${bpath}/run_recfct.sh TQ0126L028 CTR ${data} &
      . ${bpath}/run_recfct.sh TQ0126L028 ${npert} ${data}
    fi
    wait

    if [ ${proc} == "eof" ]; then . ${bpath}/run_eof.sh TQ0126L028 ${npert} YES ${data} ${anltype}; wait; fi
    if [ ${proc} == "deceof" ]; then . ${bpath}/run_deceof.sh TQ0126L028 EOF YES ${data} ${npert} ${anltype}; wait; fi

    if [ ${proc} == "model15d" ]
    then
      . ${bpath}/run_model.sh 128 4 1 TQ0126L028 ${anltype} ${data} ${datafct15d} NMC 2 1 &
      . ${bpath}/run_model.sh 128 4 1 TQ0126L028 ${anltype} ${data} ${datafct15d} NPT 2 ${npert} &
      . ${bpath}/run_model.sh 128 4 1 TQ0126L028 ${anltype} ${data} ${datafct15d} PPT 2 ${npert} 
    fi
    wait

    if [ ${proc} == "pos15d" ]
    then
    # Para o pós-processamento na Egeon, 64 cores estão sendo utilizados porque
    # com 128, ocorre um erro relacionado com FFTs
      . ${bpath}/run_pos.sh 64 4 1 TQ0126L028 ${data} ${datafct15d} NMC &
      . ${bpath}/run_pos.sh 64 4 1 TQ0126L028 ${data} ${datafct15d} NPT ${npert} &
      . ${bpath}/run_pos.sh 64 4 1 TQ0126L028 ${data} ${datafct15d} PPT ${npert} 
    fi
    wait

    if [ ${proc} == "gribmap" ]
    then
      . ${bpath}/run_gribmap.sh TQ0126L028 ${data} 1 NMC &
      . ${bpath}/run_gribmap.sh TQ0126L028 ${data} ${npert} NPT &
      . ${bpath}/run_gribmap.sh TQ0126L028 ${data} ${npert} PPT 
    fi
    wait

  done

  data=$(${inctime} ${data} +1d %y4%m2%d2%h2)

done

exit 0
