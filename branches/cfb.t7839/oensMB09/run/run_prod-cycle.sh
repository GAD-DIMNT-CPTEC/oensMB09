#! /bin/bash
#--------------------------------------------------------------------#
#  Sistema de Previsão por Conjunto Global - GDAD/CPTEC/INPE - 2021  #
#--------------------------------------------------------------------#
#BOP
#
# !DESCRIPTION:
# Script para submeter os produtos do Sistema de Previsão por Conjunto 
# Global (SPCON) do CPTEC, para uma data ou um período.
#
# !INTERFACE:
#      ./run_prod-cycle.sh
#
# !INPUT PARAMETERS:
#            
# !REVISION HISTORY:
#
# 10 Setembro de 2020 - C. F. Bastarz - Versão inicial.  
# 18 Junho de 2021    - C. F. Bastarz - Revisão geral.
#
# !REMARKS:
# Antes de utilizar este script, revise as variáveis a seguir:
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

export inctime=${HOME}/bin/inctime

export bpath=/cray_home/carlos_bastarz/oensMB09.svn/run

# 
# Nomes dos produtos
#
# grh..........: grid history com as plumas de probabilidade (produz figuras)
# ensmed.......: média dos conjuntos de análises e previsões (não produz figuras)
# spread.......: espalhamento dos conjuntos de análises e previsões (produz figuras)
# cluster......: análise de agrupamento dos conjuntos de análises e previsões (produz figuras)
# probability..: probabilidades de precipitação dos conjuntos de previsões (produz figuras)
# probagr......: probabilidade de precipitação acumulada dos conjuntos de previsões (produz figuras)
# chievol......: potencial de velocidade em altos níveis (produz figuras)
# perturbations: perturbações iniciais dos conjuntos de análises (produz figuras)
# spaguetti....: espagueti dos conjuntos de análises e previsões (produz figuras)
#

# Acrescentar ou remover os produtos conforme a necessidade
Procs=(grh ensmed spread cluster probability probagr plumes chievol perturbations spaguetti)

#
# Datas de início e fim
#

export datai=2020070100
export dataf=2020083100

export data=${datai}

#
# Loop entre as datas
#

while [ ${data} -le ${dataf} ]
do
  
  datafct48h=$(${inctime} ${data} +48hr %y4%m2%d2%h2)
  datafct15d=$(${inctime} ${data} +15dy %y4%m2%d2%h2)

  for proc in ${Procs[@]}
  do

    echo ${data} ${datafct48h} ${datafct15d} ${proc}

    if [ ${proc} == "ensmed" ]; then ${bpath}/run_ensmed.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    if [ ${proc} == "spread" ]; then ${bpath}/run_spread.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    
    if [ ${proc} == "grh" ]
    then
      ${bpath}/run_grh.sh 4 TQ0126L028 ${data} ${datafct15d} NMC &
      ${bpath}/run_grh.sh 4 TQ0126L028 ${data} ${datafct15d} NPT 7 &
      ${bpath}/run_grh.sh 4 TQ0126L028 ${data} ${datafct15d} PPT 7 
    fi
    wait

    if [ ${proc} == "cluster" ]; then ${bpath}/run_cluster.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    if [ ${proc} == "probability" ]; then ${bpath}/run_probability.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    if [ ${proc} == "probagr" ]; then ${bpath}/run_probagr.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    if [ ${proc} == "plumes" ]; then ${bpath}/run_plumes.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    if [ ${proc} == "chievol" ]; then ${bpath}/run_chievol.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    if [ ${proc} == "perturbations" ]; then ${bpath}/run_perturbations.sh TQ0126L028 ${data} 15 NMC 7; wait; fi
    if [ ${proc} == "spaguetti" ]; then ${bpath}/run_spaguetti.sh TQ0126L028 ${data} 15 NMC 7; wait; fi

  done

  data=$(${inctime} ${data} +1dy %y4%m2%d2%h2)

done

exit 0
