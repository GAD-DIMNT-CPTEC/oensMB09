#! /bin/bash

export inctime=${HOME}/bin/inctime

export bpath=/cray_home/carlos_bastarz/oensMB09.svn/run

#Procs=(grh ensmed spread cluster probability probagr plumes chievol perturbations spaguetti)
Procs=(spread)

export datai=2020070100
export dataf=2020083100

export data=${datai}

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
