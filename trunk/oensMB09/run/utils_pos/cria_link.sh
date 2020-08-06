#! /bin/bash

export resol=TQ0126L028
export dateanl=2020062000

export bpath=/scratchin/grupos/ensemble/home/carlos.bastarz/oensMB09/produtos/pos/dataout/${resol}/${dateanl}

arq_link() {
  echo ${mem}
 
  cd ${bpath}/${1} 

  arq_fct_grb=GPOS${1}${dateanl}${dateanl}P.fct.${resol}.grb
  arq_fct_ctl=GPOS${1}${dateanl}${dateanl}P.fct.${resol}.ctl

  arq_icn_grb=GPOS${1}${dateanl}${dateanl}P.icn.${resol}.grb
  arq_icn_ctl=GPOS${1}${dateanl}${dateanl}P.icn.${resol}.ctl

  ln -sv ${arq_icn_grb} ${arq_fct_grb}
  ln -sv ${arq_icn_ctl} ${arq_fct_ctl}
}

for i in $(seq -f %02g 1 8)
do

  if [ ${i} -eq 8 ]
  then

    mem=NMC
   
    arq_link ${mem}

  else

    for j in N P
    do
  
      mem=${i}${j}
  
      arq_link ${mem}
  
    done

  fi

done 

exit 0
