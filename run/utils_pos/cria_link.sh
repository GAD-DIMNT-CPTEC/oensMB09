#! /bin/bash -x

#if [ $# -eq 0 ]
#then
#  exit 1
#fi

export inctime=${HOME}/bin/inctime

export datai=2020051500
export dataf=2020083100

export resol=TQ0126L028

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

export data=${datai}

while [ ${data} -le ${dataf} ]
do

  export dateanl=${data}

  export bpath=/lustre_xc50/carlos_bastarz/oensMB09.svn/pos/dataout/${resol}/${dateanl}


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

  data=$(${inctime} ${data} +1dy %y4%m2%d2%h2)

done

exit 0
