#! /bin/bash

export inctime=${HOME}/bin/inctime

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

datai=2020051500
dataf=2020083100

data=${datai}

while [ ${data} -le ${dataf} ]
do

  export dateanl=${data}

  export bpath=/lustre_xc50/carlos_bastarz/oensMB09.svn/pos/dataout/${resol}/${dateanl}


  mem=NMC
  
  arq_link ${mem}

  data=$(${inctime} ${data} +1dy %y4%m2%d2%h2)

done

exit 0
