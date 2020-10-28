#! /bin/bash

export inctime=${HOME}/bin/inctime

export datai=2020051500
export dataf=2020083100

export resol=TQ0126L028

exp_links() {
  cd ${bpath}/ 

  ln -sv ${mem}/*fct*ctl .
  ln -sv ${mem}/*fct*idx .
  ln -sv ${mem}/*fct*grb .

  ln -sv ${mem}/*icn*ctl .
  ln -sv ${mem}/*icn*idx .
  ln -sv ${mem}/*icn*grb .

  ln -sv ${mem}/*inz*ctl .
  ln -sv ${mem}/*inz*idx .
  ln -sv ${mem}/*inz*grb .
}

data=${datai}

while [ ${data} -le ${dataf} ]
do

  export dateanl=${data}

  export bpath=/lustre_xc50/carlos_bastarz/oensMB09.svn/pos/dataout/${resol}/${dateanl}

  for i in $(seq -f %02g 1 8)
  do
  
    if [ ${i} -eq 8 ]
    then
  
      mem=NMC
     
      exp_links ${mem}
  
    else
  
      for j in N P
      do
    
        mem=${i}${j}
    
        exp_links ${mem}
    
      done
  
    fi
  
  done 

  data=$(${inctime} ${data} +1dy %y4%m2%d2%h2)

done

exit 0
