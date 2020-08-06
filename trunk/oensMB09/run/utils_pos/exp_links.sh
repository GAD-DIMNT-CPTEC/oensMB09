#! /bin/bash

export resol=TQ0126L028
export dateanl=2020062000

export bpath=/scratchin/grupos/ensemble/home/carlos.bastarz/oensMB09/produtos/pos/dataout/${resol}/${dateanl}

exp_links() {
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

exit 0
