#! /bin/bash

if [ $# -eq 0]
then
  exit 1
fi

export resol=TQ0126L028
export dateanl=${1}

export bpath=/lustre_xc50/carlos_bastarz/oensMB09_test_preXC50/pos/dataout/${resol}/${dateanl}

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
