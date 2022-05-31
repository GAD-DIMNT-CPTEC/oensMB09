#! /bin/bash 

inctime=/cray_home/carlos_bastarz/bin/inctime

datai=2020051500
dataf=2020083100

data=${datai}

while [ ${data} -le ${dataf} ] 
do

  for i in $(seq -f %02g 1 7)
  do

    for pert in N P
    do

      mem=${i}${pert}

      nctl=$(find ./${data}/${mem} -name "*.ctl" | wc -l)
      nidx=$(find ./${data}/${mem} -name "*.idx" | wc -l)
      ngrb=$(find ./${data}/${mem} -name "*.grb" | wc -l)

      if [ ${nctl} -ne 62 -o ${nidx} -ne 62 -o ${ngrb} -ne 62 ]
      then

        echo ${data} ${mem} ${nctl} ${nidx} ${ngrb}

      fi

    done

  done

  data=$(${inctime} ${data} +1dy %y4%m2%d2%h2)

done

exit 0
