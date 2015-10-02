#!/bin/bash 
export DATE=$1
export HH=${DATE:8:2}
export NDAYS_VIES=$2
export NDAYS_FCT=$3
export DIRIN=/stornext/online7/pnt/preoper/tempo/oensMB09
export DIRVIES=/scratchout/oper/io/oensMB09/vies_oper/
export DIRTMP=/scratchout/oper/io/oensMB09/rvies/tmp ; mkdir ${DIRTMP}

# FUNCAO QUE GERA AS LISTAS PARA CADA HORARIO DE MODELO 
function gera_lista {
   di=$1
   icn=$2
   d=$icn #modelo 00Z
   if [ ${icn:8:2} -eq 00 -o ${di:8:2} -ge ${icn:8:2} ]; then
      fct=$(echo ${di:8:2}-${icn:8:2} | bc)
   else
      fct=$(echo 24+${di:8:2}-${icn:8:2} | bc)
   fi
   if [ $fct -eq 0 ]; then
      fct=24
   fi
   i=0
   while [ $i -lt ${NDAYS_FCT} ]; do
      f=$(echo $fct+24*$i | bc)
      if [ $f -lt 10 ]; then
         f=0$f
      fi
      if [ $f -lt 100 ]; then
         f=0$f
      fi
      d=$(date -u -d "${d:0:8} ${d:8:2}:00 24 hours ago" +"%Y%m%d%H")
      fct_file="${DIRIN}/${d:0:6}/${d:6:4}/NMC/GPOSNMC${d}${di}P.fct.TQ0126L028.grb"
         if [ $(ls -ltr --full-time ${fct_file} | awk '{print $5}') -ge 19034853 ]; then
            echo -e "${d:8:2} ${f} ${fct_file}" >> ${f_out}.txt
         else 
            echo -e "${d:8:2} ${f} DUMMIE" >> ${f_out}.txt
         fi
   let i=i+1
   done
}

df=$(date -u -d "${DATE:0:8} ${HH}:00  ${NDAYS_VIES} days ago" +"%Y%m%d%H")
di=${DATE}

rm -f ${DIRTMP}/*
while [ $di -ge $df ]; do
   echo $di
   icn_file="${DIRIN}/${di:0:6}/${di:6:4}/NMC/GPOSNMC${di}${di}P.icn.TQ0126L028.grb"
   if [ $(ls -ltr --full-time ${icn_file} | awk '{print $5}') -ge 15198371 ]; then
      f_out=${DIRTMP}/files_${di}
      echo -e "00 000 ${icn_file}" >  ${f_out}.txt
      for h_icn in 00 06 12 18; do
         icn=$(date -u -d "${di:0:8} ${di:8:2}:00 ${h_icn} hours " +"%Y%m%d%H")
         gera_lista $di $icn
      done
   fi
   di=$(date -u -d "${di:0:8} ${di:8:2}:00  6 hours ago" +"%Y%m%d%H")
done

a=${DIRTMP}/lista_${RAMDON}.txt
ls -l --full-time ${DIRTMP}/*.txt | awk '{print $9}' > ${a}

exit 0
