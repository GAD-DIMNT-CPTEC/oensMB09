#! /bin/bash

bpath=/lustre_xc50/carlos_bastarz/prevs_24_48_hr_bam_sigma_TQ0126L028/recfct/dataout/TQ0126L028

inctime=${HOME}/bin/inctime

datai=2020010100
dataf=2020123000

data=${datai}

while [ ${data} -le ${dataf} ]
do

  echo ${data}

  ofile=GFCTCTR${data}${datafct24hr}R.fct.TQ0126L028

  datafct24hr=$(${inctime} ${data} +24hr %y4%m2%d2%h2)
  datafct48hr=$(${inctime} ${data} +48hr %y4%m2%d2%h2)

  file24hr=${bpath}/${data}/GFCTCTR${data}${datafct24hr}R.fct.TQ0126L028
  file48hr=${bpath}/${data}/GFCTCTR${data}${datafct48hr}R.fct.TQ0126L028

  mkdir -p ${bpath}/24hr
  mkdir -p ${bpath}/48hr

  cd ${bpath}/24hr
  ln -s ${file24hr} GFCTCTR${datafct24hr}R.fct.TQ0126L028

  cd ${bpath}/48hr
  ln -s ${file48hr} GFCTCTR${datafct48hr}R.fct.TQ0126L028

  data=$(${inctime} ${data} +7dy %y4%m2%d2%h2)
 
done

exit 0
