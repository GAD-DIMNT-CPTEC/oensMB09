#! /bin/ksh

#set -o xtrace

inctime=${HOME}/bin/inctime

datai=2014111712
dataf=2015022812

data=${datai}

while [ ${data} -le ${dataf} ]
do

dataanl=${data}

hh=`echo ${dataanl} | cut -c 9-10`
dd=`echo ${dataanl} | cut -c 7-8`
mm=`echo ${dataanl} | cut -c 5-6`
yy=`echo ${dataanl} | cut -c 1-4`

if [ ${mm} -eq 01 ]; then mes=JAN; fi
if [ ${mm} -eq 02 ]; then mes=FEB; fi
if [ ${mm} -eq 03 ]; then mes=MAR; fi
if [ ${mm} -eq 04 ]; then mes=APR; fi
if [ ${mm} -eq 05 ]; then mes=MAY; fi
if [ ${mm} -eq 06 ]; then mes=JUN; fi
if [ ${mm} -eq 07 ]; then mes=JUL; fi
if [ ${mm} -eq 08 ]; then mes=AUG; fi
if [ ${mm} -eq 09 ]; then mes=SEP; fi
if [ ${mm} -eq 10 ]; then mes=OCT; fi
if [ ${mm} -eq 11 ]; then mes=NOV; fi
if [ ${mm} -eq 12 ]; then mes=DEC; fi

dataform=${hh}Z${dd}${mes}${yy}

cat << EOF > ./CRPS4CPTECEPS.ForecastFor${data}.aave.ctl
DSET ${PWD}/CRPS4CPTECEPS.%f2hForecastFor${data}.aave.grads
UNDEF -9.99e+08
OPTIONS SEQUENTIAL TEMPLATE
XDEF 1 LINEAR 1.0 1.0 
YDEF 1 LINEAR 1.0 1.0 
ZDEF 1 LINEAR 1000 10
TDEF 16 LINEAR 00Z01JAN2015 24HR
VARS 2
CRPSF 1 99 CRPS FOR FORECAST [no dim - the lower the better]
CRPSC 1 99 CRPS FOR CLIMATOLOGICAL DISTRIBUTION [no dim - the lower the better] 
ENDVARS
EOF

cat << EOF > ./CRPS4CPTECEPS.ForecastFor${data}.ctl
DSET ${PWD}/CRPSC4CPTECEPS.%f2hForecastFor${data}.grads
UNDEF -9.99e+08
OPTIONS SEQUENTIAL TEMPLATE
XDEF 240  Linear    0.00 1.5 
YDEF  48  Linear  -90.00 1.5 
ZDEF 1 LINEAR 1000 10
TDEF 16 LINEAR ${dataform} 24HR
VARS 1
CRPSC 1 99 CRPS FOR CLIMATOLOGICAL ABSOLUTE TEMPERATURE AT 850 hPa [ K ] 
ENDVARS
EOF

  data=`${inctime} ${data} +1dy %y4%m2%d2%h2`

done

exit 0
