#!/bin/bash
DATE=$1
HH=${DATE:8:2}
DIRIN=/stornext/online7/pnt/preoper/tempo/oensMB09
DIRVIES=/scratchout/oper/io/oensMB09/vies_oper/

rm -f incluir_* excluir_* viesfiles_* viesfilesout_*

# LISTAGEM DE ARQUIVOS A INCLUIR
a=$DATE
d=$DATE
f=incluir_$a
echo ${DIRIN}/${d:0:6}/${d:6:4}/NMC/GPOSNMC${d}${DATE}P.icn.TQ0126L028.grb > $f
for fct in $(seq -w 00 6 359); do
   d=$(date -u -d "${d:0:8} ${d:8:2}:00  6 hours ago" +"%Y%m%d%H")
   arq=${DIRIN}/${d:0:6}/${d:6:4}/NMC/GPOSNMC${d}${DATE}P.fct.TQ0126L028.grb
   echo $arq >> $f
done

DATEU=$(date -u -d "${DATE:0:8} ${DATE:8:2}:00 60 days ago" +"%Y%m%d%H")

# LISTAGEM DE ARQUIVOS A EXCLUIR

d=$DATEU
f=excluir_$a
echo ${DIRIN}/${d:0:6}/${d:6:4}/NMC/GPOSNMC${d}${DATEU}P.icn.TQ0126L028.grb > $f
for fct in $(seq -w 00 6 359); do
   d=$(date -u -d "${d:0:8} ${d:8:2}:00  6 hours ago" +"%Y%m%d%H")
   arq=${DIRIN}/${d:0:6}/${d:6:4}/NMC/GPOSNMC${d}${DATEU}P.fct.TQ0126L028.grb
   echo $arq >> $f
done

#LISTAGEM DE ARQUIVOS DE VIES

#f=viesfiles_$a
#DATEU=$(date -u -d "${DATE:0:8} 1 days ago" +"%Y%m%d")
#for fct in $(seq  06 06 360); do
#   echo "${DIRVIES}/${DATEU}${HH}/vieserromedio.60Forecasts.${fct}hLag.${DATEU}${HH}.grads" >> $f
#done

f=viesfiles_$a
DATEU=$(date -u -d "${DATE:0:8} 1 days ago" +"%Y%m%d")
for fct in $(seq  06 06 360); do
   echo "${DIRVIES}/${DATEU}${HH}/vieserromedio.60Forecasts.${fct}hLag.${DATEU}${HH}.grads" >> $f
done


f=viesfilesout_$a
DATEU=${DATE:0:8}
DATECTL=$(date -u -d "${DATEU:0:8} ${HH}:00  0 hours ago" +"%HZ%d%b%Y")

mkdir -p ${DIRVIES}/teste/${DATEU}${HH}/

for fct in $(seq  06 06 360); do
   echo "${DIRVIES}/teste/${DATEU}${HH}/vies.60Forecasts.${fct}hLag.${DATEU}${HH}.grads" >> $f
cat<<EOF>${DIRVIES}/teste/${DATEU}${HH}/vies.60Forecasts.${fct}hLag.${DATEU}${HH}.ctl
DSET ^vies.60Forecasts.${fct}hLag.${DATEU}${HH}.grads
TITLE vies para ${DATEU}${HH} em ${fct} horas
OPTIONS YREV
UNDEF 9.99E+33
XDEF 384 linear    0.000   0.9375000000
YDEF 192 levels
 -89.28423 -88.35700 -87.42430 -86.49037 -85.55596 -84.62133 -83.68657 -82.75173
 -81.81684 -80.88191 -79.94696 -79.01199 -78.07701 -77.14201 -76.20701 -75.27199
 -74.33697 -73.40195 -72.46692 -71.53189 -70.59685 -69.66182 -68.72678 -67.79173
 -66.85669 -65.92165 -64.98660 -64.05155 -63.11650 -62.18145 -61.24640 -60.31135
 -59.37630 -58.44124 -57.50619 -56.57114 -55.63608 -54.70103 -53.76597 -52.83091
 -51.89586 -50.96080 -50.02574 -49.09069 -48.15563 -47.22057 -46.28551 -45.35045
 -44.41540 -43.48034 -42.54528 -41.61022 -40.67516 -39.74010 -38.80504 -37.86998
 -36.93492 -35.99986 -35.06480 -34.12974 -33.19468 -32.25962 -31.32456 -30.38950
 -29.45444 -28.51938 -27.58431 -26.64925 -25.71419 -24.77913 -23.84407 -22.90901
 -21.97395 -21.03889 -20.10383 -19.16876 -18.23370 -17.29864 -16.36358 -15.42852
 -14.49346 -13.55839 -12.62333 -11.68827 -10.75321  -9.81815  -8.88309  -7.94802
  -7.01296  -6.07790  -5.14284  -4.20778  -3.27272  -2.33765  -1.40259  -0.46753
   0.46753   1.40259   2.33765   3.27272   4.20778   5.14284   6.07790   7.01296
   7.94802   8.88309   9.81815  10.75321  11.68827  12.62333  13.55839  14.49346
  15.42852  16.36358  17.29864  18.23370  19.16876  20.10383  21.03889  21.97395
  22.90901  23.84407  24.77913  25.71419  26.64925  27.58431  28.51938  29.45444
  30.38950  31.32456  32.25962  33.19468  34.12974  35.06480  35.99986  36.93492
  37.86998  38.80504  39.74010  40.67516  41.61022  42.54528  43.48034  44.41540
  45.35045  46.28551  47.22057  48.15563  49.09069  50.02574  50.96080  51.89586
  52.83091  53.76597  54.70103  55.63608  56.57114  57.50619  58.44124  59.37630
  60.31135  61.24640  62.18145  63.11650  64.05155  64.98660  65.92165  66.85669
  67.79173  68.72678  69.66182  70.59685  71.53189  72.46692  73.40195  74.33697
  75.27199  76.20701  77.14201  78.07701  79.01199  79.94696  80.88191  81.81684
  82.75173  83.68657  84.62133  85.55596  86.49037  87.42430  88.35700  89.28423
ZDEF 9 levels 1000 925 850 700 500 300 250 200 50
TDEF 1 LINEAR ${DATECTL} 12hr
VARS 10
UMESV 9 99 SPECIFIC HUMIDITY BIAS                      (KG/KG)
UVELV 9 99 ZONAL WIND (U) BIAS                         (M/S)
VVELV 9 99 MERIDIONAL WIND (V) BIAS                    (M/S)
ZGEOV 9 99 GEOPOTENTIAL HEIGHT BIAS                    (GPM)
TEMPV 9 99 ABSOLUTE TEMPERATURE BIAS                   (K)
U10MV 1 99 10 METRE U-WIND COMPONENT BIAS              (M/S)
TP2MV 1 99 2m TEMPERATURE AT 2-M FROM SURFACE BIAS     (K)
PSNMV 1 99 SEA LEVEL PRESSURE BIAS                     (HPA)
V10MV 1 99 10 METRE V-WIND COMPONENT BIAS              (M/S)
PSLCV 1 99 SURFACE PRESSURE BIAS                       (HPA)
ENDVARS
EOF
done

../exec/new_vies.x viesfiles_$a incluir_$a excluir_$a viesfilesout_$a

exit 0
