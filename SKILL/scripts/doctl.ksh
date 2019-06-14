#!/bin/ksh 

echo "INICIANDO... \(set \-x\)"
set -x

. ../../include/config.sx6

LABELI=$1
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunmodgmpi.sx6 YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunmodgmpi.sx6 YYYYMMDDHH"
            exit 2
      else
            YYYY=`echo $LABELI |cut -c 1-4`
            MM=`echo $LABELI |cut -c 5-6`
            DD=`echo $LABELI |cut -c 7-8`
            HH=`echo $LABELI |cut -c 9-10`

            LABELF=`date -d "${NFDAYS} day ${YYYY}${MM}${DD}" +"%Y%m%d${HH}"`
            YYYYF=`echo $LABELF |cut -c 1-4`
            MMF=`echo $LABELF |cut -c 5-6`
            DDF=`echo $LABELF |cut -c 7-8`
            HHF=`echo $LABELF |cut -c 9-10`
      fi
fi

PREFX=$2

if [ -s $PREFX ]; then
      echo "ERRO - PARAMETRO PREFX\nFORMATO: runrectigge.sx6 yyyymmddhh 01N"
      exit 2
fi

DFSCSKILL=$3

if [ -s $DFSCSKILL ]; then
      echo "ERRO - PARAMETRO DFSCSKILL\nFORMATO: runrectigge.sx6 yyyymmddhh 01N 05"
      echo "Numero de dias para calcular o skill"
      exit 2
fi

NFCTDY=${FSCT}
NMDAYS=${FSCT}
NMEMBR=`echo "${NPERT}*2+1" | bc -l`
OUT=out
NPROC=1
RESOL=T${TRC}
typeset -RZ2 membn
#
#  End of setting parameters to run
#
set +x

DIRARQ=/rede/nas/modoper/tempo/global/oens/nmc/T126L28/GPOS
DIRRMV=${NAS}/removies/ensmed
GRIBMAP="/usr/local/grads/bin/gribmap "

cd ${OPERM}/SKILL/scripts
mkdir -p ./ctl
rm -f ./ctl/*
#rm -f file*
set -A NP P N ${PREFX} enmrmv

nasf=0
npc=0
while [ $npc -le 2 ]; do # VIRA O N e P, AVN e ENM RMV(RM VIES)
      membn=1
      if [ $npc -lt 2 ]; then
            while [ membn -le 7 ]; do # VIRA OS MEMBROS 01 a 07
                  echo membn=$membn${NP[$npc]}
                  t=0
                  while [ $t -le $DFSCSKILL ]; do
                        datefct=`date -d "$YYYY$MM$DD $HH:00 $t day ago" +"%Y%m%d%H"`
                        echo "DATA: $datefct -lt 2009030212"
                        if [ $datefct -lt 2009030212 ]; then
                              DIRARQ=
                        else
                              DIRARQ=/rede/nas/modoper/tempo/global/oens/nmc/T126L28/GPOS
                        fi
                        echo "DIRARQ=$DIRARQ"
                        CTLDATE=`date -d "$YYYY$MM$DD $HH:00 $t day ago" +"%HZ%d%b%Y"`
                        yyyyd=`echo $datefct | cut -c 1-4`
                        mmd=`echo $datefct | cut -c 5-6`
                        ddd=`echo $datefct | cut -c 7-8`
                        if [ $t -eq 0 ]; then
                              icnfct=icn
                        else
                              icnfct=fct
                        fi
                        
                        if [ -s ${NAS}/produtos/skill/$datefct/GSKL$membn${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb ]; then
                              nasf=1
                              ARQINGRB=${NAS}/produtos/skill/$datefct/GSKL$membn${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb
                        else
                              nasf=0
                              ARQINGRB=`ls -ltr ${DIRARQ}/$yyyyd/$mmd/$ddd/GPOS$membn${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb | head -1 | awk '{print $9}'`
                        fi
                  if [ ! -z $ARQINGRB ];then
                        ln -sf $ARQINGRB ./ctl/GPOS$membn${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb
                  else
                        ln -sf ../nullfile ./ctl/GPOS$membn${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb
                  fi
                        CTLNAME=`ls ./ctl/GPOS$membn${NP[$npc]}$datefct$LABELI*.grb | sed -e s#grb#ctl#g | sed -e s#./ctl/#""#g`
                        GRBNAME=`ls ./ctl/GPOS$membn${NP[$npc]}$datefct$LABELI*.grb | sed -e s#./ctl/#""#g`
                        IDXNAME=`ls ./ctl/GPOS$membn${NP[$npc]}$datefct$LABELI*.grb | sed -e s#grb#idx#g | sed -e s#./ctl/#""#g`
                  if [ $t -ne 0 ]; then
                  if [ $nasf -eq 0 ]; then
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
*
index ^$IDXNAME
*
undef 9.999E+20
*
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
*
dtype grib   255
*
options yrev
*
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
*
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
vars    36
topo  0 132,1,0 ** surface TOPOGRAPHY [m]
lsmk  0  81,1,0 ** surface LAND SEA MASK [0,1]
PSLC    0  135,    1,    0  ** sfc   SURFACE PRESSURE                        (HPA             )
UVES    0  192,    1,    0  ** sfc   SURFACE ZONAL WIND (U)                  (M/S             )
UVEL   13   33,  100,    0  **       ZONAL WIND (U)                          (M/S             )
VVES    0  194,    1,    0  ** sfc   SURFACE MERIDIONAL WIND (V)             (M/S             )
VVEL   13   34,  100,    0  **       MERIDIONAL WIND (V)                     (M/S             )
OMEG   13   39,  100,    0  **       OMEGA                                   (PA/S            )
DIVG   13   44,  100,    0  **       DIVERGENCE                              (1/S             )
VORT   13   43,  100,    0  **       VORTICITY                               (1/S             )
FCOR   13   35,  100,    0  **       STREAM FUNCTION                         (M2/S            )
POTV   13   36,  100,    0  **       VELOCITY POTENTIAL                      (M2/S            )
ZGEO   13    7,  100,    0  **       GEOPOTENTIAL HEIGHT                     (GPM             )
PSNM    0    2,  102,    0  ** msl   SEA LEVEL PRESSURE                      (HPA             )
TEMS    0  188,    1,    0  ** sfc   SURFACE ABSOLUTE TEMPERATURE            (K               )
TEMP   13   11,  100,    0  **       ABSOLUTE TEMPERATURE                    (K               )
UMRS    0  226,    1,    0  ** sfc   SURFACE RELATIVE HUMIDITY               (NO DIM          )
UMRL   13   52,  100,    0  **       RELATIVE HUMIDITY                       (NO DIM          )
UMES   13   51,  100,    0  **       SPECIFIC HUMIDITY                       (KG/KG           )
AGPL    0   54,  200,    0  ** atm   INST. PRECIPITABLE WATER                (KG/M2           )
TSFC    0  187,    1,    0  ** sfc   SURFACE TEMPERATURE                     (K               )
USSL    0  182,    1,    0  ** sfc   SOIL WETNESS OF SURFACE                 (0-1             )
T02M    0  128,  105,    0  ** sfc2m TEMPERATURE AT 2-M FROM SURFACE         (K               )
Q02M    0  199,    1,    0  ** sfc   SPECIFIC HUMIDITY AT 2-M FROM SURFACE   (KG/KG           )
U10M    0  130,  105,    0  ** sfc10m10 METRE U-WIND COMPONENT               (M/S             )
V10M    0  131,  105,    0  ** sfc10m10 METRE V-WIND COMPONENT               (M/S             )
PREC    0   61,    1,    0  ** sfc   TOTAL PRECIPITATION                     (KG/M2/DAY       )
PRCV    0   63,    1,    0  ** sfc   CONVECTIVE PRECIPITATION                (KG/M2/DAY       )
NEVE    0   64,    1,    0  ** sfc   SNOWFALL                                (KG/M2/DAY       )
CSSF    0  122,    1,    0  ** sfc   SENSIBLE HEAT FLUX FROM SURFACE         (W/M2            )
CLSF    0  121,    1,    0  ** sfc   LATENT HEAT FLUX FROM SURFACE           (W/M2            )
CBNV    0   71,    3,    0  ** cltlayCLOUD COVER                             (0-1             )
ROLE    0  114,    8,    0  ** toa   OUTGOING LONG WAVE AT TOP               (W/M2            )
OCAS    0  111,    1,    0  ** sfc   SHORT WAVE ABSORBED AT GROUND           (W/M2            )
SLDS    0  112,    1,    0  ** sfc   NET LONG WAVE AT BOTTOM                 (W/M2            )
TGSC    0  191,    1,    0  ** sfc   GROUND/SURFACE COVER TEMPERATURE        (K               )
endvars
CTLFCT

cat << EOF >> filefct${membn}${NP[$npc]}.T${TRC}
${OPERM}/SKILL/scripts/ctl/$CTLNAME
EOF
else
                        #CTLDATE=`date -d "$YYYY$MM$DD $HH:00 0 day ago" +"%HZ%d%b%Y"`
                        CTLDATE=`date -d "$YYYY$MM$DD $HH:00 0 day ago" +"%HZ%d%b%Y"`
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
index ^$IDXNAME
undef 1e+20
dtype grib
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
zdef 4 levels
1000 850 500 250
vars 7
psnm      0    2,  1,  0,  0 SEA LEVEL PRESSURE [hPa]
agpl      0   54,  1,  0,  0 INST. PRECIPITABLE WATER [Kg/m2]
uvel      4   33,100 ZONAL WIND (U) [m/s]
vvel      4   34,100 MERIDIONAL WIND (V) [m/s]
zgeo      4    7,100 GEOPOTENTIAL HEIGHT [gpm]
temp      4   11,100 ABSOLUTE TEMPERATURE [K]
umes      4   51,100 SPECIFIC HUMIDITY [kg/kg]
endvars
CTLFCT

cat << EOF >> filefct${membn}${NP[$npc]}.T${TRC}
${OPERM}/SKILL/scripts/ctl/$CTLNAME
EOF
fi

else
if [ $nasf -eq 0 ]; then
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
*
index ^$IDXNAME
*
undef 9.999E+20
*
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
*
dtype grib   255
*
options yrev
*
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
*
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
vars    26
topo  0 132,1,0 ** surface TOPOGRAPHY [m]
lsmk  0  81,1,0 ** surface LAND SEA MASK [0,1]
PSLC    0  135,    1,    0  ** sfc   SURFACE PRESSURE                        (HPA             )
UVES    0  192,    1,    0  ** sfc   SURFACE ZONAL WIND (U)                  (M/S             )
UVEL   13   33,  100,    0  **       ZONAL WIND (U)                          (M/S             )
VVES    0  194,    1,    0  ** sfc   SURFACE MERIDIONAL WIND (V)             (M/S             )
VVEL   13   34,  100,    0  **       MERIDIONAL WIND (V)                     (M/S             )
OMEG   13   39,  100,    0  **       OMEGA                                   (PA/S            )
DIVG   13   44,  100,    0  **       DIVERGENCE                              (1/S             )
VORT   13   43,  100,    0  **       VORTICITY                               (1/S             )
FCOR   13   35,  100,    0  **       STREAM FUNCTION                         (M2/S            )
POTV   13   36,  100,    0  **       VELOCITY POTENTIAL                      (M2/S            )
ZGEO   13    7,  100,    0  **       GEOPOTENTIAL HEIGHT                     (GPM             )
PSNM    0    2,  102,    0  ** msl   SEA LEVEL PRESSURE                      (HPA             )
TEMS    0  188,    1,    0  ** sfc   SURFACE ABSOLUTE TEMPERATURE            (K               )
TEMP   13   11,  100,    0  **       ABSOLUTE TEMPERATURE                    (K               )
UMRS    0  226,    1,    0  ** sfc   SURFACE RELATIVE HUMIDITY               (NO DIM          )
UMRL   13   52,  100,    0  **       RELATIVE HUMIDITY                       (NO DIM          )
UMES   13   51,  100,    0  **       SPECIFIC HUMIDITY                       (KG/KG           )
AGPL    0   54,  200,    0  ** atm   INST. PRECIPITABLE WATER                (KG/M2           )
TSFC    0  187,    1,    0  ** sfc   SURFACE TEMPERATURE                     (K               )
USSL    0  182,    1,    0  ** sfc   SOIL WETNESS OF SURFACE                 (0-1             )
T02M    0  128,  105,    0  ** sfc2m TEMPERATURE AT 2-M FROM SURFACE         (K               )
Q02M    0  199,    1,    0  ** sfc   SPECIFIC HUMIDITY AT 2-M FROM SURFACE   (KG/KG           )
U10M    0  130,  105,    0  ** sfc10m10 METRE U-WIND COMPONENT               (M/S             )
V10M    0  131,  105,    0  ** sfc10m10 METRE V-WIND COMPONENT               (M/S             )
endvars
CTLFCT
else
                        CTLDATE=`date -d "$YYYY$MM$DD $HH:00 0 day ago" +"%HZ%d%b%Y"`
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
index ^$IDXNAME
undef 1e+20
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
dtype grib
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
zdef 4 levels
1000 850 500 250
vars 7
psnm      0    2,  1,  0,  0 SEA LEVEL PRESSURE [hPa]
agpl      0   54,  1,  0,  0 INST. PRECIPITABLE WATER [Kg/m2]
uvel      4   33,100 ZONAL WIND (U) [m/s]
vvel      4   34,100 MERIDIONAL WIND (V) [m/s]
zgeo      4    7,100 GEOPOTENTIAL HEIGHT [gpm]
temp      4   11,100 ABSOLUTE TEMPERATURE [K]
umes      4   51,100 SPECIFIC HUMIDITY [kg/kg]
endvars
CTLFCT

fi
fi

                  cd ./ctl                     
                  ${GRIBMAP} -i $CTLNAME
                  cd ../
                  let t=t+1
                  done
            let membn=membn+1
            done
      else
            echo membn=${NP[$npc]}
            
            t=0
            while [ $t -le $DFSCSKILL ]; do
                  datefct=`date -d "$YYYY$MM$DD $HH:00 $t day ago" +"%Y%m%d%H"`
                        echo "DATA: $datefct -lt 2009030212"
                        if [ $datefct -lt 2009030212 ]; then
                              DIRARQ=
                        else
                              DIRARQ=/rede/nas/modoper/tempo/global/oens/nmc/T126L28/GPOS
                        fi
                        echo "DIRARQ=$DIRARQ"
                  CTLDATE=`date -d "$YYYY$MM$DD $HH:00 $t day ago" +"%HZ%d%b%Y"`
                  yyyyd=`echo $datefct | cut -c 1-4`
                  mmd=`echo $datefct | cut -c 5-6`
                  ddd=`echo $datefct | cut -c 7-8`
                  if [ $t -eq 0 ]; then
                        icnfct=icn
                  else
                        icnfct=fct
                  fi


                  if [ -s ${NAS}/produtos/skill/$datefct/GSKL${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb ]; then
                        nasf=1
                        ARQINGRB=${NAS}/produtos/skill/$datefct/GSKL${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb
                  else
                        nasf=0
                        ARQINGRB=`ls -ltr ${DIRARQ}/$yyyyd/$mmd/$ddd/GPOS${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb | head -1 | awk '{print $9}'`
                  fi
                  if [ ! -z $ARQINGRB ];then
                        ln -sf $ARQINGRB ./ctl/GPOS${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb
                  else
                        ln -sf ../nullfile ./ctl/GPOS${NP[$npc]}$datefct${LABELI}P.${icnfct}.T${TRC}L${LV}.grb
                  fi


                  CTLNAME=`ls ./ctl/GPOS${NP[$npc]}$datefct$LABELI*.grb | sed -e s#grb#ctl#g | sed -e s#./ctl/#""#g`
                  GRBNAME=`ls ./ctl/GPOS${NP[$npc]}$datefct$LABELI*.grb | sed -e s#./ctl/#""#g`
                  IDXNAME=`ls ./ctl/GPOS${NP[$npc]}$datefct$LABELI*.grb | sed -e s#grb#idx#g | sed -e s#./ctl/#""#g`
if [ $t -ne 0 ]; then
if [ $nasf -eq 0 ]; then
                        #CTLDATE=`date -d "$YYYY$MM$DD $HH:00 0 day ago" +"%HZ%d%b%Y"`
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
*
index ^$IDXNAME
*
undef 9.999E+20
*
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
*
dtype grib   255
*
options yrev
*
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
*
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
vars    36
topo  0 132,1,0 ** surface TOPOGRAPHY [m]
lsmk  0  81,1,0 ** surface LAND SEA MASK [0,1]
PSLC    0  135,    1,    0  ** sfc   SURFACE PRESSURE                        (HPA             )
UVES    0  192,    1,    0  ** sfc   SURFACE ZONAL WIND (U)                  (M/S             )
UVEL   13   33,  100,    0  **       ZONAL WIND (U)                          (M/S             )
VVES    0  194,    1,    0  ** sfc   SURFACE MERIDIONAL WIND (V)             (M/S             )
VVEL   13   34,  100,    0  **       MERIDIONAL WIND (V)                     (M/S             )
OMEG   13   39,  100,    0  **       OMEGA                                   (PA/S            )
DIVG   13   44,  100,    0  **       DIVERGENCE                              (1/S             )
VORT   13   43,  100,    0  **       VORTICITY                               (1/S             )
FCOR   13   35,  100,    0  **       STREAM FUNCTION                         (M2/S            )
POTV   13   36,  100,    0  **       VELOCITY POTENTIAL                      (M2/S            )
ZGEO   13    7,  100,    0  **       GEOPOTENTIAL HEIGHT                     (GPM             )
PSNM    0    2,  102,    0  ** msl   SEA LEVEL PRESSURE                      (HPA             )
TEMS    0  188,    1,    0  ** sfc   SURFACE ABSOLUTE TEMPERATURE            (K               )
TEMP   13   11,  100,    0  **       ABSOLUTE TEMPERATURE                    (K               )
UMRS    0  226,    1,    0  ** sfc   SURFACE RELATIVE HUMIDITY               (NO DIM          )
UMRL   13   52,  100,    0  **       RELATIVE HUMIDITY                       (NO DIM          )
UMES   13   51,  100,    0  **       SPECIFIC HUMIDITY                       (KG/KG           )
AGPL    0   54,  200,    0  ** atm   INST. PRECIPITABLE WATER                (KG/M2           )
TSFC    0  187,    1,    0  ** sfc   SURFACE TEMPERATURE                     (K               )
USSL    0  182,    1,    0  ** sfc   SOIL WETNESS OF SURFACE                 (0-1             )
T02M    0  128,  105,    0  ** sfc2m TEMPERATURE AT 2-M FROM SURFACE         (K               )
Q02M    0  199,    1,    0  ** sfc   SPECIFIC HUMIDITY AT 2-M FROM SURFACE   (KG/KG           )
U10M    0  130,  105,    0  ** sfc10m10 METRE U-WIND COMPONENT               (M/S             )
V10M    0  131,  105,    0  ** sfc10m10 METRE V-WIND COMPONENT               (M/S             )
PREC    0   61,    1,    0  ** sfc   TOTAL PRECIPITATION                     (KG/M2/DAY       )
PRCV    0   63,    1,    0  ** sfc   CONVECTIVE PRECIPITATION                (KG/M2/DAY       )
NEVE    0   64,    1,    0  ** sfc   SNOWFALL                                (KG/M2/DAY       )
CSSF    0  122,    1,    0  ** sfc   SENSIBLE HEAT FLUX FROM SURFACE         (W/M2            )
CLSF    0  121,    1,    0  ** sfc   LATENT HEAT FLUX FROM SURFACE           (W/M2            )
CBNV    0   71,    3,    0  ** cltlayCLOUD COVER                             (0-1             )
ROLE    0  114,    8,    0  ** toa   OUTGOING LONG WAVE AT TOP               (W/M2            )
OCAS    0  111,    1,    0  ** sfc   SHORT WAVE ABSORBED AT GROUND           (W/M2            )
SLDS    0  112,    1,    0  ** sfc   NET LONG WAVE AT BOTTOM                 (W/M2            )
TGSC    0  191,    1,    0  ** sfc   GROUND/SURFACE COVER TEMPERATURE        (K               )
endvars
CTLFCT

cat << EOF >> filefct${NP[$npc]}.T${TRC}
${OPERM}/SKILL/scripts/ctl/$CTLNAME
EOF
else
                        CTLDATE=`date -d "$YYYY$MM$DD $HH:00 0 day ago" +"%HZ%d%b%Y"`
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
index ^$IDXNAME
undef 1e+20
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
dtype grib
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
zdef 4 levels
1000 850 500 250
vars 7
psnm      0    2,  1,  0,  0 SEA LEVEL PRESSURE [hPa]
agpl      0   54,  1,  0,  0 INST. PRECIPITABLE WATER [Kg/m2]
uvel      4   33,100 ZONAL WIND (U) [m/s]
vvel      4   34,100 MERIDIONAL WIND (V) [m/s]
zgeo      4    7,100 GEOPOTENTIAL HEIGHT [gpm]
temp      4   11,100 ABSOLUTE TEMPERATURE [K]
umes      4   51,100 SPECIFIC HUMIDITY [kg/kg]
endvars
*
CTLFCT

cat << EOF >> filefct${NP[$npc]}.T${TRC}
${OPERM}/SKILL/scripts/ctl/$CTLNAME
EOF

fi
else
if [ $nasf -eq 0 ]; then
                        #CTLDATE=`date -d "$YYYY$MM$DD $HH:00 0 day ago" +"%HZ%d%b%Y"`
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
*
index ^$IDXNAME
*
undef 9.999E+20
*
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
*
dtype grib   255
*
options yrev
*
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
*
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
vars    26
topo  0 132,1,0 ** surface TOPOGRAPHY [m]
lsmk  0  81,1,0 ** surface LAND SEA MASK [0,1]
PSLC    0  135,    1,    0  ** sfc   SURFACE PRESSURE                        (HPA             )
UVES    0  192,    1,    0  ** sfc   SURFACE ZONAL WIND (U)                  (M/S             )
UVEL   13   33,  100,    0  **       ZONAL WIND (U)                          (M/S             )
VVES    0  194,    1,    0  ** sfc   SURFACE MERIDIONAL WIND (V)             (M/S             )
VVEL   13   34,  100,    0  **       MERIDIONAL WIND (V)                     (M/S             )
OMEG   13   39,  100,    0  **       OMEGA                                   (PA/S            )
DIVG   13   44,  100,    0  **       DIVERGENCE                              (1/S             )
VORT   13   43,  100,    0  **       VORTICITY                               (1/S             )
FCOR   13   35,  100,    0  **       STREAM FUNCTION                         (M2/S            )
POTV   13   36,  100,    0  **       VELOCITY POTENTIAL                      (M2/S            )
ZGEO   13    7,  100,    0  **       GEOPOTENTIAL HEIGHT                     (GPM             )
PSNM    0    2,  102,    0  ** msl   SEA LEVEL PRESSURE                      (HPA             )
TEMS    0  188,    1,    0  ** sfc   SURFACE ABSOLUTE TEMPERATURE            (K               )
TEMP   13   11,  100,    0  **       ABSOLUTE TEMPERATURE                    (K               )
UMRS    0  226,    1,    0  ** sfc   SURFACE RELATIVE HUMIDITY               (NO DIM          )
UMRL   13   52,  100,    0  **       RELATIVE HUMIDITY                       (NO DIM          )
UMES   13   51,  100,    0  **       SPECIFIC HUMIDITY                       (KG/KG           )
AGPL    0   54,  200,    0  ** atm   INST. PRECIPITABLE WATER                (KG/M2           )
TSFC    0  187,    1,    0  ** sfc   SURFACE TEMPERATURE                     (K               )
USSL    0  182,    1,    0  ** sfc   SOIL WETNESS OF SURFACE                 (0-1             )
T02M    0  128,  105,    0  ** sfc2m TEMPERATURE AT 2-M FROM SURFACE         (K               )
Q02M    0  199,    1,    0  ** sfc   SPECIFIC HUMIDITY AT 2-M FROM SURFACE   (KG/KG           )
U10M    0  130,  105,    0  ** sfc10m10 METRE U-WIND COMPONENT               (M/S             )
V10M    0  131,  105,    0  ** sfc10m10 METRE V-WIND COMPONENT               (M/S             )
endvars
CTLFCT

cat << EOF >> fileanl.T${TRC}
${OPERM}/SKILL/scripts/ctl/$CTLNAME
EOF
else
                        CTLDATE=`date -d "$YYYY$MM$DD $HH:00 0 day ago" +"%HZ%d%b%Y"`
cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
index ^$IDXNAME
undef 1e+20
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T126L28  COLD
dtype grib
xdef   384 linear    0.000   0.9375000000
ydef   192 levels 
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
tdef     1 linear $CTLDATE 6hr
*
zdef 4 levels
1000 850 500 250
vars 7
psnm      0    2,  1,  0,  0 SEA LEVEL PRESSURE [hPa]
agpl      0   54,  1,  0,  0 INST. PRECIPITABLE WATER [Kg/m2]
uvel      4   33,100 ZONAL WIND (U) [m/s]
vvel      4   34,100 MERIDIONAL WIND (V) [m/s]
zgeo      4    7,100 GEOPOTENTIAL HEIGHT [gpm]
temp      4   11,100 ABSOLUTE TEMPERATURE [K]
umes      4   51,100 SPECIFIC HUMIDITY [kg/kg]
endvars
CTLFCT

cat << EOF >> fileanl.T${TRC}
${OPERM}/SKILL/scripts/ctl/$CTLNAME
EOF

fi
fi
                  cd ./ctl                     
                  ${GRIBMAP} -i $CTLNAME
                  cd ../
            
            let t=t+1
            done
      fi
let npc=npc+1
done

cat << EOF >> filec.T${TRC}.n
${OPERM}/SKILL/scripts/clmreannmc.T126.ctl
EOF

npc=3
t=1
echo "MAMEBRO=${NP[$npc]}"
while [ $t -le $DFSCSKILL ]; do
      datefct=`date -d "$YYYY$MM$DD $HH:00 $t day ago" +"%Y%m%d%H"`
                        echo "DATA: $datefct -lt 2009030212"
                        if [ $datefct -lt 2009030212 ]; then
                              DIRARQ= 
                        else
                              DIRARQ=/rede/nas/modoper/tempo/global/oens/nmc/T126L28/GPOS
                        fi
                        echo "DIRARQ=$DIRARQ"
      ASD=`echo $LABELI | cut -c 1-8`
      HH=`echo $LABELI | cut -c 9-10`
      CTLDATE=`date -d "$ASD $HH:00 0 day ago" +"%HZ%d%b%Y"`
      yyyyd=`echo $datefct | cut -c 1-4`
      mmd=`echo $datefct | cut -c 5-6`
      ddd=`echo $datefct | cut -c 7-8`

      if [ $t -eq 0 ]; then
            icnfct=icn
      else
            icnfct=fct
      fi
set -x
      ARQINGRB=`ls -ltr ${DIRRMV}/$yyyyd/$mmd/$ddd/gpos${NP[$npc]}$datefct${LABELI}p.${icnfct}.t${TRC}l${LV} | head -1 | awk '{print $9}'`

      if [ ! -z $ARQINGRB ];then
            ln -sf ${DIRRMV}/$yyyyd/$mmd/$ddd/gpos${NP[$npc]}$datefct${LABELI}p.${icnfct}.t${TRC}l${LV} ./ctl/
      else
            ln -sf ../nullfile ./ctl/gpos${NP[$npc]}$datefct${LABELI}p.${icnfct}.t${TRC}l${LV}
      fi
      CTLNAME=`basename $ARQINGRB`.ctl
      GRBNAME=`basename $ARQINGRB`
      IDXNAME=

cat << CTLFCT > ./ctl/$CTLNAME
dset ^$GRBNAME
*
*index ^$IDXNAME
*
undef 1e+20
*
*dtype grib
*
xdef 384 linear 0.000000 0.937500
*
ydef 192 levels
-89.284 -88.357
-87.424 -86.490 -85.556 -84.621 -83.687 -82.752 -81.817 -80.882 -79.947 -79.012
-78.077 -77.142 -76.207 -75.272 -74.337 -73.402 -72.467 -71.532 -70.597 -69.662
-68.727 -67.792 -66.857 -65.922 -64.987 -64.052 -63.116 -62.181 -61.246 -60.311
-59.376 -58.441 -57.506 -56.571 -55.636 -54.701 -53.766 -52.831 -51.896 -50.961
-50.026 -49.091 -48.156 -47.221 -46.286 -45.350 -44.415 -43.480 -42.545 -41.610
-40.675 -39.740 -38.805 -37.870 -36.935 -36.000 -35.065 -34.130 -33.195 -32.260
-31.325 -30.389 -29.454 -28.519 -27.584 -26.649 -25.714 -24.779 -23.844 -22.909
-21.974 -21.039 -20.104 -19.169 -18.234 -17.299 -16.364 -15.429 -14.493 -13.558
-12.623 -11.688 -10.753  -9.818  -8.883  -7.948  -7.013  -6.078  -5.143  -4.208
 -3.273  -2.338  -1.403  -0.468   0.468   1.403   2.338   3.273   4.208   5.143
  6.078   7.013   7.948   8.883   9.818  10.753  11.688  12.623  13.558  14.493
 15.429  16.364  17.299  18.234  19.169  20.104  21.039  21.974  22.909  23.844
 24.779  25.714  26.649  27.584  28.519  29.454  30.389  31.325  32.260  33.195
 34.130  35.065  36.000  36.935  37.870  38.805  39.740  40.675  41.610  42.545
 43.480  44.415  45.350  46.286  47.221  48.156  49.091  50.026  50.961  51.896
 52.831  53.766  54.701  55.636  56.571  57.506  58.441  59.376  60.311  61.246
 62.181  63.116  64.052  64.987  65.922  66.857  67.792  68.727  69.662  70.597
 71.532  72.467  73.402  74.337  75.272  76.207  77.142  78.077  79.012  79.947
 80.882  81.817  82.752  83.687  84.621  85.556  86.490  87.424  88.357  89.284
*
zdef 3 levels
850 500 250
*
tdef 1 linear $CTLDATE  6hr
*
vars 9
psnm      0    1,  1,  0,  0 SEA LEVEL PRESSURE [hPa] []
tems      0    2,  1,  0,  0 SURFACE ABSOLUTE TEMPERATURE [K] []
umes      3    3,100 SURFACE SPEC HUMIDITY [no Dim] []
umrl      3    4,100 RELATIVE HUMIDITY [no Dim] []
fcor      3    5,100 STREAM FUNCTION [m2/s] []
temp      3    6,100 ABSOLUTE TEMPERATURE [K] []
zgeo      3    7,100 GEOPOTENTIAL HEIGHT [gpm] []
uvel      3    8,100 ZONAL WIND (U) [m/s] []
vvel      3    9,100 MERIDIONAL WIND (V) [m/s] []
endvars
CTLFCT
      cd ./ctl                     
#      ${GRIBMAP} -i $CTLNAME
      cd ../
cat << EOF >> filefctrmv.T${TRC}
${OPERM}/SKILL/scripts/ctl/$CTLNAME
EOF

let t=t+1
done

exit 0
