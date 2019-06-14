#!/bin/ksh -x

. ../include/config.sx6

BANGU=${NAS} #ABS

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

PERT=$2

DATECTL=`$CALDATE $LABELI + 0d "hhZddmmmyyyy"`

if [ $PERT = "ENM" ]; then
#      out=${mDK}/ensmed/dataout/T${TRC}L${LV}/
      out=${BANGU}/ENSMED/$YYYY/$MM/$DD
else
      if [ $PERT = "PROB" ]; then
#            out=${mDK}/probability/dataout/T${TRC}L${LV}/
            out=${BANGU}/produtos/probability/binctl/$YYYY/$MM/$DD
      else
#            out=${mDK}/pos/dataout/T${TRC}L${LV}/
            out=${BANGU}/GPOS/$YYYY/$MM/$DD
      fi
fi

if [ ! $PERT = "PROB" ]; then
cat << EOF > ${out}/GPOS${PERT}${LABELI}.ctl
dset ^GPOS${PERT}${LABELI}%y4%m2%d2%h2P.fct.T${TRC}L${LV}.grb
*
index ^GPOS${PERT}${LABELI}.gmp
*
undef 9.999E+20
*
title PRESSURE HISTORY    CPTEC AGCM REVIS 1.0 2000  T${TRC}L${LV}  COLD
*
dtype grib   255
*
options yrev template
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
tdef     61 linear ${DATECTL} 6hr
*
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
vars    38
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
UVMT   13  255,  100,    0  **       TIME MEAN ZONAL WIND (U)                (M/S             )
VVMT   13  241,  100,    0  **       TIME MEAN MERIDIONAL WIND (V)           (M/S             )
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

EOF

a=`pwd`
cd $out
ln -s GPOS${PERT}${LABELI}${LABELI}P.icn.T${TRC}L${LV}.grb GPOS${PERT}${LABELI}${LABELI}P.fct.T${TRC}L${LV}.grb
$GRIBMAP -i $out/GPOS${PERT}${LABELI}.ctl
cd $a

else

DATECTL=`$CALDATE $LABELI + 1d "hhZddmmmyyyy"`

cat << EOF > ${out}/prob${LABELI}.ctl
DSET ^prob${LABELI}%y4%m2%d2%h2.T${TRC}L${LV}.bin
*
OPTIONS SEQUENTIAL BIG_ENDIAN YREV TEMPLATE
*
UNDEF -2.56E+33
*
TITLE PROBABILITIES FROM ENS CPTEC AGCM v3.0 1999 T${TRC}L${LV}  COLD
*
XDEF    384  LINEAR    0.000000   0.937500
YDEF    192  LEVELS
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
ZDEF   1 LEVELS 1000
TDEF   57 LINEAR ${DATECTL} 06HR
*
VARS  4
PROB1   0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION >  1.0 mm
PROB5   0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION >  5.0 mm
PROB10  0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION > 10.0 mm
PROB20  0 99 PROB. OF 24HR ACCUMULATED PRECIPITATION > 20.0 mm
ENDVARS

EOF

fi


exit 0
