#!/bin/csh
#help#
#**********************************************************#
#                                                          #
#     Name:           doctl.T126.csh                       #
#                                                          #
#     Function:       This script create the list          #
#                     of ctls that will be opened          #
#                     by skill.gs                          #
#                     It runs in C shell.                  #
#                                                          #
#     Date:           September 06th, 2001.                #
#     Last change:    September 06th, 2001.                #
#                                                          #
#     Valid Arguments for doctl.T126.csh                   #
#                                                          #
#     First : LABELI  : help or                            #
#     First : LABELI  : initial forecasting label          #
#    Second : PERT    : number of members of the ensemble  #
#     Third : FCTDAYS : number of forecast days            #
#                                                          #
#             LABELx : yyyymmddhh                          #
#                      yyyy = four digit year              #
#                        mm = two digit month              #
#                        dd = two digit day                #
#                        hh = two digit hour               #
#                                                          #
#**********************************************************#
#help#
#
#       Help:
#

if ("${1}" == "help" |"${1}" == "" ) then
cat < ${0} | sed -n '/^#help#/,/^#help#/p'
exit 0
endif
#
#       Test of Valid Arguments
#
if ("${1}" == "" ) then
echo "LABELI is not set (yyyymmddhh)"
exit 1
endif
if ("${2}" == "" ) then
echo "PERT is not set"
exit
endif
if ("${3}" == "" ) then
echo "FCTDAYS is not set"
exit
endif
#
set date=${1}
set pert=${2}
set fctdays=${3}
set resol=T126L28
set trc=T126
#

set OPERM = `cat ../../include/config.sx6 |grep "mHOME=" | cut -d"=" -f2`
set ROPERM = `cat ../../include/config.sx6 |grep "mDK=" | cut -d"=" -f2`
set BANGU = `cat ../../include/config.sx6 |grep "BANGU=" | cut -d"=" -f2`

echo OPERM=$OPERM
echo ROPERM=$ROPERM
echo BANGU=$BANGU

@ pert=(${pert} - 1) / 2
echo 'pert='${pert}
#
set yy=`echo ${date} | cut -c 1-4`
set mm=`echo ${date} | cut -c 5-6`
set dd=`echo ${date} | cut -c 7-8`
set hh=`echo ${date} | cut -c 9-10`
#
set date=${yy}${mm}${dd}${hh}
echo 'date='${date}
#
set mon=(JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC)
set mes=${mon[${mm}]}
set mes2=${mes}
#
set mm2=${mm}
set mma=${mm}
@ dd2=${dd} - ${fctdays}
if (${dd2} <= 0) then      
@ mm2=${mm} - 1
if (${mm2} == 0) then
set mm2=12
endif
endif
#
set mma='$'${mm2}
echo 'mma='${mma}
@ year= ${yy} % 4
if (${year} == 0) then
set days='31 29 31 30 31 30 31 31 30 31 30 31'
else
set days='31 28 31 30 31 30 31 31 30 31 30 31'
endif
set daya=`echo ${days} | awk '{print '${mma}'}'`
if (${dd2} <= 0) then      
@ dd2=${daya} + ${dd2}
endif
if (${dd2} < 10) then
set dd2='0'${dd2}
endif
#
#AAFset DIRSCR='/gfs/home3/modoper/tempo/global/oenspro/produtos/SKILL/scripts'
#AAFset DIRARQ=/bangu/samfs/modoper/tempo/global/oens/avn/${resol}/GPOS
#AAFset DIRCTL='/gfs/home3/modoper/tempo/global/oenspro/produtos/SKILL/scripts/ctl'
#AAFset DIRGRD='/usr/local/grads/bin'
set DIRSCR=${OPERM}/SKILL/scripts
set DIRARQ=${BANGU}/GPOS
set DIRCTL=${OPERM}/SKILL/scripts/ctl
set DIRGRD=/usr/local/grads/bin
#
cd ${DIRSCR}
#
rm -f fileanl.${trc}
rm -f filefctAVN.${trc}
#
####################################
#
echo 'file analysis'
#
cat <<EOT> fileanl.${trc}
${DIRCTL}/GPOSAVN${date}${date}P.icn.${resol}.ctl
EOT
#
if ( -e ${DIRARQ}/${yy}/${mm}/${dd}/GPOSAVN${date}${date}P.icn.${resol}.grb ) then
echo "${DIRARQ}/${yy}/${mm}/${dd}/GPOSAVN${date}${date}P.icn.${resol}.grb exist"
ln -s ${DIRARQ}/${yy}/${mm}/${dd}/GPOSAVN${date}${date}P.icn.${resol}.grb ${DIRCTL}/GPOSAVN${date}${date}P.icn.${resol}.grb  
else
echo "${DIRARQ}/${yy}/${mm}/${dd}/GPOSAVN${date}${date}P.icn.${resol}.grb does not exist"
ln -s ${DIRSCR}/nullfile ${DIRCTL}/GPOSAVN${date}${date}P.icn.${resol}.grb  
endif
#
cat <<EOT> ${DIRCTL}/GPOSAVN${date}${date}P.icn.${resol}.ctl
dset ${DIRCTL}/GPOSAVN${date}${date}P.icn.${resol}.grb 
title pressure history cptec agcm v3.0 1999 t126l28 cold
undef 9.999E+20
options yrev
dtype grib
index ${DIRCTL}/GPOSAVN${date}${date}P.icn.${resol}.gmp
XDEF 384 LINEAR    0.000   0.937500
YDEF 192 LEVELS 
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
TDEF   1 LINEAR ${hh}Z${dd}${mes}${yy} 24HR
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
EOT
${DIRGRD}/gribmap -i ${DIRCTL}/GPOSAVN${date}${date}P.icn.${resol}.ctl 
#
####################################
#
echo 'files forecast'
#
set npert=1
while (${npert} <= ${pert})
if (${npert} < 10) then
set npert='0'${npert}
endif
#
rm -f filefct${npert}P.${trc}
rm -f filefct${npert}N.${trc}
#
set yy2=${yy}
set mm2=${mm}
set dd2=${dd}
set hh2=${hh}
set mes2=${mes}
#
set tim=1
while (${tim} <= ${fctdays})
#
@ dd2=${dd} - ${tim}
if (${dd2} <= 0) then      
@ mm2=${mm} - 1
if (${mm2} == 0) then
@ yy2=${yy} - 1
set mm2=12
endif
set mes2=${mon[${mm2}]}
if (${mm2} < 10) then
set mm2='0'${mm2}
endif
endif
if (${dd2} <= 0) then      
@ dd2=${daya} + ${dd2}
endif
if (${dd2} < 10) then
set dd2='0'${dd2}
endif
#
set date2=${yy2}${mm2}${dd2}${hh2}
#
cat <<EOT>> filefct${npert}P.${trc}
${DIRCTL}/GPOS${npert}P${date2}${date}P.fct.${resol}.ctl
EOT
#
if ( -e ${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}P${date2}${date}P.fct.${resol}.grb ) then
echo "${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}P${date2}${date}P.fct.${resol}.grb exist"
ln -s ${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}P${date2}${date}P.fct.${resol}.grb ${DIRCTL}/GPOS${npert}P${date2}${date}P.fct.${resol}.grb 
else
echo "${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}P${date2}${date}P.fct.${resol}.grb does not exist"
ln -s ${DIRSCR}/nullfile ${DIRCTL}/GPOS${npert}P${date2}${date}P.fct.${resol}.grb 
endif
#
cat <<EOT > ${DIRCTL}/GPOS${npert}P${date2}${date}P.fct.${resol}.ctl
DSET ${DIRCTL}/GPOS${npert}P${date2}${date}P.fct.${resol}.grb
title pressure history cptec agcm v3.0 1999 t126l28 cold
undef 9.999E+20
options yrev
dtype grib
index ${DIRCTL}/GPOS${npert}P${date2}${date}P.fct.${resol}.gmp
XDEF 384 LINEAR    0.000   0.937500
YDEF 192 LEVELS 
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
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
TDEF   1 LINEAR ${hh}Z${dd}${mes}${yy} 24HR
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
EOT
${DIRGRD}/gribmap -i ${DIRCTL}/GPOS${npert}P${date2}${date}P.fct.${resol}.ctl 
#
cat <<EOT>> filefct${npert}N.${trc}
${DIRCTL}/GPOS${npert}N${date2}${date}P.fct.${resol}.ctl
EOT
#
if ( -e ${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}N${date2}${date}P.fct.${resol}.grb ) then
echo "${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}N${date2}${date}P.fct.${resol}.grb exist"
ln -s ${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}N${date2}${date}P.fct.${resol}.grb ${DIRCTL}/GPOS${npert}N${date2}${date}P.fct.${resol}.grb 
else
echo "${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOS${npert}N${date2}${date}P.fct.${resol}.grb does not exist"
ln -s ${DIRSCR}/nullfile ${DIRCTL}/GPOS${npert}N${date2}${date}P.fct.${resol}.grb 
endif
#
cat <<EOT > ${DIRCTL}/GPOS${npert}N${date2}${date}P.fct.${resol}.ctl
DSET ${DIRCTL}/GPOS${npert}N${date2}${date}P.fct.${resol}.grb
title pressure history cptec agcm v3.0 1999 t126l28 cold
undef 9.999E+20
options yrev
dtype grib
index ${DIRCTL}/GPOS${npert}N${date2}${date}P.fct.${resol}.gmp
XDEF 384 LINEAR    0.000   0.937500
YDEF 192 LEVELS 
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
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
TDEF   1 LINEAR ${hh}Z${dd}${mes}${yy} 24HR
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
EOT
${DIRGRD}/gribmap -i ${DIRCTL}/GPOS${npert}N${date2}${date}P.fct.${resol}.ctl 
#
if ("${npert}" == "01") then
cat <<EOT>> filefctAVN.${trc}
${DIRCTL}/GPOSAVN${date2}${date}P.fct.${resol}.ctl
EOT
#
if ( -e ${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOSAVN${date2}${date}P.fct.${resol}.grb ) then
echo "${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOSAVN${date2}${date}P.fct.${resol}.grb exist"
ln -s ${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOSAVN${date2}${date}P.fct.${resol}.grb  ${DIRCTL}/GPOSAVN${date2}${date}P.fct.${resol}.grb  
else
echo "${DIRARQ}/${yy2}/${mm2}/${dd2}/GPOSAVN${date2}${date}P.fct.${resol}.grb does not exist"
ln -s ${DIRSCR}/nullfile ${DIRCTL}/GPOSAVN${date2}${date}P.fct.${resol}.grb  
endif
#
cat <<EOT > ${DIRCTL}/GPOSAVN${date2}${date}P.fct.${resol}.ctl
DSET ${DIRCTL}/GPOSAVN${date2}${date}P.fct.${resol}.grb 
title pressure history cptec agcm v3.0 1999 t126l28 cold
undef 9.999E+20
options yrev
dtype grib
index ${DIRCTL}/GPOSAVN${date2}${date}P.fct.${resol}.gmp
XDEF 384 LINEAR    0.000   0.937500
YDEF 192 LEVELS 
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
zdef    13 levels  1000  925  850  700  500  300  250  200  100   70
                   50   30   10
TDEF   1 LINEAR ${hh}Z${dd}${mes}${yy} 24HR
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
EOT
${DIRGRD}/gribmap -i ${DIRCTL}/GPOSAVN${date2}${date}P.fct.${resol}.ctl 
#
endif
#
@ tim=${tim} + 1
end
#
set listP=`cat filefct${npert}P.${trc}`
set listN=`cat filefct${npert}N.${trc}`
rm -f filefct${npert}P.${trc}
rm -f filefct${npert}N.${trc}
set lk=${fctdays}
while (${lk} >= 1)
set arg='$'${lk}
set fileP=`echo ${listP} | awk '{print '${arg}'}'` 
set fileN=`echo ${listN} | awk '{print '${arg}'}'` 
echo ${fileP} >> filefct${npert}P.${trc}
echo ${fileN} >> filefct${npert}N.${trc}
@ lk=${lk} - 1
end
#
@ npert=${npert} + 1
end
#
set listAVN=`cat filefctAVN.${trc}`
rm -f filefctAVN.${trc}
set lk=${fctdays}
while (${lk} >= 1)
set arg='$'${lk}
set fileAVN=`echo ${listAVN} | awk '{print '${arg}'}'` 
echo ${fileAVN} >> filefctAVN.${trc}
@ lk=${lk} - 1
end
#
exit
#

