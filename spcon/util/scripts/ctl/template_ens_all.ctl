dset ^%e/GPOS%e2020040200%y4%m2%d2%h2P.fct.TQ0126L028.grb
*
index ^template_ens_all.idx
*
undef 9.999E+20
*
title PRESSURE HISTORY    PTEC AGCM REVIS 1.0 2000  T012628   COLD
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
*
tdef     61 linear 00Z02APR2020 6hr
edef 15
NMC 61 00Z02APR2020   
01N 61 00Z02APR2020   
01P 61 00Z02APR2020   
02N 61 00Z02APR2020   
02P 61 00Z02APR2020   
03N 61 00Z02APR2020   
03P 61 00Z02APR2020   
04N 61 00Z02APR2020   
04P 61 00Z02APR2020   
05N 61 00Z02APR2020   
05P 61 00Z02APR2020   
06N 61 00Z02APR2020   
06P 61 00Z02APR2020   
07N 61 00Z02APR2020   
07P 61 00Z02APR2020   
endedef
*
zdef    18 levels  1000  925  850  775  700  500  400  300  250  200  150   100  70   50   30   20   10   3
vars    34
topo  0 132,1,0 ** surface TOPOGRAPHY [m]
lsmk  0  81,1,0 ** surface LAND SEA MASK [0,1]
PSLC    0  135,    1,    0  ** sfc   SURFACE PRESSURE                        (HPA             )
UVES    0  192,    1,    0  ** sfc   SURFACE ZONAL WIND (U)                  (M/S             )
UVEL   18   33,  100,    0  **       ZONAL WIND (U)                          (M/S             )
VVES    0  194,    1,    0  ** sfc   SURFACE MERIDIONAL WIND (V)             (M/S             )
VVEL   18   34,  100,    0  **       MERIDIONAL WIND (V)                     (M/S             )
OMEG   18   39,  100,    0  **       OMEGA                                   (PA/S            )
VORT   18   43,  100,    0  **       VORTICITY                               (1/S             )
FCOR   18   35,  100,    0  **       STREAM FUNCTION                         (M2/S            )
POTV   18   36,  100,    0  **       VELOCITY POTENTIAL                      (M2/S            )
ZGEO   18    7,  100,    0  **       GEOPOTENTIAL HEIGHT                     (GPM             )
PSNM    0    2,  102,    0  ** msl   SEA LEVEL PRESSURE                      (HPA             )
TEMS    0  188,    1,    0  ** sfc   SURFACE ABSOLUTE TEMPERATURE            (K               )
TEMP   18   11,  100,    0  **       ABSOLUTE TEMPERATURE                    (K               )
UMRS    0  226,    1,    0  ** sfc   SURFACE RELATIVE HUMIDITY               (NO DIM          )
UMRL   18   52,  100,    0  **       RELATIVE HUMIDITY                       (NO DIM          )
UMES   18   51,  100,    0  **       SPECIFIC HUMIDITY                       (KG/KG           )
AGPL    0   54,  200,    0  ** atm   INST. PRECIPITABLE WATER                (KG/M2           )
TSFC    0  187,    1,    0  ** sfc   SURFACE TEMPERATURE                     (K               )
TP2M    0  128,  105,    2  ** sfc2m TEMPERATURE AT 2-M FROM SURFACE         (K               )
U10M    0  130,  105,   10  ** sfc10m10 METRE U-WIND COMPONENT               (M/S             )
V10M    0  131,  105,   10  ** sfc10m10 METRE V-WIND COMPONENT               (M/S             )
UVMT   18  255,  100,    0  **       TIME MEAN ZONAL WIND (U)                (M/S             )
VVMT   18  241,  100,    0  **       TIME MEAN MERIDIONAL WIND (V)           (M/S             )
PREC    0   61,    1,    0  ** sfc   TOTAL PRECIPITATION                     (KG/M2/DAY       )
NEVE    0   64,    1,    0  ** sfc   SNOWFALL                                (KG/M2/DAY       )
USST    0  193,    1,    0  ** sfc   SURFACE ZONAL WIND STRESS               (PA              )
VSST    0  195,    1,    0  ** sfc   SURFACE MERIDIONAL WIND STRESS          (PA              )
CBNV    0   71,    3,    0  ** cltlayCLOUD COVER                             (0-1             )
ROLE    0  114,    8,    0  ** toa   OUTGOING LONG WAVE AT TOP               (W/M2            )
OCIS    0  209,    1,    0  ** sfc   DOWNWARD SHORT WAVE AT GROUND           (W/M2            )
OCAS    0  111,    1,    0  ** sfc   SHORT WAVE ABSORBED AT GROUND           (W/M2            )
TGSC    0  191,    1,    0  ** sfc   GROUND/SURFACE COVER TEMPERATURE        (K               )
endvars
