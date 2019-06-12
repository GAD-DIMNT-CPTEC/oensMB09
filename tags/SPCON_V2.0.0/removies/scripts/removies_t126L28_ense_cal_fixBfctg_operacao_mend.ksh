#!/bin/ksh 
#
# Desenvolvido por Andrea Cardoso em 07/12/2005, seguindo como exemplo o script de Rodrigo Tozzi
# Calcula o Bias do modelo com base nas previsoes deterministicas anteriores para um dado periodo, sendo fixando vies a partir de um prazo da previsao, p. ex., 096.
# Neste script o Bias caculado eh removido do ensemble medio.
# Para rodar siga o exemplo: removies_t126L28_ense_cal_fixBfctg_operacao.ksh 2005071200 360 024 024 30 096
# Significado dos parametros: DATA=2005071200 é a data inicial da previsao a ser ser corrigida;
#                             FCTEND=360 prazo mais longo (em horas) da previsaoa ser corrigida;
#                             FCTINI=024 prazo mais curto (em horas) da previsaoa ser corrigida;
#                             FCTINT=024 intervalo (em horas) entre as previsões a serem corrigidas;  
#                             DIASDEVIES=30  ???
#                             FCTFIX=096 período a partir do qual o Bias calculado é fixado;


# ***** Opcoes de saida *****
CompBias=1   # ( = 1) Gera um BINARIO (e o CTL) com o Bias, a Variancia do Bias e Raiz do Erro Medio Quadratico (REMQ) com base em previsoes anterioes.
RemoFile=1   # ( = 1) Gera e grava o arquivo BINARIO (e o CTL) da previsao sem Bias, com a Variancia do Bias e Raiz do Erro Medio Quadratico
ResGrib=1    # ( = 1) Transforma o BIN em GRB (RemoFile)
Limpeza=1    # ( = 1) Limpa CTLs, GMPs, GSs e etc...

CALDATE='/gfs/home3/modoper/tools/caldate.3.1.2'

if [ ${1} = "surpresa" ] ; then
  DATAE=`date +'%Y%m%d'`${2}
  DATA=`${CALDATE} ${DATAE} - 24h 'yyyymmddhh'`
else
 DATA=${1}
 FCTEND=${2}
 FCTINI=${3}
 FCTINT=${4}
 DIASDEVIES=${5}
 FCTFIX=${6}
 ANO=`echo $DATA | cut -c 1-4`
 MES=`echo $DATA | cut -c 5-6`
 DIA=`echo $DATA | cut -c 7-8`
 HOR=`echo $DATA | cut -c 9-10`
fi


FCTDAYS=15  #AMM prazo de previsao atual em dias
MAXDAYS=4  #AMM prazo de previsao atual em dias


echo " DATA=${DATA} FCTEND=${FCTEND} FCTINI=${FCTINI} FCTINT=${FCTINT} DIASDEVIES=${DIASDEVIES} FCTFIX=${FCTFIX}"
typeset -Z3 fct FCTEND FCTINI FCTINT FCTFIX
typeset -Z2 HOR

#AMM DIRDAT='/bangu/samfs/modoper/tempo/global/oens/avn/T126L28/GPOS'
#AMM DIRDATENS='/bangu/samfs/modoper/tempo/global/oens/avn/T126L28/ENSMED'
#AMM DIR126='/gfs/home3/io_dop/tempo/global/oenspro/scripts'
#AMM DIRTEMP='/gfs/dk19/io_dop/tempo/global/oenspro/datatemp'
#AMM DIRRESGRB='/gfs/dk19/io_dop/tempo/global/oenspro/deterministico/avaens'${ANO}${MES}${DIA}
#AMM DIRBANG='/bangu/samfs/io_dop/tempo/global/oenspro/deterministico'

#AAF DIRDAT='/gfs/dk10/mod_glob/oens/pos/dataout/T126L28'
#AAF DIRDATENS='/gfs/dk10/mod_glob/oens/ensmed/dataout/T126L28'
#AAF DIRSCR='/gfs/home2/mod_glob/oens/produtos/removies'
#AAF DIRRESGRB='/gfs/dk10/mod_glob/oens/produtos/removies'
#AAF DIRBANG='/rede/nas/modglob/mod_glob/oens/removies/ensmed'
#AAF DIRBANV='/rede/nas/modglob/mod_glob/oens/removies/vies'
#AAF DIRGRDMAP='/usr/local/grads-1.9b3/bin'

. ../../includes/config.sx6

#DIRDAT=${ROPERM}/pos/dataout/T${TRC}L${LV}
DIRDAT=${BANGU}/GPOS
#DIRDATENS=${ROPERM}/ensmed/dataout/T${TRC}L${LV}
#AAF DIRDATENS=/rede/hsm/oens/nmc/T126L28/ENSMED/${ANO}/${MES}/${DIA}
DIRDATENS=/rede/nas/modoper/tempo/global/oens/nmc/T126L28/ENSMED/${ANO}/${MES}/${DIA}
DIRSCR=${OPERM}/removies
DIRRESGRB=${ROPERM}/removies/deterministico/avaens${ANO}${MES}${DIA}
#DSM DIRBANG=${NAS}/removies/ensmed
DIRBANG=$DIR_LOC/removies/ensmed  #DSM - Mudando a escrita para disco local, pois como informado no email do dia 04/Fev/2010, 
                                  #      estava havendo problema no GrADS ao gerar arquivos no NAS.
#DIRBANG=${ROPERM}/removies/ensmed
#DSM DIRBANV=${NAS}/removies/vies
DIRBANV=$DIR_LOC/removies/vies

DIRGRDMAP=/usr/local/grads-1.9b3/bin
GRADS=/usr/local/grads-1.9b4/bin

echo "mkdir -p ${DIRRESGRB}"
mkdir -p ${DIRRESGRB}


PREMEMB='GPOS'
PREFIXA=GPOSAVN
PREFIXP='GPOSAVN'
PREFVIES='126vies'
PREFENSE='GPOSENM'
PREFIXSAI='gposenmrmv'


# ><> ><> ><> ><> ><> ><> ><> ><> ><>

# Gerando o bias diario

# ><> ><> ><> ><> ><> ><> ><> ><> ><>


\rm -Rf ${DIRRESGRB}/ana*
\rm -Rf ${DIRRESGRB}/prev*


###### Gerando os arquivos temporarios com os links dos gribs a serem utilizados  ########## 
############################################################################################
 echo 
 echo "=-=-> Gravando o arquivo de Vies para  ${DATA}, indo ate ${FCTEND}h, com intervalo de ${FCTINT}h"
 echo 

  DATAFIM=${DATA}

  ##gerando um link para os grbs correspondentes as analises que serao utilizadas no calculo do vies
  DIA2=`echo ${DATAFIM} | cut -c 7-8`
  MES2=`echo ${DATAFIM} | cut -c 5-6`
  ANO2=`echo ${DATAFIM} | cut -c 1-4`	
  if ((${DATAFIM} >= 2005091400)); then
  ln -s ${DIRDAT}/${ANO2}/${MES2}/${DIA2}/${PREFIXP}${DATAFIM}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/ana${DATAFIM}.grb
#  ln -s ${DIRDAT}/${PREFIXP}${DATAFIM}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/ana${DATAFIM}.grb
  echo "say \"= ***ANALISE =-> ana${DATAFIM}.grb\"" 
  else
  ln -s ${DIRDAT}/${ANO2}/${MES2}/${DIA2}/${PREFIXA}${DATAFIM}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/ana${DATAFIM}.grb
#  ln -s ${DIRDAT}/${PREFIXA}${DATAFIM}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/ana${DATAFIM}.grb
  echo "say \"= ***ANALISE =-> ana${DATAFIM}.grb\""	
  fi


  #AMM let numdias=1
  #AMM  while ((${numdias} <= ${DIASDEVIES})) ; do

   let fct=$FCTINI
   while ((${fct} <= ${FCTFIX})) ; do
    ##gerando um link para os grbs correspondentes as previsoes que serao utilizadas no calculo do vies 
    DATAINIC=`${CALDATE} ${DATAFIM} - ${fct}h 'yyyymmddhh'`
    DIA1=`echo ${DATAINIC} | cut -c 7-8`
    MES1=`echo ${DATAINIC} | cut -c 5-6`
    ANO1=`echo ${DATAINIC} | cut -c 1-4`
    if ((${DATAINIC} >= 2005091400)); then
     if ((${DATAINIC} != ${DATAFIM})); then
      ln -s ${DIRDAT}/${ANO1}/${MES1}/${DIA1}/${PREFIXP}${DATAINIC}${DATAFIM}P.fct.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
#      ln -s ${DIRDAT}/${PREFIXP}${DATAINIC}${DATAFIM}P.fct.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
      echo "say \"= PREV =-> prev${fct}${DATAFIM}.grb\""
     else
      ln -s ${DIRDAT}/${ANO1}/${MES1}/${DIA1}/${PREFIXP}${DATAINIC}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
#      ln -s ${DIRDAT}/${PREFIXP}${DATAINIC}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
      echo "say \"= PREV =-> prev${fct}${DATAFIM}.grb\""
     fi
    else
     if ((${DATAINIC} != ${DATAFIM})); then
      ln -s ${DIRDAT}/${ANO1}/${MES1}/${DIA1}/${PREFIXA}${DATAINIC}${DATAFIM}P.fct.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
#      ln -s ${DIRDAT}/${PREFIXA}${DATAINIC}${DATAFIM}P.fct.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
      echo "say \"= PREV =-> prev${fct}${DATAFIM}.grb\"" 
     else
      ln -s ${DIRDAT}/${ANO1}/${MES1}/${DIA1}/${PREFIXA}${DATAINIC}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
#      ln -s ${DIRDAT}/${PREFIXA}${DATAINIC}${DATAFIM}P.icn.T126L28.grb ${DIRRESGRB}/prev${fct}${DATAFIM}.grb
      echo "say \"= PREV =-> prev${fct}${DATAFIM}.grb\"" 
     fi
    fi
    let fct=$fct+$FCTINT
   done
   #AMM DATAFIM=`${CALDATE} ${DATAFIM} - 1d 'yyyymmddhh'`
   #AMM let numdias=numdias+1
  #AMM done
        
#AMM DATACTL=`${CALDATE} ${DATAFIM} + 1d 'hhZddmmmyyyy'`
DATACTL=`${CALDATE} ${DATA} + 0d 'hhZddmmmyyyy'`


########### CTL para a abrir as analises e as previsoes indicadas pelos links dos arquivos gribados ############
###################################################################################################################################

#### CTL da analise ####
 cat << EOF1 >  ${DIRRESGRB}/analise.ctl
dset  ^ana${DATA}.grb
*
index ^analise.gmp
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
tdef 1 linear ${DATACTL} 6hr
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
EOF1

 ls -lrt ${DIRRESGRB}/analise.ctl | awk '{print $9" "$10" "$11}'
 ${DIRGRDMAP}/gribmap -i ${DIRRESGRB}/analise.ctl



#### CTL da previsao ####
 let fct=$FCTINI
 while ((${fct} <= ${FCTFIX})) ; do  
# BATCH para rodar o grads em background
  batch="b"

 cat << EOF1 >  ${DIRRESGRB}/previsao${fct}.ctl
dset  ^prev${fct}${DATA}.grb
*
index ^previsao${fct}.gmp
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
tdef 1 linear ${DATACTL} 24hr
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
EOF1

 ls -lrt ${DIRRESGRB}/previsao${fct}.ctl | awk '{print $9" "$10" "$11}'
 ${DIRGRDMAP}/gribmap -i ${DIRRESGRB}/previsao${fct}.ctl
 let fct=$fct+$FCTINT
 done


if (( ${CompBias} == 1 )) ; then

##### Gerando os GSs para o calculo do erro Bias, da Variancia do Bias e da Raiz do Erro Medio Quadratico (REMQ) ########
#########################################################################################################################
## Um GS para cada prazo de previsao, ate o periodo em que fixa o Bias ##
 let fct=$FCTINI
 while ((${fct} <= ${FCTFIX})) ; do  
 rm -f ${DIRSCR}/BiasFile${fct}h.gs
 echo "'open ${DIRRESGRB}/analise.ctl' "  >> ${DIRSCR}/BiasFile${fct}h.gs
 echo "'open ${DIRRESGRB}/previsao${fct}.ctl' "  >> ${DIRSCR}/BiasFile${fct}h.gs
 echo "abre = " ${DIRRESGRB}"/analise.ctl"
 echo "abre = " ${DIRRESGRB}"/previsao"${fct}".ctl"
 cat << EOF2 >> ${DIRSCR}/BiasFile${fct}h.gs
 'set x 1 384'
 'set y 1 192'
* Variaveis do 126:
* SUPERFICIE:topo lsmk pslc uves vves psnm tems umrs agpl tsfc prec prcv cbnv role 
* ALTITUDE: uvel vvel omeg divg vort fcor potv zgeo temp umrl umes
 varsfc='pslc uves vves psnm tems' 
 varprs='uvel vvel zgeo temp umes'
 niveis='850 500 250' 
 totsfc=5
 totprs=5
 totniv=3
 'set gxout fwrite'
 'set fwrite ${DIRRESGRB}/BiasFile${fct}h${DATA}'
*  do vies e variancia
* *******-=-=-=-=-=-=-=-=-=-> VARIAVEIS DE SUPERFICIE
  nvar=1
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totsfc)
   varnm=subwrd(varsfc,nvar)
   'set x 1 384'
   'set y 1 192'
* =-=-=-=-=-=-=-=- BIAS -=-=-=-=-=-=-=-=
*AMM    'define b'varnm'=ave('varnm'.2-'varnm'.1,t=1,t=${DIASDEVIES})'
   'define b'varnm'='varnm'.2-'varnm'.1'
   'd '"b"varnm'' 
* =-=-=-=-=-=-=-=- VARIANCIA DO ERRO EM RELACAO AO BAIAS -=-=-=-=-=-=-=-= 
*AMM    'define v'varnm'=sqrt(ave((pow('varnm'.2-'varnm'.1-b'varnm',2)),t=1,t=${DIASDEVIES}))'
*AMM    'd '"v"varnm''
   'undefine b'varnm'' 
*AMM    'undefine v'varnm''
* =-=-=-=-=-=-=-=- RAIZ DO ERRO MEDIO QUADRATICO -=-=-=-=-=-=-=-= 
*AMM    'define e'varnm'=sqrt(ave((pow('varnm'.2-'varnm'.1,2)),t=1,t=${DIASDEVIES}))'
*AMM    'd '"e"varnm''
*AMM    'undefine e'varnm'' 
    nvar=nvar+1 
  endwhile

* ******-=-=-=-=-=-=-=-=-=-> VARIAVEIS EM DIFERENTES NIVEIS DE PRESSAO
  nvar=1 
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totprs)
   varnm=subwrd(varprs,nvar)
* =-=-=-=-=-=-=-=- BIAS -=-=-=-=-=-=-=-=
   nniv=1
   while(nniv<=totniv)
    nivv=subwrd(niveis,nniv)
   'set lev 'nivv
   'set x 1 384'
   'set y 1 192'
*AMM    'define b'varnm%nniv'=ave('varnm'.2-'varnm'.1,t=1,t=${DIASDEVIES})'
   'define b'varnm%nniv'='varnm'.2-'varnm'.1'
   'd '"b"varnm%nniv''
   nniv=nniv+1
   endwhile
* =-=-=-=-=-=-=-=- VARIANCIA DO ERRO EM RELACAO AO BAIAS -=-=-=-=-=-=-=-=
*AMM    nniv=1
*AMM    while(nniv<=totniv)
*AMM     nivv=subwrd(niveis,nniv)
*AMM    'set lev 'nivv
*AMM    'set x 1 384'
*AMM    'set y 1 192'
*AMM    'define v'varnm%nniv'=sqrt(ave((pow('varnm'.2-'varnm'.1-b'varnm%nniv',2)),t=1,t=${DIASDEVIES}))'
*AMM     'd '"v"varnm%nniv''
*AMM     'undefine v'varnm%nniv
*AMM     'undefine b'varnm%nniv
*AMM     nniv=nniv+1
*AMM    endwhile
* =-=-=-=-=-=-=-=- RAIZ DO ERRO MEDIO QUADRATICO -=-=-=-=-=-=-=-=   
*AMM    nniv=1
*AMM    while(nniv<=totniv)
*AMM     nivv=subwrd(niveis,nniv)
*AMM    'set lev 'nivv
*AMM    'set x 1 384'
*AMM    'set y 1 192'
*AMM    'define e'varnm%nniv'=sqrt(ave((pow('varnm'.2-'varnm'.1,2)),t=1,t=${DIASDEVIES}))'
*AMM    'd '"e"varnm%nniv''
*AMM    'undefine e'varnm%nniv  
*AMM     nniv=nniv+1
*AMM    endwhile
  
   nvar=nvar+1
  endwhile

 'disable fwrite'
 'set gxout contour'
 'quit'
EOF2

 ls -lrt ${DIRSCR}/BiasFile${fct}h.gs | awk '{print $9" "$10" "$11}'
 ${GRADS}/gradsc -${batch}lc "run ${DIRSCR}/BiasFile${fct}h.gs"

 let fct=$fct+$FCTINT
 done
 
 
########### CTL para ler os binarios do Bias, Variancia do Baias e REMQ, para cada prazo de previsao ############
#################################################################################################################

 let fct=$FCTINI
 while ((${fct} <= ${FCTFIX})) ; do 
 cat << EOF1 >  ${DIRRESGRB}/BiasFile${fct}h${DATA}.ctl
dset  ^BiasFile${fct}h${DATA}
undef 9.999E+20
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
zdef 3 levels 
850 500 250 
tdef 1 linear ${DATACTL} 6hr
vars 10
bpslc  0  99  SURFACE PRESSURE [hPa]
buves  0  99  SURFACE ZONAL WIND (U) [m/s]
bvves  0  99  SURFACE MERIDIONAL WIND (V) [m/s]
bpsnm  0  99  SEA LEVEL PRESSURE [hPa]
btems  0  99  SURFACE ABSOLUTE TEMPERATURE [K]
buvel  3  99  ZONAL WIND (U) [m/s]
bvvel  3  99  MERIDIONAL WIND (V) [m/s]
bzgeo  3  99  GEOPOTENTIAL HEIGHT [gpm]
btemp  3  99  ABSOLUTE TEMPERATURE [K]
bumes  3  99  SPECIFIC HUMIDITY [kg/kg]
endvars
EOF1

 ls -lrt ${DIRRESGRB}/BiasFile${fct}h${DATA}.ctl | awk '{print $9" "$10" "$11}'
    
 let fct=$fct+$FCTINT
 done
 
fi

mkdir  ${DIRBANV} ${DIRBANV}/${ANO} ${DIRBANV}/${ANO}/${MES} ${DIRBANV}/${ANO}/${MES}/${DIA}
mv -f ${DIRRESGRB}/BiasFile*h${DATA}*  ${DIRBANV}/${ANO}/${MES}/${DIA}

rm -f ${DIRSCR}/BiasFile*
rm -f ${DIRRESGRB}/ana* ${DIRRESGRB}/prev* 


# ><> ><> ><> ><> ><> ><> ><> ><> ><>

# Gerando o bias medio dos ultimos n dias a ser removido das previsoes mais recentes

# ><> ><> ><> ><> ><> ><> ><> ><> ><>


rm -f ${DIRRESGRB}/bias*

### Gerando os arquivos temporarios com os links dos arquivos de bias a serem utilizados ###
############################################################################################

  ##gerando um link para os bias correspondentes 

  DATAFIM=${DATA}
  let numdias=1
  while ((${numdias} <= ${DIASDEVIES})) ; do
   DIA1=`echo ${DATAFIM} | cut -c 7-8`
   MES1=`echo ${DATAFIM} | cut -c 5-6`
   ANO1=`echo ${DATAFIM} | cut -c 1-4`
   let fct=$FCTINI
   while ((${fct} <= ${FCTFIX})) ; do
    ##gerando um link para os bias correspondentes as previsoes que serao utilizadas no calculo do vies 
    ln -s ${DIRBANV}/${ANO1}/${MES1}/${DIA1}/BiasFile${fct}h${DATAFIM} ${DIRRESGRB}/bias${fct}h${DATAFIM}
    echo "say \"= BIAS =-> bias${fct}h${DATAFIM}\"" 
    let fct=$fct+$FCTINT
   done
   DATAFIM=`${CALDATE} ${DATAFIM} - 1d 'yyyymmddhh'`
   let numdias=numdias+1
  done
        
DATACTL=`${CALDATE} ${DATAFIM} + 1d 'hhZddmmmyyyy'`


########### CTL (options template) para a abrir os bias indicadas pelos links ############
###################################################################################################################################

 let fct=$FCTINI
 while ((${fct} <= ${FCTFIX})) ; do  
cat << EOF1 >  ${DIRRESGRB}/bias${fct}.ctl
dset  ^bias${fct}h%y4%m2%d2%h2
undef 9.999E+20
options template
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
zdef 3 levels
850 500 250
tdef ${DIASDEVIES} linear ${DATACTL} 24hr
vars 10
bpslc  0  99  SURFACE PRESSURE [hPa]
buves  0  99  SURFACE ZONAL WIND (U) [m/s]
bvves  0  99  SURFACE MERIDIONAL WIND (V) [m/s]
bpsnm  0  99  SEA LEVEL PRESSURE [hPa]
btems  0  99  SURFACE ABSOLUTE TEMPERATURE [K]
buvel  3  99  ZONAL WIND (U) [m/s]
bvvel  3  99  MERIDIONAL WIND (V) [m/s]
bzgeo  3  99  GEOPOTENTIAL HEIGHT [gpm]
btemp  3  99  ABSOLUTE TEMPERATURE [K]
bumes  3  99  SPECIFIC HUMIDITY [kg/kg]
endvars
EOF1

 ls -lrt ${DIRRESGRB}/bias${fct}.ctl | awk '{print $9" "$10" "$11}'
 let fct=$fct+$FCTINT
 done


##### Calculando o vies medio dos ultimos n dias ########
#########################################################################################################################
## Um GS para cada prazo de previsao, ate o periodo em que fixa o Bias ##
 let fct=$FCTINI
 while ((${fct} <= ${FCTFIX})) ; do  
 rm -f ${DIRSCR}/BiasMean${fct}h.gs
 echo "'open ${DIRRESGRB}/bias${fct}.ctl' "  >> ${DIRSCR}/BiasMean${fct}h.gs
 echo "abre = " ${DIRRESGRB}"/bias"${fct}".ctl"
 cat << EOF2 >> ${DIRSCR}/BiasMean${fct}h.gs
 'set x 1 384'
 'set y 1 192'
* Variaveis do 126:
* SUPERFICIE:topo lsmk pslc uves vves psnm tems umrs agpl tsfc prec prcv cbnv role 
* ALTITUDE: uvel vvel omeg divg vort fcor potv zgeo temp umrl umes 
 varsfc='bpslc buves bvves bpsnm btems' 
 varprs='buvel bvvel bzgeo btemp bumes'
 niveis='850 500 250' 
 totsfc=5
 totprs=5
 totniv=3
 'set gxout fwrite'
 'set fwrite ${DIRRESGRB}/BiasMean${fct}h'
*  do vies e variancia
* *******-=-=-=-=-=-=-=-=-=-> VARIAVEIS DE SUPERFICIE
  nvar=1
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totsfc)
   varnm=subwrd(varsfc,nvar)
   'set x 1 384'
   'set y 1 192'
* =-=-=-=-=-=-=-=- BIAS -=-=-=-=-=-=-=-=
   'define b'varnm'=ave('varnm'.1,t=1,t=${DIASDEVIES})'
   'd '"b"varnm'' 
   'undefine b'varnm'' 
    nvar=nvar+1 
  endwhile


* ******-=-=-=-=-=-=-=-=-=-> VARIAVEIS EM DIFERENTES NIVEIS DE PRESSAO
  nvar=1 
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totprs)
   varnm=subwrd(varprs,nvar)
* =-=-=-=-=-=-=-=- BIAS -=-=-=-=-=-=-=-=
   nniv=1
   while(nniv<=totniv)
    nivv=subwrd(niveis,nniv)
   'set lev 'nivv
   'set x 1 384'
   'set y 1 192'
   'define b'varnm%nniv'=ave('varnm'.1,t=1,t=${DIASDEVIES})'
   'd '"b"varnm%nniv''
   'undefine b'varnm%nniv
    nniv=nniv+1
   endwhile
   nvar=nvar+1
  endwhile

 'disable fwrite'
 'set gxout contour'
 'quit'
EOF2

 ls -lrt ${DIRSCR}/BiasMean${fct}h.gs | awk '{print $9" "$10" "$11}'
 ${GRADS}/gradsc -${batch}lc "run ${DIRSCR}/BiasMean${fct}h.gs"
     
 let fct=$fct+$FCTINT
 done
 
 
########### CTL para ler os bias medios ############
#################################################################################################################

 DATACTL=`${CALDATE} ${DATA} + 0d 'hhZddmmmyyyy'`
 let fct=$FCTINI
 while ((${fct} <= ${FCTFIX})) ; do 
 cat << EOF1 >  ${DIRRESGRB}/BiasMean${fct}h.ctl
dset  ^BiasMean${fct}h
undef 9.999E+20
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
zdef 3 levels
850 500 250
tdef 1 linear ${DATACTL} 24hr
vars 10
bpslc  0  99  SURFACE PRESSURE [hPa]
buves  0  99  SURFACE ZONAL WIND (U) [m/s]
bvves  0  99  SURFACE MERIDIONAL WIND (V) [m/s]
bpsnm  0  99  SEA LEVEL PRESSURE [hPa]
btems  0  99  SURFACE ABSOLUTE TEMPERATURE [K]
buvel  3  99  ZONAL WIND (U) [m/s]
bvvel  3  99  MERIDIONAL WIND (V) [m/s]
bzgeo  3  99  GEOPOTENTIAL HEIGHT [gpm]
btemp  3  99  ABSOLUTE TEMPERATURE [K]
bumes  3  99  SPECIFIC HUMIDITY [kg/kg]
endvars
EOF1

 ls -lrt ${DIRRESGRB}/BiasMean${fct}h.ctl | awk '{print $9" "$10" "$11}'
    
 let fct=$fct+$FCTINT
 done


# ><> ><> ><> ><> ><> ><> ><> ><> ><> 

# Fazendo a remocao do bias medio das previsoes mais recentes

# ><> ><> ><> ><> ><> ><> ><> ><> ><> 


if (( ${RemoFile} == 1 )) ; then
rm -f ${DIRRESGRB}/RemoFile*
rm -f ${DIRSCR}/RemoFile*
#################### Arquivo BINARIO da previsao sem vies ###########################   
#####################################################################################
 DATAINIPREV=${DATA}
 DIAI=`echo ${DATAINIPREV} | cut -c 7-8`
 MESI=`echo ${DATAINIPREV} | cut -c 5-6`
 ANOI=`echo ${DATAINIPREV} | cut -c 1-4`  
 let fct=$FCTINI
 while ((${fct} <= ${FCTEND})) ; do
# BATCH para rodar o grads em background
 batch="b"
 DATAFIMPREV=`${CALDATE} ${DATAINIPREV} + ${fct}h 'yyyymmddhh'`

 if (( ${fct} <= ${FCTFIX} )) ; then
 echo "'open ${DIRRESGRB}/BiasMean${fct}h.ctl' "  >> ${DIRSCR}/RemoFile${fct}h.gs
 echo "abre = " ${DIRRESGRB}/"BiasMean${fct}h.ctl"
 else
 echo "'open ${DIRRESGRB}/BiasMean${FCTFIX}h.ctl' "  >> ${DIRSCR}/RemoFile${fct}h.gs
 echo "abre = " ${DIRRESGRB}/"BiasMean${FCTFIX}h.ctl"
 fi

cat << EOF2 >> ${DIRSCR}/RemoFile${fct}h.gs 
varsfcout='psnm tems'
varprsout='umes temp zgeo uvel vvel'
niveisout='850 500 250'
totsfcout=2
totprsout=5
totnivout=3
 'set x 1 384'
 'set y 1 192'
**** variaveis da superficie
  nvar=1
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totsfcout)
  'set x 1 384'
  'set y 1 192'
   varnm=subwrd(varsfcout,nvar)
   'define pb'varnm'=b'varnm'.1'
   nvar=nvar+1 
  endwhile

**** variarveis em diferentes niveis de pressao 
  nvar=1 
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totprsout)
   varnm=subwrd(varprsout,nvar)
   nniv=1
   while(nniv<=totnivout)
    nivv=subwrd(niveisout,nniv)
   'set lev 'nivv
   'set x 1 384'
   'set y 1 192'
   'define pb'varnm%nniv'=b'varnm'.1'
   nniv=nniv+1
   endwhile
   nvar=nvar+1
  endwhile
 'close 1'
EOF2

# Abrido o arquivo da previsao do Ensemble medio para realizar a correcao
PREFIX=${PREFENSE}
if ((${DATAINIPREV} != ${DATAFIMPREV})); then
#echo "'open ${DIRDATENS}/${ANOI}/${MESI}/${DIAI}/${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28.ctl' " >> ${DIRSCR}/RemoFile${fct}h.gs
echo "'open ${DIRDATENS}/${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28.ctl' " >> ${DIRSCR}/RemoFile${fct}h.gs
echo "'set gxout fwrite' " >> ${DIRSCR}/RemoFile${fct}h.gs
echo "'set fwrite ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28' " >> ${DIRSCR}/RemoFile${fct}h.gs
else
#echo "'open ${DIRDATENS}/${ANOI}/${MESI}/${DIAI}/${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28.ctl' " >> ${DIRSCR}/RemoFile${fct}h.gs
echo "'open ${DIRDATENS}/${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28.ctl' " >> ${DIRSCR}/RemoFile${fct}h.gs
echo "'set gxout fwrite' " >> ${DIRSCR}/RemoFile${fct}h.gs
echo "'set fwrite ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28' " >> ${DIRSCR}/RemoFile${fct}h.gs
fi

cat << EOF2 >> ${DIRSCR}/RemoFile${fct}h.gs 
*AMM  'set gxout fwrite'
*AMM  'set fwrite ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28'
* *******-=-=-=-=-=-=-=-=-=-> removendo o vies das variaveis selecionadas e para a area determinada
 'set x 1 384'
 'set y 1 192'
**** variaveis da superficie
  nvar=1
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totsfcout)
  'set x 1 384'
  'set y 1 192'
   varnm=subwrd(varsfcout,nvar)
   'define aux='varnm'.1-pb'varnm
   'd aux'
   'undefine aux'
   'undefine pb'varnm
   nvar=nvar+1 
  endwhile

**** variarveis em diferentes niveis de pressao 
  nvar=1 
  'q file'
  dimen=sublin(result,5)
  while(nvar<=totprsout)
   varnm=subwrd(varprsout,nvar)
   nniv=1
   while(nniv<=totnivout)
    nivv=subwrd(niveisout,nniv)
   'set lev 'nivv
   'set x 1 384'
   'set y 1 192'
   'define aux='varnm'.1-pb'varnm%nniv
   'd aux'
   'undefine aux'
   'undefine pb'varnm%nniv
   nniv=nniv+1
   endwhile
   nvar=nvar+1
  endwhile
 'close 1'
 'disable fwrite'
 'set gxout contour'
 'quit'
EOF2

 ls -lrt ${DIRSCR}/RemoFile${fct}h.gs | awk '{print $9" "$10" "$11}'
 
 echo "LALALALALALA"
 echo "${GRADS}/gradsnc -${batch}lc run ${DIRSCR}/RemoFile${fct}h.gs"
 ${GRADS}/gradsnc -${batch}lc "run ${DIRSCR}/RemoFile${fct}h.gs"

 let fct=$fct+$FCTINT
 done
 

########### CTL para ler a previsao corrigida (sem vies) ############
#####################################################################
 let fct=$FCTINI
 while ((${fct} <= ${FCTEND})) ; do 
 DATAFIMPREV=`${CALDATE} ${DATAINIPREV} + ${fct}h 'yyyymmddhh'`
 DATACTLPREV=`${CALDATE} ${DATAFIMPREV} + 0d 'hhZddmmmyyyy'`

if ((${DATAINIPREV} != ${DATAFIMPREV})); then

cat << EOF1 >  ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28.ctl
dset  ^${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.fct.t126l28
undef 9.999E+20
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
zdef 3 levels 
850 500 250 
tdef 1 linear ${DATACTLPREV} 24hr
vars 7
psnm  0  99  SEA LEVEL PRESSURE [hPa]
tems  0  99  SURFACE ABSOLUTE TEMPERATURE [K]
umes  3  99  SURFACE SPEC HUMIDITY [no Dim]
temp  3  99  ABSOLUTE TEMPERATURE [K]
zgeo  3  99  GEOPOTENTIAL HEIGHT [gpm]
uvel  3  99  ZONAL WIND (U) [m/s]
vvel  3  99  MERIDIONAL WIND (V) [m/s]
endvars
EOF1

ls -lrt ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28.ctl | awk '{print $9" "$10" "$11}'

else

cat << EOF1 >  ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28.ctl
dset  ^${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.icn.t126l28
undef 9.999E+20
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
zdef 3 levels 
850 500 250 
tdef 1 linear ${DATACTLPREV} 24hr
vars 7
psnm  0  99  SEA LEVEL PRESSURE [hPa]
tems  0  99  SURFACE ABSOLUTE TEMPERATURE [K]
umes  3  99  SURFACE SPEC HUMIDITY [no Dim]
temp  3  99  ABSOLUTE TEMPERATURE [K]
zgeo  3  99  GEOPOTENTIAL HEIGHT [gpm]
uvel  3  99  ZONAL WIND (U) [m/s]
vvel  3  99  MERIDIONAL WIND (V) [m/s]
endvars
EOF1

ls -lrt ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28.ctl | awk '{print $9" "$10" "$11}'

fi

  
 let fct=$fct+$FCTINT
 done

 fi


# ><> ><> ><> ><> ><> ><> ><> ><> ><>  

# Fazendo a gribagem das previsoes corrigidas e enviando para o nas

# ><> ><> ><> ><> ><> ><> ><> ><> ><>  


 if (( ${ResGrib} == 1 )) ; then
#Convertendo o binario Removies em grib
 echo 
 echo "=-=-> Inicia gribagem de tudo..."
 echo
DATAINIPREV=${DATA}
let fct=$FCTINI
 while ((${fct} <= ${FCTEND})) ; do

  DATAFIMPREV=`${CALDATE} ${DATAINIPREV} + ${fct}h 'yyyymmddhh'`

  if ((${DATAINIPREV} != ${DATAFIMPREV})); then
#AMM     ${DIRGRDMAP}/lats4d -i ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28.ctl -o ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.fct.t126l28  -format "grads_grib" -ftype ctl -v -q
    mv ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28.ctl  ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.fct.t126l28.ctl  
    mv ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.fct.T126L28      ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.fct.t126l28  
  else
#AMM     ${DIRGRDMAP}/lats4d -i ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28.ctl -o ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.icn.t126l28  -format "grads_grib" -ftype ctl -v -q
    mv ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28.ctl  ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.icn.t126l28.ctl  
    mv ${DIRRESGRB}/RemoFile${PREFIX}${DATAINIPREV}${DATAFIMPREV}P.icn.T126L28      ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}${DATAFIMPREV}p.icn.t126l28  
  fi

  let fct=$fct+$FCTINT
 done

 fi

mkdir  ${DIRBANG} ${DIRBANG}/${ANO} ${DIRBANG}/${ANO}/${MES} ${DIRBANG}/${ANO}/${MES}/${DIA}
mv -f ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}* ${DIRBANG}/${ANO}/${MES}/${DIA}


# ><> ><> ><> ><> ><> ><> ><> ><> ><>

# Fazendo a remocao de arquivos temporarios

# ><> ><> ><> ><> ><> ><> ><> ><> ><>  

 if (( ${Limpeza} == 1 )) ; then
#Gerando o CTL do grib Removies
 echo 
 echo "=-=-> Limpando os lixos e etc..."
 echo
 rm -f ${DIRSCR}/RemoFile* ${DIRSCR}/BiasMean*
 rm -f ${DIRRESGRB}/RemoFile* ${DIRRESGRB}/BiasMean*
 rm -f ${DIRRESGRB}/ana* ${DIRRESGRB}/prev*  ${DIRRESGRB}/bias* 
 rm -f ${DIRRESGRB}/${PREFIXSAI}${DATAINIPREV}* 
  
 fi


exit 0

