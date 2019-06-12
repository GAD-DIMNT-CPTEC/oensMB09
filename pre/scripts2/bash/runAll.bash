#!/bin/bash
# 
# script to run CPTEC Global Model on PC Clusters under MPI Scali
# and Sun Grid Engine without OpenMP
#575) MTO=575 ; MTC=575 ; ITR=10 ; CUT=12 ; IRO=1728; JRO=864 ;;
#382) MTO=382 ; MTC=382 ; ITR=10 ; CUT=12 ; IRO=1152; JRO=576 ;;
# = 299 : Dl= 0.400000000 deg : Dx= 44.80 km : Imax= 900 : Jmax= 450 : Quadratic


# Set  Res for Chopping
typeset -x prefi
export RESIN=254
export KMIN=64
export RESOUT=62
export KMOUT=28
export SetLinear=FALSE
export RESO=62
export IM=192
export JM=96
if [[ "$SetLinear" = "TRUE" ]]; then
if [ ${RESOUT} -lt 10000 ]; then
export TRUNC=TL${RESOUT}
if [ ${RESOUT} -lt 1000 ]; then
export TRUNC=TL0${RESOUT}
if [ ${RESOUT} -lt 100 ]; then
export TRUNC=TL00${RESOUT}
fi
fi
fi
else
if [ ${RESOUT} -lt 10000 ]; then
export TRUNC=TQ${RESOUT}
if [ ${RESOUT} -lt 1000 ]; then
export TRUNC=TQ0${RESOUT}
if [ ${RESOUT} -lt 100 ]; then
export TRUNC=TQ00${RESOUT}
fi
fi
fi
fi
if [ ${JM} -lt 10000 ]; then
export prefix=0${JM}
if [ ${JM} -lt 1000 ]; then
export prefix=00${JM}
if [ ${JM} -lt 100 ]; then
export prefix=000${JM}
if [ ${JM} -lt 10 ]; then
export prefix=0000${JM}
fi
fi
fi
fi
#set run date
export DATA=2004032600

export dirhome=/home/pkubota/mcgacptec/pre
export dirdata=/mpp/pkubota/mcgacptec
export dirgrads=/usr/local/grads

# Machine options: SX6; Linux
export MAQUI=SX6


#
# Set <pre-scripts>=1 to execute or <pre-scripts>=0 not execute
# 
TopoWaterPercNavy=0
TopoWaterPercGT30=0 
LandSeaMask=1
Chopping=1
VarTopo=1
TopoSpectral=1
VegetationMaskSSiB=1
VegetationMask=1
VegetationAlbedoSSiB=1
DeepSoilTemperatureClima=1
DeepSoilTemperature=1
RoughnessLengthClima=1
RoughnessLength=1
SoilMoistureClima=1
SoilMoisture=1
AlbedoClima=1
Albedo=1
SnowClima=1
SSTClima=1
SSTWeeklyNCEP=1 
SSTWeekly=1

if [ $TopoWaterPercNavy -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_TopoWaterPercNavy.bash 1 1 TopoNavy hold
   ${dirhome}/scripts/run_TopoWaterPercNavy.bash  1 1 TopoNavy hold
   file_out=${dirdata}/pre/dataout/TopoNavy.dat
   file_out2=${dirdata}/pre/dataout/WaterNavy.dat
   sleep 15
   if test ! -s ${file_out} ; then 
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else if test ! -s ${file_out2} ; then
      echo "Problema!!! Nao foi gerado ${file_out2} "
      exit 1
   else
      echo
      echo "Fim do TopoWaterPercNavy"
      echo
   fi
   fi
fi

if [ $TopoWaterPercGT30 -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_TopoWaterPercGT30.bash  1 1 GT30 hold
   ${dirhome}/scripts/run_TopoWaterPercGT30.bash 1 1 GT30 hold
   file_out=${dirdata}/pre/dataout/TopoGT30.dat
   file_out2=${dirdata}/pre/dataout/WaterGT30.dat
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else if test ! -s ${file_out2} ; then
      echo "Problema!!! Nao foi gerado ${file_out2} "
      exit 1
   else
      echo
      echo "Fim do TopoWaterPercGT30"
      echo
   fi
   fi
fi

if [ $LandSeaMask -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_LandSeaMask.bash  1 1 SeaMask hold 
   ${dirhome}/scripts/run_LandSeaMask.bash  1 1 SeaMask hold
   file_out=${dirdata}/pre/dataout/LandSeaMaskNavy.G${prefix}.dat
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do LandSeaMask"
      echo
   fi
fi

if [ $Chopping -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_Chopping.bash  1 1 Chopping hold
   ${dirhome}/scripts/run_Chopping.bash  1 1 Chopping hold
   file_out=${dirdata}/model/datain/GANLNMC${DATA}S.unf.${TRUNC}L0${KMOUT}
  # cp -f  ${dirdata}/model/datain/GANLNMC${DATA}S.unf.TL0382L064 ${dirdata}/model/datain/GANLNMC${DATA}S.unf.TQ0382L064
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do Chopping"
      echo
   fi
fi 
if [ $VarTopo -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_VarTopo.bash  1 1 VarTop hold
   ${dirhome}/scripts/run_VarTopo.bash  1 1 VarTop  hold
   file_out=${dirdata}/pre/dataout/Topography.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do VarTopo"
      echo
   fi
fi

if [ $TopoSpectral -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_TopoSpectral.bash   1 1 TopoSpectra hold
   ${dirhome}/scripts/run_TopoSpectral.bash    1 1 TopoSpectra hold
   file_out=${dirdata}/model/datain/TopoVariance.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do TopoSpectral"
      echo
   fi
fi 

if [ $VegetationMaskSSiB -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_VegetationMaskSSiB.bash 1 1 VegMasSSiB hold
   ${dirhome}/scripts/run_VegetationMaskSSiB.bash  1 1 VegMasSSiB hold
   file_out=${dirdata}/pre/dataout/VegetationMaskClima.dat
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do VegetationMaskSSiB"
      echo
   fi
fi 

if [ $VegetationMask -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_VegetationMask.bash  1 1 VegetationMask  hold
   ${dirhome}/scripts/run_VegetationMask.bash   1 1 VegetationMask  hold
   file_out=${dirdata}/pre/dataout/VegetationMask.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do VegetationMask"
      echo
   fi
fi 

if [ $VegetationAlbedoSSiB -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_VegetationAlbedoSSiB.bash  1 1 VegAlbSSiB  hold
   ${dirhome}/scripts/run_VegetationAlbedoSSiB.bash   1 1 VegAlbSSiB  hold
   file_out=${dirdata}/model/datain/VegetationMask.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do VegetationAlbedoSSiB"
      echo
   fi
fi 

if [ $DeepSoilTemperatureClima -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_DeepSoilTemperatureClima.bash 1 1 DeepSoilTemClim hold
   ${dirhome}/scripts/run_DeepSoilTemperatureClima.bash 1 1 DeepSoilTemClim hold
   file_out=${dirdata}/pre/dataout/DeepSoilTemperatureClima.dat
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do DeepSoilTemperatureClima"
      echo
   fi
fi 

if [ $DeepSoilTemperature -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_DeepSoilTemperature.bash  1 1 DeepSoilTemp hold
   ${dirhome}/scripts/run_DeepSoilTemperature.bash 1 1 DeepSoilTemp hold
   file_out=${dirdata}/model/datain/DeepSoilTemperature.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do DeepSoilTemperature"
      echo
   fi
fi 

if [ $RoughnessLengthClima -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_RoughnessLengthClima.bash 1 1 RouLenClm hold
   ${dirhome}/scripts/run_RoughnessLengthClima.bash 1 1 RouLenClm hold 
   file_out=${dirdata}/pre/dataout/RoughnessLengthClima.dat
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do RoughnessLengthClima"
      echo
   fi
fi 

if [ $RoughnessLength -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_RoughnessLength.bash  1 1 RougLeng hold
   ${dirhome}/scripts/run_RoughnessLength.bash 1 1 RougLeng hold
   file_out=${dirdata}/model/datain/RoughnessLength.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do RoughnessLength"
      echo
   fi
fi 

if [ $SoilMoistureClima -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_SoilMoistureClima.bash  1 1 SoilMoisClm hold
   ${dirhome}/scripts/run_SoilMoistureClima.bash 1 1 SoilMoisClm hold
   file_out=${dirdata}/pre/dataout/SoilMoistureClima.dat
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do SoilMoistureClima"
      echo
   fi
fi 

if [ $SoilMoisture -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_SoilMoisture.bash  1 1 SoilMoist hold
   ${dirhome}/scripts/run_SoilMoisture.bash 1 1 SoilMoist hold
   file_out=${dirdata}/model/datain/SoilMoisture.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do SoilMoisture"
      echo
   fi
fi 

if [ $AlbedoClima -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_AlbedoClima.bash 1 1 AlbClm hold
   ${dirhome}/scripts/run_AlbedoClima.bash 1 1 AlbClm hold
   file_out=${dirdata}/pre/dataout/AlbedoClima.dat
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do AlbedoClima"
      echo
   fi
fi 

if [ $Albedo -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_Albedo.bash 1 1 Alb hold
   ${dirhome}/scripts/run_Albedo.bash 1 1 Alb hold
   file_out=${dirdata}/pre/dataout/Albedo.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do Albedo"
      echo
   fi
fi 

if [ $SnowClima -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_SnowClima.bash  1 1 SnowClm hold 
   ${dirhome}/scripts/run_SnowClima.bash 1 1 SnowClm hold 
   file_out=${dirdata}/model/datain/Snow${DATA}S.unf.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do SnowClima"
      echo
   fi
fi 

if [ $SSTClima -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_SSTClima.bash   1 1 SSTClim hold
   ${dirhome}/scripts/run_SSTClima.bash  1 1 SSTClim hold
   datt=`echo ${DATA} |cut -c 1-8`
   file_out=${dirdata}/model/datain/SSTClima$datt.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do SSTClima"
      echo
   fi
fi 

if [ $SSTWeeklyNCEP -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_SSTWeeklyNCEP.bash 1 1 SSTWeeklyNCEP hold
   ${dirhome}/scripts/run_SSTWeeklyNCEP.bash 1 1 SSTWeeklyNCEP hold
   datt=`echo ${DATA} |cut -c 1-8`
   file_out=${dirdata}/pre/dataout/SSTWeekly.$datt
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else

      echo
      echo "Fim do SSTWeeklyNCEP"
      echo
   fi
fi 

if [ $SSTWeekly -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_SSTWeekly.bash  1 1 SSTWeekly hold
   ${dirhome}/scripts/run_SSTWeekly.bash 1 1 SSTWeekly hold
   datt=`echo ${DATA} |cut -c 1-8`
   file_out=${dirdata}/model/datain/SSTWeekly$datt.G${prefix}
   sleep 15
   if test ! -s ${file_out} ; then
      echo "Problema!!! Nao foi gerado ${file_out} "
      exit 1
   else
      echo
      echo "Fim do SSTWeekly"
      echo
   fi
fi 
