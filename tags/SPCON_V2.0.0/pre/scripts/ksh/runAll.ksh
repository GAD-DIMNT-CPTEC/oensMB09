#! /bin/ksh
#
#  $Author: tomita $
#  $Date: 2007/08/01 20:09:58 $
#  $Revision: 1.1.1.1 $
#

set -x
export dirhome=/gfs/dk12/pkubota/mcga-1.0.0/pre
export dirdata=/gfs/dk12/pkubota/mcga-1.0.0
export dirgrads=/usr/local/grads

# Mend=   15 : Dl= 7.500000000 deg : Dx= 840.00 km : Imax=   48 : Jmax=   24 : Quadratic
# Mend=   21 : Dl= 5.625000000 deg : Dx= 630.00 km : Imax=   64 : Jmax=   32 : Quadratic
# Mend=   31 : Dl= 3.750000000 deg : Dx= 420.00 km : Imax=   96 : Jmax=   48 : Quadratic
# Mend=   42 : Dl= 2.812500000 deg : Dx= 315.00 km : Imax=  128 : Jmax=   64 : Quadratic
# Mend=   62 : Dl= 1.875000000 deg : Dx= 210.00 km : Imax=  192 : Jmax=   96 : Quadratic
# Mend=  106 : Dl= 1.125000000 deg : Dx= 126.00 km : Imax=  320 : Jmax=  160 : Quadratic
# Mend=  126 : Dl= 0.937500000 deg : Dx= 105.00 km : Imax=  384 : Jmax=  192 : Quadratic
# Mend=  159 : Dl= 0.750000000 deg : Dx=  84.00 km : Imax=  480 : Jmax=  240 : Quadratic
# Mend=  170 : Dl= 0.703125000 deg : Dx=  78.75 km : Imax=  512 : Jmax=  256 : Quadratic
# Mend=  213 : Dl= 0.562500000 deg : Dx=  63.00 km : Imax=  640 : Jmax=  320 : Quadratic
# Mend=  254 : Dl= 0.468750000 deg : Dx=  52.50 km : Imax=  768 : Jmax=  384 : Quadratic
# Mend=  299 : Dl= 0.400000000 deg : Dx=  44.80 km : Imax=  900 : Jmax=  450 : Quadratic
# Mend=  341 : Dl= 0.351562500 deg : Dx=  39.38 km : Imax= 1024 : Jmax=  512 : Quadratic
# Mend=  382 : Dl= 0.312500000 deg : Dx=  35.00 km : Imax= 1152 : Jmax=  576 : Quadratic
# Mend=  511 : Dl= 0.234375000 deg : Dx=  26.25 km : Imax= 1536 : Jmax=  768 : Quadratic
# Mend=  666 : Dl= 0.180000000 deg : Dx=  20.16 km : Imax= 2000 : Jmax= 1000 : Quadratic
# Mend=  799 : Dl= 0.150000000 deg : Dx=  16.80 km : Imax= 2400 : Jmax= 1200 : Quadratic
# Mend=  999 : Dl= 0.120000000 deg : Dx=  13.44 km : Imax= 3000 : Jmax= 1500 : Quadratic
# Mend= 1260 : Dl= 0.093750000 deg : Dx=  10.50 km : Imax= 3840 : Jmax= 1920 : Quadratic

# Machine options: SX6; Linux
export MAQUI=SX6

# Set  Output Res for Chopping
export RESOUT=299
export KMOUT=64

# Set  T170 Quadratic
export RESO=299
export IM=900
export JM=450

# Set  T170 Quadratic

export SetLinear=FALSE

if [ "$SetLinear" = "TRUE" ]; then
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

if [ ${KMOUT} -lt 1000 ]; then
export prefixVert=${KMOUT}
if [ ${KMOUT} -lt 100 ]; then
export prefixVert=0${KMOUT}
if [ ${KMOUT} -lt 10 ]; then
export prefixVert=00${KMOUT}
fi
fi
fi

if [ ${JM} -lt "10000" ]; then
export prefixGrid=0${JM}
if [ ${JM} -lt 1000 ]; then
export prefixGrid=00${JM}
if [ ${JM} -lt 100 ]; then
export prefixGrid=000${JM}
if [ ${JM} -lt 10 ]; then
export prefixGrid=0000${JM}
fi
fi
fi
fi
#
#set run date
export DATA=2004032600

echo " ***** Gerando arquivos do pre-processamento do Global *****" 
#
# Set <pre-scripts>=1 to execute or <pre-scripts>=0 not execute
# 
TopoWaterPercNavy=0
TopoWaterPercGT30=0 
LandSeaMask=1
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
Chopping=1
SSTClima=1
SSTWeeklyNCEP=1
SSTWeekly=1


cd  ${dirhome}/scripts
if [ $TopoWaterPercNavy -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_TopoWaterPercNavy.ksh
   ${dirhome}/scripts/run_TopoWaterPercNavy.ksh
   file_out=${dirdata}/pre/dataout/TopoNavy.dat
   file_out2=${dirdata}/pre/dataout/WaterNavy.dat
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
   echo executando: ${dirhome}/scripts/run_TopoWaterPercGT30.ksh
   ${dirhome}/scripts/run_TopoWaterPercGT30.ksh
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
   echo executando: ${dirhome}/scripts/run_LandSeaMask.ksh
   ${dirhome}/scripts/run_LandSeaMask.ksh
   file_out=${dirdata}/pre/dataout/LandSeaMaskNavy.G${prefixGrid}.dat
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
if [ $VarTopo -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_VarTopo.ksh
   ${dirhome}/scripts/run_VarTopo.ksh
   file_out=${dirdata}/pre/dataout/Topography.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_TopoSpectral.ksh
   ${dirhome}/scripts/run_TopoSpectral.ksh
   file_out=${dirdata}/model/datain/TopoVariance.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_VegetationMaskSSiB.ksh
   ${dirhome}/scripts/run_VegetationMaskSSiB.ksh
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
   echo executando: ${dirhome}/scripts/run_VegetationMask.ksh
   ${dirhome}/scripts/run_VegetationMask.ksh
   file_out=${dirdata}/pre/dataout/VegetationMask.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_VegetationAlbedoSSiB.ksh
   ${dirhome}/scripts/run_VegetationAlbedoSSiB.ksh
   file_out=${dirdata}/model/datain/VegetationMask.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_DeepSoilTemperatureClima.ksh
   ${dirhome}/scripts/run_DeepSoilTemperatureClima.ksh
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
   echo executando: ${dirhome}/scripts/run_DeepSoilTemperature.ksh
   ${dirhome}/scripts/run_DeepSoilTemperature.ksh
   file_out=${dirdata}/model/datain/DeepSoilTemperature.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_RoughnessLengthClima.ksh
   ${dirhome}/scripts/run_RoughnessLengthClima.ksh
   file_out=${dirdata}/pre/dataout/RoughnessLengthClima.dat
   sleep 15
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
   echo executando: ${dirhome}/scripts/run_RoughnessLength.ksh
   ${dirhome}/scripts/run_RoughnessLength.ksh
   file_out=${dirdata}/model/datain/RoughnessLength.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_SoilMoistureClima.ksh
   ${dirhome}/scripts/run_SoilMoistureClima.ksh
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
   echo executando: ${dirhome}/scripts/run_SoilMoisture.ksh
   ${dirhome}/scripts/run_SoilMoisture.ksh
   file_out=${dirdata}/model/datain/SoilMoisture.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_AlbedoClima.ksh
   ${dirhome}/scripts/run_AlbedoClima.ksh
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
   echo executando: ${dirhome}/scripts/run_Albedo.ksh
   ${dirhome}/scripts/run_Albedo.ksh
   file_out=${dirdata}/pre/dataout/Albedo.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_SnowClima.ksh
   ${dirhome}/scripts/run_SnowClima.ksh
   file_out=${dirdata}/model/datain/Snow${DATA}S.unf.G${prefixGrid}
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
if [ $Chopping -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_Chopping.ksh
   ${dirhome}/scripts/run_Chopping.ksh
   file_out=${dirdata}/model/datain/GANLNMC${DATA}S.unf.${TRUNC}L${prefixVert}
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
if [ $SSTClima -eq 1 ]; then
   echo executando: ${dirhome}/scripts/run_SSTClima.ksh
   ${dirhome}/scripts/run_SSTClima.ksh
   datt=`echo ${DATA} |cut -c 1-8`
   file_out=${dirdata}/model/datain/SSTClima$datt.G${prefixGrid}
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
   echo executando: ${dirhome}/scripts/run_SSTWeeklyNCEP.ksh
   ${dirhome}/scripts/run_SSTWeeklyNCEP.ksh
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
   echo executando: ${dirhome}/scripts/run_SSTWeekly.ksh
   ${dirhome}/scripts/run_SSTWeekly.ksh
   datt=`echo ${DATA} |cut -c 1-8`
   file_out=${dirdata}/model/datain/SSTWeekly$datt.G${prefixGrid}
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

