#
#  $Author: pkubota $
#  $Date: 2007/03/06 14:43:32 $
#  $Revision: 1.3 $
#
# $Id: Makefile,v 1.3 2007/03/06 14:43:32 pkubota Exp $
#
SHELL=/bin/sh
#
# Makefile for Pos-Proccessing output global model
#
# Targets are the suffixes of the system-specific makefiles in
# the makefiles/ directory.
# For example, to build PosGrib for Solaris, give the command
#
#   make solaris
#
# This builds an intermediate library in the util/ directory,
# then builds the nedit and nc executables in the source/ directory.
#
aptions1="alb       albc       chp	dst	  dstc      lndmk     "
aptions2="rolen     rolenc     snowc	soimo	  soimoc    sstclm    "
aptions3="sstwkl    sstwklncep topospec topogt30  toponavy  vartopo   "
aptions4="albsib    vegmsk     parsib	topograd "
 
opt=" "
MAKE=make
PATH2=/scratch1/grupos/ensemble_g/home/eduardo.charles/oens_newEOF/pre/exec

all:
	@echo "Please specify target:"
	@echo "(For example, type \"make linux\" for a Linux system.)"
	@(cd  makefiles && ls -C Makefile* | sed -e 's/Makefile.//g')
	@echo "(For example, type \"make linux opt=alb\" for albedo option)"
	@echo "alb       albc        chp       dst       dstc     lndmk     "
	@echo "rolen     rolenc      snowc     soimo     soimoc   sstclm    "
	@echo "sstwkl    sstwklncep  topospec  topogt30  toponavy vartopo   "
	@echo "albsib    vegmsk      parsib    topograd                     "

.DEFAULT:
	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f Albedo/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ Albedo/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f AlbedoClima/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ AlbedoClima/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f Chopping/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ Chopping/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f DeepSoilTemperature/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ DeepSoilTemperature/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f DeepSoilTemperatureClima/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ DeepSoilTemperatureClima/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f LandSeaMask/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ LandSeaMask/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f RoughnessLength/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ RoughnessLength/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f RoughnessLengthClima/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ RoughnessLengthClima/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f SnowClima/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ SnowClima/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f SoilMoisture/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ SoilMoisture/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f SoilMoistureClima/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ SoilMoistureClima/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f SSTClima/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ SSTClima/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f SSTWeekly/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ SSTWeekly/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f SSTWeeklyNCEP/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ SSTWeeklyNCEP/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f TopoSpectral/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ TopoSpectral/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f TopoWaterPercGT30/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ TopoWaterPercGT30/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f TopoWaterPercNavy/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ TopoWaterPercNavy/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f VarTopo/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ VarTopo/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f VegetationAlbedoSSiB/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ VegetationAlbedoSSiB/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f VegetationMask/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ VegetationMask/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f VegetationMaskSSiB/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ VegetationMaskSSiB/Makefile.$@; fi)

	@- (cd .; if [ -f makefiles/Makefile.$@ -a ! -f TopographyGradient/Makefile.$@ ];\
	   then ln -fs ../makefiles/Makefile.$@ TopographyGradient/Makefile.$@; fi)

#
# builds the binaries
#
	@ (if [ ${opt} = "alb" ];then cd Albedo;$(MAKE) -f Makefile.$@;mv Albedo $(PATH2)/Albedo; fi)
	@ (if [ ${opt} = " " ];then   cd Albedo;$(MAKE) -f Makefile.$@;mv Albedo $(PATH2)/Albedo; fi)

	@ (if [ ${opt} = "albc" ];then cd AlbedoClima;$(MAKE) -f Makefile.$@;mv AlbedoClima $(PATH2)/AlbedoClima; fi)
	@ (if [ ${opt} = " " ];then   cd AlbedoClima;$(MAKE) -f Makefile.$@;mv AlbedoClima $(PATH2)/AlbedoClima; fi)

	@ (if [ ${opt} = "chp" ];then cd Chopping;$(MAKE) -f Makefile.$@;mv Chopping $(PATH2)/Chopping; fi)
	@ (if [ ${opt} = " " ];then   cd Chopping;$(MAKE) -f Makefile.$@;mv Chopping $(PATH2)/Chopping; fi)

	@ (if [ ${opt} = "dst" ];then cd DeepSoilTemperature;$(MAKE) -f Makefile.$@;mv DeepSoilTemperature $(PATH2)/DeepSoilTemperature; fi)
	@ (if [ ${opt} = " " ];then   cd DeepSoilTemperature;$(MAKE) -f Makefile.$@;mv DeepSoilTemperature $(PATH2)/DeepSoilTemperature; fi)

	@ (if [ ${opt} = "dstc" ];then cd DeepSoilTemperatureClima;$(MAKE) -f Makefile.$@;mv DeepSoilTemperatureClima $(PATH2)/DeepSoilTemperatureClima; fi)
	@ (if [ ${opt} = " " ];then   cd DeepSoilTemperatureClima;$(MAKE) -f Makefile.$@;mv DeepSoilTemperatureClima $(PATH2)/DeepSoilTemperatureClima; fi)

	@ (if [ ${opt} = "lndmk" ];then cd LandSeaMask;$(MAKE) -f Makefile.$@;mv LandSeaMask $(PATH2)/LandSeaMask; fi)
	@ (if [ ${opt} = " " ];then   cd LandSeaMask;$(MAKE) -f Makefile.$@;mv LandSeaMask $(PATH2)/LandSeaMask; fi)

	@ (if [ ${opt} = "rolen" ];then cd RoughnessLength;$(MAKE) -f Makefile.$@;mv RoughnessLength $(PATH2)/RoughnessLength; fi)
	@ (if [ ${opt} = " " ];then   cd RoughnessLength;$(MAKE) -f Makefile.$@;mv RoughnessLength $(PATH2)/RoughnessLength; fi)

	@ (if [ ${opt} = "rolenc" ];then cd RoughnessLengthClima;$(MAKE) -f Makefile.$@;mv RoughnessLengthClima $(PATH2)/RoughnessLengthClima; fi)
	@ (if [ ${opt} = " " ];then   cd RoughnessLengthClima;$(MAKE) -f Makefile.$@;mv RoughnessLengthClima $(PATH2)/RoughnessLengthClima; fi)

	@ (if [ ${opt} = "snowc" ];then cd SnowClima;$(MAKE) -f Makefile.$@;mv SnowClima $(PATH2)/SnowClima; fi)
	@ (if [ ${opt} = " " ];then   cd SnowClima;$(MAKE) -f Makefile.$@;mv SnowClima $(PATH2)/SnowClima; fi)

	@ (if [ ${opt} = "soimo" ];then cd SoilMoisture;$(MAKE) -f Makefile.$@;mv SoilMoisture $(PATH2)/SoilMoisture; fi)
	@ (if [ ${opt} = " " ];then   cd SoilMoisture;$(MAKE) -f Makefile.$@;mv SoilMoisture $(PATH2)/SoilMoisture; fi)

	@ (if [ ${opt} = "soimoc" ];then cd SoilMoistureClima;$(MAKE) -f Makefile.$@;mv SoilMoistureClima $(PATH2)/SoilMoistureClima; fi)
	@ (if [ ${opt} = " " ];then   cd SoilMoistureClima;$(MAKE) -f Makefile.$@;mv SoilMoistureClima $(PATH2)/SoilMoistureClima; fi)

	@ (if [ ${opt} = "sstclm" ];then cd SSTClima;$(MAKE) -f Makefile.$@;mv SSTClima $(PATH2)/SSTClima; fi)
	@ (if [ ${opt} = " " ];then   cd SSTClima;$(MAKE) -f Makefile.$@;mv SSTClima $(PATH2)/SSTClima; fi)

	@ (if [ ${opt} = "sstwkl" ];then cd SSTWeekly;$(MAKE) -f Makefile.$@;mv SSTWeekly $(PATH2)/SSTWeekly; fi)
	@ (if [ ${opt} = " " ];then   cd SSTWeekly;$(MAKE) -f Makefile.$@;mv SSTWeekly $(PATH2)/SSTWeekly; fi)

	@ (if [ ${opt} = "sstwklncep" ];then cd SSTWeeklyNCEP;$(MAKE) -f Makefile.$@;mv SSTWeeklyNCEP $(PATH2)/SSTWeeklyNCEP; fi)
	@ (if [ ${opt} = " " ];then   cd SSTWeeklyNCEP;$(MAKE) -f Makefile.$@;mv SSTWeeklyNCEP $(PATH2)/SSTWeeklyNCEP; fi)

	@ (if [ ${opt} = "topospec" ];then cd TopoSpectral;$(MAKE) -f Makefile.$@;mv TopoSpectral $(PATH2)/TopoSpectral; fi)
	@ (if [ ${opt} = " " ];then   cd TopoSpectral;$(MAKE) -f Makefile.$@;mv TopoSpectral $(PATH2)/TopoSpectral; fi)

	@ (if [ ${opt} = "topogt30" ];then cd TopoWaterPercGT30;$(MAKE) -f Makefile.$@;mv TopoWaterPercGT30 $(PATH2)/TopoWaterPercGT30; fi)
	@ (if [ ${opt} = " " ];then   cd TopoWaterPercGT30;$(MAKE) -f Makefile.$@;mv TopoWaterPercGT30 $(PATH2)/TopoWaterPercGT30; fi)

	@ (if [ ${opt} = "toponavy" ];then cd TopoWaterPercNavy;$(MAKE) -f Makefile.$@;mv TopoWaterPercNavy $(PATH2)/TopoWaterPercNavy; fi)
	@ (if [ ${opt} = " " ];then   cd TopoWaterPercNavy;$(MAKE) -f Makefile.$@;mv TopoWaterPercNavy $(PATH2)/TopoWaterPercNavy; fi)

	@ (if [ ${opt} = "vartopo" ];then cd VarTopo;$(MAKE) -f Makefile.$@;mv VarTopo $(PATH2)/VarTopo; fi)
	@ (if [ ${opt} = " " ];then   cd VarTopo;$(MAKE) -f Makefile.$@;mv VarTopo $(PATH2)/VarTopo; fi)

	@ (if [ ${opt} = "albsib" ];then cd VegetationAlbedoSSiB;$(MAKE) -f Makefile.$@;mv VegetationAlbedoSSiB $(PATH2)/VegetationAlbedoSSiB; fi)
	@ (if [ ${opt} = " " ];then   cd VegetationAlbedoSSiB;$(MAKE) -f Makefile.$@;mv VegetationAlbedoSSiB $(PATH2)/VegetationAlbedoSSiB; fi)

	@ (if [ ${opt} = "vegmsk" ];then cd VegetationMask;$(MAKE) -f Makefile.$@;mv VegetationMask $(PATH2)/VegetationMask; fi)
	@ (if [ ${opt} = " " ];then   cd VegetationMask;$(MAKE) -f Makefile.$@;mv VegetationMask $(PATH2)/VegetationMask; fi)

	@ (if [ ${opt} = "parsib" ];then cd VegetationMaskSSiB;$(MAKE) -f Makefile.$@;mv VegetationMaskSSiB $(PATH2)/VegetationMaskSSiB; fi)
	@ (if [ ${opt} = " " ];then   cd VegetationMaskSSiB;$(MAKE) -f Makefile.$@;mv VegetationMaskSSiB $(PATH2)/VegetationMaskSSiB; fi)

	@ (if [ ${opt} = "topograd" ];then cd TopographyGradient;$(MAKE) -f Makefile.$@;mv TopographyGradient $(PATH2)/TopographyGradient; fi)
	@ (if [ ${opt} = " " ];then   cd TopographyGradient;$(MAKE) -f Makefile.$@;mv TopographyGradient $(PATH2)/TopographyGradient; fi)

# We need a "dev-all" target that builds the docs plus binaries, but
# that doesn't work since we require the user to specify the target.  More
# thought is needed

clean:
	@echo "$(MAKE)"

	(cd Albedo;$(MAKE) -f Makefile.common clean)
	(cd AlbedoClima;$(MAKE) -f Makefile.common clean)
	(cd Chopping;$(MAKE) -f Makefile.common clean)
	(cd DeepSoilTemperature;$(MAKE) -f Makefile.common clean)
	(cd DeepSoilTemperatureClima;$(MAKE) -f Makefile.common clean)
	(cd LandSeaMask;$(MAKE) -f Makefile.common clean)
	(cd RoughnessLength;$(MAKE) -f Makefile.common clean)
	(cd RoughnessLengthClima;$(MAKE) -f Makefile.common clean)
	(cd SnowClima;$(MAKE) -f Makefile.common clean)
	(cd SoilMoisture;$(MAKE) -f Makefile.common clean)
	(cd SoilMoistureClima;$(MAKE) -f Makefile.common clean)
	(cd SSTClima;$(MAKE) -f Makefile.common clean)
	(cd SSTWeekly;$(MAKE) -f Makefile.common clean)
	(cd SSTWeeklyNCEP;$(MAKE) -f Makefile.common clean)
	(cd TopoSpectral;$(MAKE) -f Makefile.common clean)
	(cd TopoWaterPercGT30;$(MAKE) -f Makefile.common clean)
	(cd TopoWaterPercNavy;$(MAKE) -f Makefile.common clean)
	(cd VarTopo;$(MAKE) -f Makefile.common clean)
	(cd VegetationAlbedoSSiB;$(MAKE) -f Makefile.common clean)
	(cd VegetationMask;$(MAKE) -f Makefile.common clean)
	(cd VegetationMaskSSiB;$(MAKE) -f Makefile.common clean)
	(cd TopographyGradient;$(MAKE) -f Makefile.common clean)
