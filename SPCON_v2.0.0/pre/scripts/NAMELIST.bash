#!/bin/bash
#######################################################
# NAMELIST OPTIONS OF RESOLUTION AND INITIAL CONDITION#
#######################################################
export dirhome=/stornext/home/paulo.kubota/agcm/pre
export dirdata=/scratch1/home/paulo.kubota/agcm
export dirgrads=/usr/local/grads
#
# Machine options: SX6; Linux,XT6
export MAQUI=XT6
#
# Set  Res for Chopping
#
export RESOUT=42
export KMOUT=28
export SetLinear=FALSE
export RESO=42
export IM=128
export JM=64
export prefix=${JM}
#
#set run date#
#
export DATA=2004032600
###################################################
