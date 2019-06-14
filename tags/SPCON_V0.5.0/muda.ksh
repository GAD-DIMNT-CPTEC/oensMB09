#!/bin/ksh

dir1=/gfs/dk20/modoper/tempo/global/oens/pos/dataout/T126L28/
cd $dir1
for arq in `ls GPOS???2009050312*.grb`
do
      nnfd1=`echo $arq | cut -c 8-17`
      nnfd2=`echo $arq | cut -c 18-27`
      ini=`echo $arq | cut -c 1-7`
      fin=`echo $arq | cut -c 28-44`
      nnfd1=`/gfs/home3/modoper/tools/caldate.3.1.2 $nnfd1 - 12h "yyyymmddhh"`
      nnfd2=`/gfs/home3/modoper/tools/caldate.3.1.2 $nnfd2 - 12h "yyyymmddhh"`
      echo mv $arq $ini$nnfd1$nnfd2$fin
      


done
