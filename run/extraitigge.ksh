#!/bin/ksh

dirtiggeout=z_tigge_c_sbsj_20120121000000_glob
dirout=/rede/nas/modoper/tigge/${dirtiggeout}/
mkdir $dirout

for arq in $(ls /rede/hsm/tigge/2012/01/${dirtiggeout}/* | grep dsct); do
      d=1
      arqg=$(echo $arq | sed -e s%dsct%grib%g)

      for arqgrib in $(cat $arq); do
      grb=$(echo $arqgrib | cut -d: -f1)
      echo $dirout/$grb
      /usr/local/bin/wgrib2 -tigge -d $d $arqg -grib $dirout/$grb
      
      let d=$d+1
      echo $grb
      done
done


exit 0
