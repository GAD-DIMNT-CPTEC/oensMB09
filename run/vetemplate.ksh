#!/bin/ksh

dir=/rede/hsm/oens/nmc/T126L28/ENSMED/
grads=/usr/local/grads-1.9b4/bin/gradsc 
labeli=2009040100
labelf=2009042312

while [ $labeli -le $labelf ]; do
yyyy=`echo $labeli | cut -c 1-4`
mm=`echo $labeli | cut -c 5-6`
dd=`echo $labeli | cut -c 7-8`
hh=`echo $labeli | cut -c 9-10`
echo "DATA :${labeli}"

icn=`ls $dir/$yyyy/$mm/$dd/GPOSENM${labeli}*icn.*ctl`
tpl=`ls $dir/$yyyy/$mm/$dd/GPOSENM${labeli}.ctl`

cat <<EOF> /gfs/home3/modoper/.jklhasdds234
open $icn
open $tpl

d ave(psnm.1-psnm.2)
'
EOF


labeli=`date -d "$yyyy$mm$dd ${hh}:00 12 hours" +"%Y%m%d%H"`
done


exit 0
