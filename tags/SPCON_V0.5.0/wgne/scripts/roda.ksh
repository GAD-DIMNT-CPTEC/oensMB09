#!/bin/ksh

datai=20100716
dataf=20100805

while [ $datai -lt $dataf ]; do
      run_HISTORICO_wgne.ksh ${datai}00 AVN
      run_HISTORICO_wgne.ksh ${datai}12 AVN
      datai=`date -d "$datai 1 day" +"%Y%m%d"`
done

exit 0
