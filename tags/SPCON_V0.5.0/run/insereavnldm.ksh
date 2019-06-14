#!/bin/ksh

. ../include/config.sx6

for arq in `find ${DKOENS}/oens/pos/dataout/T126L28 -name "GPOSAVN*.grb" -mtime -1 -print`; do
      scp $arq ldm@tumbyra:/home/iddftp/T126
done
ssh -l ldm tumbyra "ls -ltr /home/iddftp/T126"
ssh -l ldm tumbyra "find /home/iddftp/T126 -name "GPOSAVN*.grb" -mtime +2 -exec rm -fv {} \;"

for arq in `find ${DKOENS}/oens/ensmed/dataout/T126L28 -name "GPOSENM*.grb" -mtime -1 -print`; do
      scp $arq ldm@tumbyra:/home/iddftp/OENS
done
ssh -l ldm tumbyra "ls -ltr /home/iddftp/OENS"
ssh -l ldm tumbyra "find /home/iddftp/OENS -name "GPOSENM*.grb" -mtime +2 -exec rm -fv {} \;"
