#!/bin/ksh

REPOSITORIO_TIGGE="/bangu/samfs/io_dop/tigge/"
DIROUT="/gfs/dk19/io_dop/users/alex/oenspro/tigge"
LISTA_MISSING_TIGGE="/gfs/home3/io_dop/users/alex/oenspro/tigge/scripts/lista_missingdata_email.dat"

for arq in `cat ${LISTA_MISSING_TIGGE}`; do

      DIRDATATIGGE=`echo ${arq} | cut -c 1-39`
      REPOSITORIO_TIGGE_IN=`echo ${arq} | cut -c 1-34`
      mkdir -p ${DIROUT}/${DIRDATATIGGE}
      cp -rf ${REPOSITORIO_TIGGE}/${REPOSITORIO_TIGGE_IN}/${arq} ${DIROUT}/${DIRDATATIGGE}

done

exit 0
