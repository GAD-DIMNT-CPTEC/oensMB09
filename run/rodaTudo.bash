#!/bin/bash
RESOL=TQ0126L028
TRC=126
LV=28
PREFX=NMC

LABELI=2015040100

LABELF=$(date -u -d "${LABELI:0:8} ${LABELI:8:2}:00 15 days" +"%Y%m%d")

nproc=72

./runPre ${RESOL} ${LABELI} CHOPPING SSTWEEKLYNCEP SSTWEEKLY SNOWCLIMA
./runModel $nproc 24 1 ${RESOL} $PREFX ${LABELI} $PREFX 2>/dev/null 1>&2 &   # CONTROLE

./runrecanl.bash ${RESOL} NMC ANLNMC ${LABELI}                                    # SCRIPTALONE - APENAS SUBMETE
./runrdpt.bash ${RESOL} NMC YES ${LABELI}                                         # SCRIPTALONE - APENAS SUBMETE
./rundrpt.bash ${RESOL} NMC YES ${LABELI}                                         # SCRIPTALONE - APENAS SUBMETE

./runctrmodgpro.bash 48 24 1 ${RESOL} NMC ${LABELI} CTR                           # SCRIPTALONE - APENAS SUBMETE
./runrecfct.bash ${RESOL} CTR FCTCTR ${LABELI}                                 # SCRIPTALONE - APENAS SUBMETE

for F in $(seq -w 01 07); do
   ./runperpntg2.bash ${RESOL} ${F}N YES ${LABELI} sstwkl # Este script gera as analises N e P.
   ./runModel $nproc 24 1 ${RESOL} ${F}N ${LABELI} ${F}N 2>/dev/null 1>&2 &
   ./runperpntg2.bash ${RESOL} ${F}P YES ${LABELI} sstwkl # Este script gera as analises N e P.
   ./runModel $nproc 24 1 ${RESOL} ${F}P ${LABELI} ${F}P 2>/dev/null 1>&2 &
done
wait

echo ./runPos 1 1 PosNMC $TRC $LV ${LABELI} ${LABELF} NMC 3600 2>/dev/null 1>&2 &

for F in $(seq -w 01 07); do
   sleep 2
   ./runPos 1 1 Pos${F}N $TRC $LV ${LABELI} ${LABELF} ${F}N 3600 2>/dev/null 1>&2 &
   sleep 2
   ./runPos 1 1 Pos${F}N $TRC $LV ${LABELI} ${LABELF} ${F}P 3600 2>/dev/null 1>&2 &
done
wait

exit 0
