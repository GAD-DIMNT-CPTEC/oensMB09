#!/bin/bash -x

LABELI=$1
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunmodgmpi.sx6 YYYYMMDDHH"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunmodgmpi.sx6 YYYYMMDDHH"
            exit 2
      else
            YYYY=`echo $LABELI |cut -c 1-4`
            MM=`echo $LABELI |cut -c 5-6`
            DD=`echo $LABELI |cut -c 7-8`
            HH=`echo $LABELI |cut -c 9-10`

            LABELF=`date -d "${NFDAYS} day ${YYYY}${MM}${DD}" +"%Y%m%d${HH}"`
            YYYYF=`echo $LABELF |cut -c 1-4`
            MMF=`echo $LABELF |cut -c 5-6`
            DDF=`echo $LABELF |cut -c 7-8`
            HHF=`echo $LABELF |cut -c 9-10`
      fi
fi

PREFX=$2
TRC=$3
LV=$4

CASE=$(echo ${TRC} ${LV} |awk '{ printf("TQ%4.4dL%3.3d\n",$1,$2)  }' )

PATHA=${HOME_suite}
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
export PATHBASE=`cd ${PATHENV};cd ../;pwd`
. ${FILEENV} ${CASE} ${PREFX}

if [ -s $PREFX ]; then
      echo "ERRO - PARAMETRO PERT\nFORMATO: runrectigge.sx6 yyyymmddhh 01N"
      exit 2
fi

DFSCSKILL=${NFDAYS}
NFCTDY=${NFDAYS}
NMDAYS=${NFDAYS}
NMEMBR=`echo "${NPERT}*2+1" | bc -l`
OUT=out
NPROC=1
RESOL=${CASE}
#
#  End of setting parameters to run
#
set +x

DIRARQ=${BANGU}/GPOS
DIRRMV=${NAS}/removies/ensmed
GRIBMAP="/usr/local/grads/bin/gribmap "

cd ${SC2_suite}/produtos/skill/dataout/${CASE}/temp

rm -f *

mkdir -p ./ctl
rm -f ./ctl/*

NP[1]=P
NP[2]=N
NP[3]=${PREFX}
NP[4]=enmrmv

nasf=0
npc=1
echo " " > ./nullfile
while [ $npc -le 3 ]; do # VIRA O N e P, AVN e ENM RMV(RM VIES)
      membn=1
            while [ $membn -le 7 ]; do # VIRA OS MEMBROS 01 a 07
                  membn=$(echo $membn| awk '{printf ("%2.2d\n",$1)}')
                  echo membn=$membn${NP[$npc]}
                  t=0
                  while [ $t -le $DFSCSKILL ]; do
                        datefct=$(date -d "$YYYY$MM$DD $HH:00 $t day ago" +"%Y%m%d%H")
                        DIRARQ=${ARC_suite}/${CASE}
                        echo "DIRARQ=$DIRARQ"
                        CTLDATE=`date -d "$YYYY$MM$DD $HH:00 $t day ago" +"%HZ%d%b%Y"`
                        yyyyd=`echo $datefct | cut -c 1-4`
                        mmd=`echo $datefct | cut -c 5-6`
                        ddd=`echo $datefct | cut -c 7-8`
                        hhd=`echo $datefct | cut -c 9-10`
                        if [ $t -eq 0 ]; then
                              icnfct=icn
                        else
                              icnfct=fct
                        fi
                        
                        
                        if [ $npc -lt 3 ]; then
                           PREFXN=$membn${NP[$npc]}
                           ARQINGRB=$(find ${DIRARQ}/$yyyyd$mmd/$ddd$hhd/pos/$(echo $membn${NP[$npc]} | tr A-Z a-z) -name GPOS$membn${NP[$npc]}$datefct${LABELI}P.${icnfct}.${CASE}.grb)
                           echo $ARQINGRB
                           CTLNAME=$(echo $ARQINGRB | sed -e s#grb#ctl#g | sed -e s#./ctl/#""#g)
                           IDXNAME=$(echo $ARQINGRB | sed -e s#grb#idx#g | sed -e s#./ctl/#""#g)
                           ln -sf $CTLNAME ./ctl/GPOS${PREFXN}$datefct${LABELI}P.${icnfct}.${CASE}.ctl
                           ln -sf $IDXNAME ./ctl/GPOS${PREFXN}$datefct${LABELI}P.${icnfct}.${CASE}.idx
                        else
                           PREFXN=${PREFX}
                           membn=999999
                           ARQINGRB=$(find ${DIRARQ}/$yyyyd$mmd/$ddd$hhd/pos/ctrl -name GPOS${PREFX}$datefct${LABELI}P.${icnfct}.${CASE}.grb)
                           CTLNAME=$(echo $ARQINGRB | sed -e s#grb#ctl#g | sed -e s#./ctl/#""#g)
                           IDXNAME=$(echo $ARQINGRB | sed -e s#grb#idx#g | sed -e s#./ctl/#""#g)
                           echo $ARQINGRB
                           ln -sf $CTLNAME ./ctl/GPOS${PREFXN}$datefct${LABELI}P.${icnfct}.${CASE}.ctl
                           ln -sf $IDXNAME ./ctl/GPOS${PREFXN}$datefct${LABELI}P.${icnfct}.${CASE}.idx
                        fi
                        
                        if [ ! -z $ARQINGRB ];then
                           ln -sf $ARQINGRB ./ctl/GPOS${PREFXN}$datefct${LABELI}P.${icnfct}.${CASE}.grb
                        else
                           ln -sf ./nullfile ./ctl/GPOS${PREFXN}$datefct${LABELI}P.${icnfct}.${CASE}.grb
                        fi
                        
                        CTLNAME=`ls ./ctl/GPOS${PREFXN}$datefct$LABELI*.grb | sed -e s#grb#ctl#g | sed -e s#./ctl/#""#g`
                        GRBNAME=`ls ./ctl/GPOS${PREFXN}$datefct$LABELI*.grb | sed -e s#./ctl/#""#g`
                        IDXNAME=`ls ./ctl/GPOS${PREFXN}$datefct$LABELI*.grb | sed -e s#grb#idx#g | sed -e s#./ctl/#""#g`
                        
                        if [ $t -ne 0 ]; then
                           echo "./ctl/$CTLNAME" >> filefct${PREFXN}.${CASE}
                        else
                           echo "./ctl/$CTLNAME" >> fileanl.${CASE}
                        fi

                  t=$(echo $t+1 | bc )
                  done
            membn=$(echo $membn+1 | bc )
            done
         npc=$(echo $npc+1 | bc)
   done

exit 0