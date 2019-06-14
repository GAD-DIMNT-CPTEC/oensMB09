#!/bin/ksh

. ../include/config.sx6

LABELI=$1
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunensmedg.sx6 YYYYMMDDHH n/h (n=nas, h=bangu)"
      exit 1
else
      if [ ${#LABELI} -lt 10 ]; then
            echo "ERRO: PARAMETRO INCORRETO.\nrunensmedg.sx6 YYYYMMDDHH"
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

if [ -s $2 ]; then
      echo "ERRO: FALTA PARAMETRO. \nrunensmedg.sx6 YYYYMMDDHH n/h (n=nas, h=bangu)"
      exit 3
else
      if [ "$2" = "n" -o "$2" = "N" ]; then
            BANGU=$NAS
            ntentativas=1
      else
            if [ "$2" = "h" -o "$2" = "H" ]; then
                  BANGU=$BANGU
                  ntentativas=180
            else
                  echo "ERRO: PARAMETRO 2 deve ser n ou h"
                  exit 4
            fi

      fi
fi

NFCTDY=$FSCT
let NMEMBR=${NPERT}*2+1
PREFX=${PERT}

set -x

YYYY=`echo $LABELI | cut -c 1-4`
MM=`echo $LABELI | cut -c 5-6`
DD=`echo $LABELI | cut -c 7-8`

aaa=0
itc=0
test=0
while [ 0 ]; do

f=0

a=`pwd`

# COPIA DOS ARQUIVOS DO MODELO (GFCT e GFGH)

cd $ROPERM/model/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/
mkdir -p ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/

set +x

#GFCT ###############################################################################
echo "COPIANDO GFCT... `ls -ltr ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype    dic    din    icn       inz    dir    fct     files
set -A fsize    1941   1941 11247656 11247656 3006  21775360  12276
TOTALDEARQUIVOS=1875
icont=0
fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find . -name "GFCT??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
#                  find . -name "${aname}" -print | cpio -pdmv ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/ > .out 2>&1
                  if [ ! -s ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/$aname ]; then
#                        echo "COPIANDO $aname"
                        cp -rfv ${aname} ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/
                  fi
                  let cont=cont+1
            fi
            let icont=icont+1
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GFCT COPIADOS: $cont OK"
      let f=$f+1
fi

#GFGH ###############################################################################
echo "COPIANDO GFGH... `ls -ltr ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype    unf       top   dir
set -A fsize    83980800 3892  52649
TOTALDEARQUIVOS=45

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find . -name "GFGH??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            
            if [ $asize -ge ${fsize[$fj]} ]; then
#                  echo "COPIANDO $aname"
                  if [ ! -s ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/$aname ]; then
#                  find . -name "${aname}" -print | cpio -pdmv ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/ > .out 2>&1 
                        cp -rfv ${aname} ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/
                  fi
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GFGH COPIADOS: $cont OK"
      let f=$f+1
fi

#GPRG ###############################################################################
echo "COPIANDO GPRG... `ls -ltr ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype       icn   dic     inz    din    fct    dir  files
set -A fsize    11247656 1941 11247656 1941 11247656 1941  9900
TOTALDEARQUIVOS=731

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find . -name "GPRG??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ ${ftype[$fj]} = "files" ]; then
                  if [ `echo $aname | grep AVN | wc -l` -gt 0 ]; then
                        set -A fsize    11247656 1941 11247656 1941 11247656 1941  9900
                  else
                        set -A fsize    11247656 1941 11247656 1941 11247656 1941  4356
                  fi
            fi
            if [ $asize -ge ${fsize[$fj]} ]; then
#                  echo "COPIANDO $aname"
                  if [ ! -s ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/$aname ]; then
#                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/ > .out 2>&1
                        cp -rfv ${aname} ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/
                  fi
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GPRG COPIADOS: $cont OK"
      let f=$f+1
fi

# COPIA DOS ARQUIVOS DO POS (GPOS)
cd $ROPERM/pos/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/

#GPOS ###############################################################################
echo "COPIANDO GPOS... `ls -ltr ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype   icn.T${TRC}L${LV}.grb icn.T${TRC}L${LV}.idx icn.T${TRC}L${LV}.ctl inz.T${TRC}L${LV}.grb inz.T${TRC}L${LV}.idx inz.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.grb fct.T${TRC}L${LV}.idx fct.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.lst
set -A fsize       23314006          3865           4783          23314006          3865           4783           28773615          4753            6031            7316
TOTALDEARQUIVOS=2805

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find . -name "GPOS??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
#                  echo "COPIANDO $aname"
                  if [ ! -s ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/$aname ]; then
#                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/ > .out 2>&1
                        cp -rfv $aname ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/
                  fi
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GPOS COPIADOS: $cont OK"
      let f=$f+1
fi

#GFGN/GRH ###############################################################################
cd $ROPERM/pos/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GRH/${YYYY}/${MM}/${DD}/
echo "COPIANDO GRH... `ls -ltr ${BANGU}/GRH/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype   grh.T${TRC}L${LV} grh.T${TRC}L${LV}.ctl
set -A fsize     18214560         1166
TOTALDEARQUIVOS=30

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find . -name "GFGN[0A]?[NP]${LABELI}*${ftype[$fj]}" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
#                  echo "COPIANDO $aname"
                  if [ ! -s ${BANGU}/GRH/${YYYY}/${MM}/${DD}/$aname ]; then
#                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/GRH/${YYYY}/${MM}/${DD}/ > .out 2>&1
                        cp -rfv $aname ${BANGU}/GRH/${YYYY}/${MM}/${DD}/
                  fi
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GRH COPIADOS: $cont OK"
      let f=$f+1
fi



# COPIA DOS ARQUIVOS DO ENSMED (GPOSENM)

if [ "$2" = "n" -o "$2" = "N" ]; then
      numaaa=2
else
      numaaa=0
fi

#ENSMED ###############################################################################
cd $ROPERM/ensmed/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/
echo "COPIANDO ENSMED... `ls -ltr ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype   icn.T${TRC}L${LV}.grb icn.T${TRC}L${LV}.idx icn.T${TRC}L${LV}.ctl inz.T${TRC}L${LV}.grb inz.T${TRC}L${LV}.idx inz.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.grb fct.T${TRC}L${LV}.idx fct.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.lst
set -A fsize         23311320                 3865                4782                 23311320                      3865           4782                    28770300              4753                  6030                 5580
TOTALDEARQUIVOS=187

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find . -name "GPOSENM${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
#                  echo "COPIANDO $aname"
                  if [ ! -s ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/$aname ]; then
#                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/ > .out 2>&1
                        cp -rfv $aname ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/
                  fi
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS ENSMED COPIADOS: $cont OK"
      let f=$f+1
fi


# COPIA DOS ARQUIVOS DO PLUMES (?????)

#GFGN/GRH ###############################################################################
cd $ROPERM/plumes/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/
echo "COPIANDO PLUMES... `ls -ltr ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype CONT CBNV NEVE PREC PNEV PSLC PSNM TADL TEMS TOPO TP2M UMRS UVES VSUT VVES
TOTALDEARQUIVOS=30

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find . -name "${ftype[$fj]}${LABELI}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ `echo $aname | grep ctl | wc -l` -gt 0 ]; then
                  set -A fsize 939 338 325 325 325 338 338 338 338 325 325 338 338 338 338
            else
                  set -A fsize 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000
            fi
            if [ $asize -ge ${fsize[$fj]} ]; then
#                  echo "COPIANDO $aname"
                  if [ ! -s ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/$aname ]; then
#                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/ > .out 2>&1
                        cp -rfv $aname ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/
                  fi                         
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS PLUMES COPIADOS: $cont OK"
      let f=$f+1
fi


echo 
echo "COPIAS COMPLETADAS: f=$f"


# VERIFICACAO PARA SAIR DO LOOP
set -x
if [ $f -eq 7 ]; then
      let test=$test+1
      if [ $test -eq 3 ]; then
            aaa=2210
      fi
else
      test=0
      echo "Copias realizadas: $f\ni=$aaa"
fi

if [ $aaa -gt $ntentativas ]; then
      break
fi

sleep 45
echo "sleep 45"
let aaa=$aaa+1
set +x
done

exit 0
