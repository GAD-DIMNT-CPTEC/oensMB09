#!/bin/ksh


. ../include/config.sx6

#################################################################################################################
#################################################################################################################

ensmed () {

LABELI=$1
BANGU=$2

aaa=0
itc=0
test=0
f=0

a=`pwd`

set +x
cd $ROPERM/ensmed/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/
echo "COPIANDO ENSMED... `ls -ltr ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype   icn.T${TRC}L${LV}.grb icn.T${TRC}L${LV}.idx icn.T${TRC}L${LV}.ctl inz.T${TRC}L${LV}.grb inz.T${TRC}L${LV}.idx inz.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.grb fct.T${TRC}L${LV}.idx fct.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.lst
set -A fsize         23311320                 3865                4782                 23311320                      3865           4782                    28770300              4753                  6030                 5580
TOTALDEARQUIVOS=187

echo "VERIFICANDO..."

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/ -name "GPOSENM${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
                  let cont=cont+1
                  fi
      done
let fj=$fj+1
done

if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS ENSMED JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
      smsevent Medio
      echo 0
else
      echo "ARQUIVOS ENSMED AINDA NAO COPIADOS PARA A BANGU... ${LABELI}"
      echo "COPIANDO..."
      
      fj=0
      cont=0
      while [ $fj -lt ${#ftype[*]} ]; do
            for anamesize in `find . -name "GPOSENM${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
                  asize=`echo $anamesize | cut -d: -f1`
                  aname=`echo $anamesize | cut -d: -f2`
                  if [ $asize -ge ${fsize[$fj]} ]; then
      #                  echo "COPIANDO $aname"
                        cmp ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/$aname $aname
                        if [ `echo $?` -ne 0 ]; then
                              echo "cp -rf $ROPERM/ensmed/dataout/T${TRC}L${LV}/$aname ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/"
                              cp -rf $aname ${BANGU}/ENSMED/${YYYY}/${MM}/${DD}/
                        else
                              echo "$aname Já COPIADO"
                        fi
                        let cont=cont+1
                  fi
            done
      let fj=$fj+1
      done
      if [ $cont -ge $TOTALDEARQUIVOS ]; then
            echo "ARQUIVOS ENSMED JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
            smsevent Medio
            echo 0
      else
            echo "ARQUIVOS ENSMED COPIADOS: $cont ERR"
            echo 1
      fi

fi

};

#################################################################################################################
#################################################################################################################

plumes () {

LABELI=$1
BANGU=$2


aaa=0
itc=0
test=0
f=0

a=`pwd`

set +x

cd $ROPERM/plumes/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/
echo "COPIANDO PLUMES... `ls -ltr ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype CONT CBNV NEVE PREC PNEV PSLC PSNM TADL TEMS TOPO TP2M UMRS UVES VSUT VVES
TOTALDEARQUIVOS=30

echo "VERIFICANDO..."

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/ -name "${ftype[$fj]}${LABELI}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ `echo $aname | grep ctl | wc -l` -gt 0 ]; then
                  set -A fsize 939 338 325 325 325 338 338 338 338 325 325 338 338 338 338
            else
                  set -A fsize 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 1000
            fi
            if [ $asize -ge ${fsize[$fj]} ]; then
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done

if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS PLUMES JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
      smsevent Plumas
      echo 0
else
      echo "ARQUIVOS PLUMES AINDA NAO COPIADOS PARA A BANGU... ${LABELI}"
      echo "COPIANDO..."
      
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
                        cmp ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/$aname $aname
                        if [ `echo $?` -ne 0 ]; then
      #                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/ > .out 2>&1
                              cp -rfv $aname ${BANGU}/PLUMES/${YYYY}/${MM}/${DD}/
                        fi                         
                        let cont=cont+1
                  fi
            done
      let fj=$fj+1
      done
      if [ $cont -ge $TOTALDEARQUIVOS ]; then
            echo "ARQUIVOS PLUMES JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
            smsevent Plumas
            echo 0
      else
            echo 1
      fi
fi

}

#################################################################################################################
#################################################################################################################

gfct () {

LABELI=$1
BANGU=$2

cd $ROPERM/model/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/

aaa=0
itc=0
test=0
f=0

a=`pwd`

set +x

echo "COPIANDO GFCT... `ls -ltr ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype    dic    din    icn       inz    dir    fct     files
set -A fsize    1941   1941 11247656 11247656 3006  21775360  12276
TOTALDEARQUIVOS=1875

echo "VERIFICANDO..."

set -x
fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/ -name "GFCT??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done

set +x

if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GFCT JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
      smsevent GFCT
      echo 0
else
      echo "ARQUIVOS GFCT AINDA NAO COPIADOS PARA A BANGU... ${LABELI}"
      echo "COPIANDO..."

      fj=0
      cont=0      
      while [ $fj -lt ${#ftype[*]} ]; do
            for anamesize in `find . -name "GFCT??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
                  asize=`echo $anamesize | cut -d: -f1`
                  aname=`echo $anamesize | cut -d: -f2`
                  if [ $asize -ge ${fsize[$fj]} ]; then
      #                  find . -name "${aname}" -print | cpio -pdmv ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/ > .out 2>&1
                        cmp ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/$aname $aname
                        if [ `echo $?` -ne 0 ]; then
      #                        echo "COPIANDO $aname"
                              cp -rfv ${aname} ${BANGU}/GFCT/${YYYY}/${MM}/${DD}/
                        fi
                        let cont=cont+1
                  fi
            done
      let fj=$fj+1
      done
      if [ $cont -ge $TOTALDEARQUIVOS ]; then
            echo "ARQUIVOS GFCT JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
            smsevent GFCT
            echo 0
      else
            echo 1
      fi
fi

}

#################################################################################################################
#################################################################################################################

gpos () {

LABELI=$1
BANGU=$2


cd $ROPERM/pos/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/

aaa=0
itc=0
test=0
f=0

a=`pwd`

set +x

echo "COPIANDO GPOS... `ls -ltr ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype   icn.T${TRC}L${LV}.grb icn.T${TRC}L${LV}.idx icn.T${TRC}L${LV}.ctl inz.T${TRC}L${LV}.grb inz.T${TRC}L${LV}.idx inz.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.grb fct.T${TRC}L${LV}.idx fct.T${TRC}L${LV}.ctl fct.T${TRC}L${LV}.lst
set -A fsize       23314006          3865           4783          23314006          3865           4783           28773615          4753            6031            7316
TOTALDEARQUIVOS=2805

echo "VERIFICANDO..."

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/ -name "GPOS??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done

if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GPOS JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
      smsevent GPOS
      echo 0
else      

      fj=0
      cont=0
      while [ $fj -lt ${#ftype[*]} ]; do
            for anamesize in `find . -name "GPOS??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
                  asize=`echo $anamesize | cut -d: -f1`
                  aname=`echo $anamesize | cut -d: -f2`
                  if [ $asize -ge ${fsize[$fj]} ]; then
      #                  echo "COPIANDO $aname"
                        cmp ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/$aname $aname
                        if [ `echo $?` -ne 0 ]; then
      #                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/ > .out 2>&1
                              echo "$i cp -rfv $aname ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/ `ps -fu modoper | wc -l`"
                              cp -rf $aname ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/
                        fi
                        let cont=cont+1
                  fi
            done
      let fj=$fj+1
      done

      if [ $cont -ge $TOTALDEARQUIVOS ]; then
            echo "ARQUIVOS GPOS JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
            smsevent GPOS
            echo 0
      else
            echo 1
      fi
fi

}

#################################################################################################################
#################################################################################################################

gfgn () {

LABELI=$1
BANGU=$2


cd $ROPERM/pos/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GRH/${YYYY}/${MM}/${DD}/

aaa=0
itc=0
test=0
f=0

a=`pwd`

set +x

echo "COPIANDO GRH... `ls -ltr ${BANGU}/GRH/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype   grh.T${TRC}L${LV} grh.T${TRC}L${LV}.ctl
set -A fsize     18214560         1166
TOTALDEARQUIVOS=30

echo "VERIFICANDO..."

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find ${BANGU}/GRH/${YYYY}/${MM}/${DD}/ -name "GFGN[0A]?[NP]${LABELI}*${ftype[$fj]}" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            if [ $asize -ge ${fsize[$fj]} ]; then
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GFGN JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
      smsevent GFGN
      echo 0
else
      echo "ARQUIVOS GFGN AINDA NAO COPIADOS PARA A BANGU... ${LABELI}"
      echo "COPIANDO..."
      
      fj=0
      cont=0
      while [ $fj -lt ${#ftype[*]} ]; do
            for anamesize in `find . -name "GFGN[0A]?[NP]${LABELI}*${ftype[$fj]}" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
                  asize=`echo $anamesize | cut -d: -f1`
                  aname=`echo $anamesize | cut -d: -f2`
                  if [ $asize -ge ${fsize[$fj]} ]; then
      #                  echo "COPIANDO $aname"
                        cmp ${BANGU}/GRH/${YYYY}/${MM}/${DD}/$aname $aname
                        if [ `echo $?` -ne 0 ]; then
      #                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/GRH/${YYYY}/${MM}/${DD}/ > .out 2>&1
                              cp -rfv $aname ${BANGU}/GRH/${YYYY}/${MM}/${DD}/
                        fi
                        let cont=cont+1
                  fi
            done
      let fj=$fj+1
      done
      if [ $cont -ge $TOTALDEARQUIVOS ]; then
            echo "ARQUIVOS GFGN JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
            smsevent GFGN
            echo 0
      else
            echo 1
      fi
fi


}

gfgh () {

LABELI=$1
BANGU=$2


cd $ROPERM/model/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/

aaa=0
itc=0
test=0
f=0

a=`pwd`

set +x
echo "COPIANDO GFGH... `ls -ltr ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype    unf       top   dir
set -A fsize    83980800 3892  52649
TOTALDEARQUIVOS=45

echo "VERIFICANDO..."


fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/ -name "GFGH??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
            asize=`echo $anamesize | cut -d: -f1`
            aname=`echo $anamesize | cut -d: -f2`
            
            if [ $asize -ge ${fsize[$fj]} ]; then
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GFGH JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
      smsevent GFGH
      echo 0
else
      echo "ARQUIVOS GFGH AINDA NAO COPIADOS PARA A BANGU... ${LABELI}"
      echo "COPIANDO..."

      fj=0
      cont=0
      while [ $fj -lt ${#ftype[*]} ]; do
            for anamesize in `find . -name "GFGH??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
                  asize=`echo $anamesize | cut -d: -f1`
                  aname=`echo $anamesize | cut -d: -f2`
                  
                  if [ $asize -ge ${fsize[$fj]} ]; then
                        cmp ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/$aname $aname
                        if [ `echo $?` -ne 0 ]; then
      #                  find . -name "${aname}" -print | cpio -pdmv ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/ > .out 2>&1 
                              cp -rfv ${aname} ${BANGU}/GFGH/${YYYY}/${MM}/${DD}/
                        fi
                        let cont=cont+1
                  fi
            done
      let fj=$fj+1
      done
      if [ $cont -ge $TOTALDEARQUIVOS ]; then
            echo "ARQUIVOS GFGH JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
            smsevent GFGH
            echo 0
      else
            echo 1
      fi

fi

}


gprg () {

LABELI=$1
BANGU=$2

cd $ROPERM/model/dataout/T${TRC}L${LV}
mkdir -p ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/

aaa=0
itc=0
test=0
f=0

a=`pwd`

set +x
echo "COPIANDO GPRG... `ls -ltr ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/*${LABELI}* | wc -l`"
set -A ftype       icn   dic     inz    din    fct    dir  files
set -A fsize    11247656 1941 11247656 1941 11247656 1941  9900
TOTALDEARQUIVOS=731

echo "VERIFICANDO..."

fj=0
cont=0
while [ $fj -lt ${#ftype[*]} ]; do
      for anamesize in `find ${BANGU}/GPRG/${YYYY}/${MM}/${DD} -name "GPRG??[NP]${LABELI}*${ftype[$fj]}*" -exec ls -ltr {} \; | awk '{print $5":"$9}'`; do
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
                  let cont=cont+1
            fi
      done
let fj=$fj+1
done
if [ $cont -ge $TOTALDEARQUIVOS ]; then
      echo "ARQUIVOS GPRG JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
      smsevent GPRG
      echo 0
else
      
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
                        cmp ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/$aname $aname 
                        if [ `echo $?` -ne 0 ]; then
      #                  find . -name "$aname" -print | cpio -pdmv ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/ > .out 2>&1
                              cp -rfv ${aname} ${BANGU}/GPRG/${YYYY}/${MM}/${DD}/
                        fi
                        let cont=cont+1
                  fi
            done
      let fj=$fj+1
      done
      if [ $cont -ge $TOTALDEARQUIVOS ]; then
            echo "ARQUIVOS GPRG JA COPIADOS PARA A BANGU...: $cont OK ${LABELI}"
            smsevent GPRG
            echo 0
      else
            echo 1
      fi
  
fi


}

#ensmed
#plumes
#gfct
#gpos
#gfgn
#gfgh
#gprg
