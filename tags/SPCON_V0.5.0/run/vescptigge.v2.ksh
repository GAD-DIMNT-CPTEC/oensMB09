#!/bin/ksh

set +x

. ../include/config.sx6

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

if [ -s $PREFX ]; then
      echo "ERRO - PARAMETRO PERT\nFORMATO: runrectigge.sx6 yyyymmddhh 01N"
      exit 2
fi

set +x

ARQINI=sbsj_${LABELI}00_glob_prod

LABELS=`echo ${LABELI} | cut -c 1-8`

PERT=$2

if [ $PERT -eq 000 ]; then p=AVN; fi;
if [ $PERT -eq 001 ]; then p=01N; fi;
if [ $PERT -eq 002 ]; then p=02N; fi;
if [ $PERT -eq 003 ]; then p=03N; fi;
if [ $PERT -eq 004 ]; then p=04N; fi;
if [ $PERT -eq 005 ]; then p=05N; fi;
if [ $PERT -eq 006 ]; then p=06N; fi;
if [ $PERT -eq 007 ]; then p=07N; fi;
if [ $PERT -eq 008 ]; then p=01P; fi;
if [ $PERT -eq 009 ]; then p=02P; fi;
if [ $PERT -eq 010 ]; then p=03P; fi;
if [ $PERT -eq 011 ]; then p=04P; fi;
if [ $PERT -eq 012 ]; then p=05P; fi;
if [ $PERT -eq 013 ]; then p=06P; fi;
if [ $PERT -eq 014 ]; then p=07P; fi;

VECOPIA (){
labeli=$1
dir=$2

echo "+ COPIANDO DE $dir para ldm@${tiggeldm}:${tiggedirmopora}/scratch/${ARQINI}"

#tiggedirmopora=/usr/local/ldm/tigge/data
tiggescratch=${tiggedirmopora}/scratch/${ARQINI}
tiggeoutgoing=${tiggedirmopora}/outgoing

cd $dir
ssh ldm@${tiggeldm} "mkdir $tiggescratch"
#ssh ldm@mopora       "mkdir $tiggescratch"
ssh ldm@${tiggeldm} "ls -ltr $tiggescratch" > .arqtmopora.temp$PERT
echo "scp \\" > .aaaaa.temp.ksh$PERT

for arq in `find . -name "z_tigge*${labeli}*prod*_${PERT}_*" -print`; do
arqnammoporascratch=`cat .arqtmopora.temp$PERT | grep $arq | wc -l`
if [ $arqnammoporascratch -ge 1 ]; then
#      echo "COPIADO... $arq"
.
else
#      echo scp $dir/$arq ldm@${tiggeldm}:${tiggedirmopora}/scratch/${ARQINI}
      echo " $dir/$arq \\" >> .aaaaa.temp.ksh$PERT
#      scp $dir/$arq ldm@${tiggeldm}:${tiggedirmopora}/scratch/${ARQINI}
fi

done
echo " ldm@${tiggeldm}:${tiggedirmopora}/scratch/${ARQINI}" >> .aaaaa.temp.ksh$PERT
chmod 750 .aaaaa.temp.ksh$PERT
./.aaaaa.temp.ksh$PERT
cat .aaaaa.temp.ksh$PERT | sed -e s%${tiggeldm}%mopora.cptec.inpe.br% > .aaaaa.temp.ksh$PERT.mopora
echo "\n\n\nCOPIANDO MOPORA\n\n\n"
chmod 700 ./.aaaaa.temp.ksh$PERT.mopora
#./.aaaaa.temp.ksh$PERT.mopora
rm -f .aaaaa.temp.ksh$PERT .aaaaa.temp.ksh$PERT.mopora
}


VERIFICA () {
n=48542

if [ $PERT -eq 000 ]; then
      np=3350
else
      np=3228
fi

labeli=$1
dir=$2

#tiggedirmopora=/usr/local/ldm/tigge/data
tiggescratch=${tiggedirmopora}/scratch/${ARQINI}
tiggeoutgoing=${tiggedirmopora}/outgoing
set -x
arqnummoporascratch=`ssh ldm@${tiggeldm} "find $tiggescratch -name \"z_tigge_c_sbsj_${labeli}*prod*_${PERT}_*\" -print" | wc -l`
set +x
if [ $arqnummoporascratch -ge $np ]; then
echo 0 #CONTAGEM CORRETA DE ARQUIVOS NA MOPORA
else
arqnumturinas=`find $dir -name "z_tigge*${labeli}*prod*_${PERT}_*" -print| wc -l`
if [ $arqnumturinas -lt $np ]; then
      echo 1 #CONTAGEM INCORRETA DE ARQUIVOS NO dir e INCORRETA NA MOPORA 
else
      echo 2 #CONTAGEM CORRETA DE ARQUIVOS NO dir e INCORRETA NA MOPORA 
fi

fi

}

echo "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
echo "$p"
dir=${ROPERM}/tigge/dataout
saida=`VERIFICA ${LABELI} $dir`
if [ $saida -eq 0 ]; then
      echo "+ ARQUIVOS CORRETOS na MOPORA"
fi
if [ $saida -eq 1 ]; then
      echo "+ ARQUIVOS INCORRETOS em: $dir e PROBLEMAS na MOPORA"
      dir=${tiggenas}/z_tigge_c_sbsj_${LABELI}0000_glob/
      saida=`VERIFICA ${LABELI} $dir`
      if [ $saida -eq 0 ]; then
            echo "+ ARQUIVOS CORRETOS na MOPORA"
      fi
      if [ $saida -eq 1 ]; then
            echo "+ ARQUIVOS INCORRETOS em: $dir e PROBLEMAS na MOPORA"
            echo "++ REFAZER MEMBRO: $p"
            exit 1
      fi
      if [ $saida -eq 2 ]; then
            echo "++ ARQUIVOS CORRETOS em: $dir e PROBLEMAS na MOPORA"
            VECOPIA ${LABELI} $dir
            saida=`VERIFICA ${LABELI} $dir`
      fi
fi
if [ $saida -eq 2 ]; then
      echo "++ ARQUIVOS CORRETOS em: $dir e PROBLEMAS na MOPORA"
      VECOPIA ${LABELI} $dir
fi
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"

#VECOPIA ${LABELI} ${tiggenas}/z_tigge_c_sbsj_${LABELI}0000_glob/

exit 0

f=1

if [ $f -eq 1 ]; then
arqnumturinas=`find ${tiggenas}/${ARQINI}/ -name "z_tigge*${LABELI}*prod*" -print| wc -l`
if [ $arqnumturinas -lt $n ]; then
      nas=1
else
      nas=0
fi
if [ $dk9 -eq 1 ]; then
      echo "COPIANDO DO DK, REPASSANDO ERROS..."
fi



arqnumturidk=`find ${ROPERM}/tigge/dataout/T${TRC}L${LV} -name "z_tigge*${LABELI}*prod*" -print| wc -l`
if [ $arqnumturidk -lt $n ]; then
      dk9=1
else
      dk9=0
fi
if [ $nas -eq 1 ]; then
      echo "COPIANDO DO NAS, REPASSANDO ERROS..."
fi




exit 0
