#!/bin/ksh

. ../include/config.sx6

LABELI=$1 #YYYYMMDDHH

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

echo "+ COPIANDO DE $dir para ldm@mopora:${tiggedirmopora}/scratch/${ARQINI}"

tiggedirmopora=/usr/local/ldm/tigge/data
tiggescratch=${tiggedirmopora}/scratch/${ARQINI}
tiggeoutgoing=${tiggedirmopora}/outgoing

cd $dir
for arq in `find . -name "z_tigge*${labeli}*prod*_${PERT}_*" -print`; do
arqnammoporascratch=`ssh ldm@mopora "ls -ltr $tiggescratch/$arq" | wc -l`
if [ $arqnammoporascratch -ge 1 ]; then
#      echo "COPIADO... $arq"
.
else
      echo scp $dir/$arq ldm@mopora:${tiggedirmopora}/scratch/${ARQINI}
      scp $dir/$arq ldm@mopora:${tiggedirmopora}/scratch/${ARQINI}
fi

done

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

tiggedirmopora=/usr/local/ldm/tigge/data
tiggescratch=${tiggedirmopora}/scratch/${ARQINI}
tiggeoutgoing=${tiggedirmopora}/outgoing
arqnummoporascratch=`ssh ldm@mopora "find $tiggescratch -name \"z_tigge_c_sbsj_${labeli}*prod*_${PERT}_*\" -print" | wc -l`

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
      dir=/rede/nas/io_dop/tigge/z_tigge_c_sbsj_${LABELI}0000_glob/
      saida=`VERIFICA ${LABELI} $dir`
      if [ $saida -eq 0 ]; then
            echo "+ ARQUIVOS CORRETOS na MOPORA"
      fi
      if [ $saida -eq 1 ]; then
            echo "+ ARQUIVOS INCORRETOS em: $dir e PROBLEMAS na MOPORA"
            echo "++ REFAZER MEMBRO: $p"
      fi
      if [ $saida -eq 2 ]; then
            echo "++ ARQUIVOS CORRETOS em: $dir e PROBLEMAS na MOPORA"
            VECOPIA ${LABELI} $dir
      fi
fi
if [ $saida -eq 2 ]; then
      echo "++ ARQUIVOS CORRETOS em: $dir e PROBLEMAS na MOPORA"
      VECOPIA ${LABELI} $dir
fi
echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"

#VECOPIA ${LABELI} /rede/nas/io_dop/tigge/z_tigge_c_sbsj_${LABELI}0000_glob/

exit 0

f=1

if [ $f -eq 1 ]; then
arqnumturinas=`find /rede/nas/io_dop/tigge/${ARQINI}/ -name "z_tigge*${LABELI}*prod*" -print| wc -l`
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
