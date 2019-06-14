#!/bin/ksh -x
. ../include/config.sx6

# ESTE SCRIPT VERIFICA OS ARQUIVOS E 
# SAIDA 0 - ARQUIVOS COMPLETOS E OK
# SAIDA DIFERENTE DE 0 - CONTINUAR COPIA ATEH SER OK
# INICIO 27112008


LABELI=$1
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\nrunensmedg.sx6 YYYYMMDDHH"
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

PERT=$2

NFCTDY=$FSCT
let NMEMBR=${NPERT}*2+1
PREFX=${PERT}

if [ ! -z $3 ]; then
      LIMPA=$3
else
      LIMPA=n # DEFAULT NAO FAZ LIMPEZA
fi

if [ -s $4 ]; then
      echo "ERRO: FALTA PARAMETRO. \nrunensmedg.sx6 YYYYMMDDHH n/h (n=nas, h=bangu)"
      exit 3
else
      if [ "$4" = "n" -o "$4" = "N" ]; then
            BANGU=$NAS
      else
            if [ "$4" = "h" -o "$4" = "H" ]; then
                  BANGU=$BANGU
            else
                  echo "ERRO: PARAMETRO 2 deve ser n ou h"
                  exit 4
            fi

      fi
fi

#########################################################################################################
#FUNCOES
ve () {
#!/bin/ksh
DIR=$1 # AREA NO DK DE ONDE SERAO COPIADOS OS ARQUIVOS COM PREFIXO ARQN (ORIGEM)
DIRBG=$2 # AREA NA BANGU ONDE OS ARQUIVOS COM PREFIXO ARQN SERAO COPIADOS (DESTINO)
ARQN=$3 # PREFIXO A SER VERIFICADO 
siztot=$4 # SOMA DO TAMANHO DE TODOS OS ARQUIVO, CONFORME PREFIXO DE ARQN
qtdtot=$5 # QUANTIDADE DE ARQUIVOS COM O PREFIXO ARQN

rm -f ${DIR}/.arqmail.txt
cat << EOF > ${DIR}/.arqmail.txt
Possiveis arquivos com problemas:

EOF

mkdir -p ${DIRBG}
#smslabel Rodada "${LABELI}"

cd ${DIRBG} # ENTRA NO DIRETORIO DA BANGU, ONDE SERAO CONTADOS OS ARQUIVOS
gfct=`find . -name "${ARQN}*" -exec ls -ltr {} \; |awk 'BEGIN { s=c=0 } { s+=$5;c+=1 } END { printf( "%d:%d",c,s )}'` # CONTA QUANTOS ARQUIVOS E SOMA O TAMANHO DE TODOS OS ARQUIVOS CRIADOS
gfctnarq=`echo $gfct | cut -d: -f1` # CAPTURA DA VARIAVEL gfct APENAS O NOME DO ARQUIVO
gfctsarq=`echo $gfct | cut -d: -f2` # CAPTURA DA VARIAVEL gfct APENAS O TAMANHO DO ARQUIVO
gfctsarq=0
gfctnarq=0
if [ $gfctnarq -ge ${qtdtot} -a $gfctsarq -ge ${siztot} ]; then # COMPARA COM OS CALORES CORRETOS A QUANTIDADE E A AREA TOTAL OCUPADA PELOS ARQUIVOS
      echo "Arquivos ${ARQN} Ok - 1 Copia"
else # EM CASO DE ERRO NOS ARQUIVOS DA BANGU
      echo "Erro na copia dos arquivos ${ARQN}, Refazendo..."
      for arqnzr in `ls -ltr ${DIR}/${ARQN}* | awk '{print $5":"$9}'`; do # VERIFICA NO DK O NOME E O TAMANHO DOS ARQUIVOS, UM A UM.
            arqnr=`echo "$arqnzr" | cut -d: -f2` # NOME DO ARQUIVO DK
            arqzr=`echo "$arqnzr" | cut -d: -f1` # TAMANHO DO ARQUIVO DK
            
            arqnr=`basename $arqnr` # NOME SEM DIRETORIOS
            
            arqnzb=`ls -ltr ${DIRBG}/${arqnr}* | awk '{print $5":"$9}'` # VERIFICA NA BANGU O NOME E O TAMANHO DOS ARQUIVOS, UM A UM.
            arqnb=`echo "$arqnzb" | cut -d: -f2` # NOME DO ARQUIVO BANGU
            arqzb=`echo "$arqnzb" | cut -d: -f1` # TAMANHO DO ARQUIVO BANGU
            
            arqnb=`basename $arqnb` # NOME SEM DIRETORIOS
#            echo ${DIR}/$arqnb ${DIRBG}/$arqnb
            cmp ${DIR}/$arqnb ${DIRBG}/$arqnb # COMPARA ARQUIVO DA BANGU E DO DK
            comp=$? # VERIFICA SAIDA DA COMPARACAO: 0 OK, DIFERENTE DE 0 ERRO
            
            if [ \( "$arqnb$arqzb" = "$arqnr$arqzr" \) -a \( $comp -eq 0 \) ]; then # COMPARA A STRING NOMETAMANHO DO DK E BANGU, CASO SEJAM DIFERENTES INICIA A COPIA DO ARQUIVO EM QUESTAO
                  echo "$arqnb - $arqzb : OK"
            else
                  echo "\nCOPIANDO ARQUIVO COM PROBLEMAS..."
#                  a=`pwd`
#                  cd $DIR
#                  find . -name "$arqnr" -print | cpio -pdmv ${DIRBG}
#                  cd $a
                  #smslabel Info "Possivel Problema com $arqnr"
                  #smslabel STATUS "COPIANDO ${DIR}/$arqnr para ${DIRBG}"
                  cp -rfv ${DIR}/$arqnr ${DIRBG}
                  echo " ${DIR}/$arqnr - ${DIRBG}" >> ${DIR}/.arqmail.txt
                  echo "\n"
            fi
      done
      # VERIFICA NOVAMENTE A AREA TOTAL OCUPADA PELOS ARQUIVOS E A QUANTIDADE
      cd ${DIRBG}
      gfct=`find . -name "${ARQN}*" -exec ls -ltr {} \; |awk 'BEGIN { s=c=0 } { s+=$5;c+=1 } END { printf( "%d:%d",c,s )}'`
      gfctnarq=`echo $gfct | cut -d: -f1`
      gfctsarq=`echo $gfct | cut -d: -f2`
      if [ $gfctnarq -ge ${qtdtot} -a $gfctsarq -ge ${siztot} ]; then
            #smslabel Rodada "${LABELI}"
            #smslabel Info "Copia Ok..."
            #smslabel STATUS "..."
            echo "Arquivos ${ARQN} Copiados e Ok - Varias Copias"
      else
            echo "Arquivos ${ARQN} Copiados e Possiveis Problemas Revisados "
            mail alex.fernandes@cptec.inpe.br -s "ARQUIVOS ENSEMBLE COM PROBLEMAS ${LABELI}" < ${DIR}/.arqmail.txt
            rm -f ${DIR}/.arqmail.txt
            #exit 1
      fi
fi
};

limpa () {
#!/bin/ksh
DIR=$1
ARQN=$2
TEMPO=$3

echo "++ REMOVENDO $DIR/$ARQN..."
find ${DIR} -name "${ARQN}*" -mmin +${TEMPO} -exec rm -fv {} \;

};

# FIM FUNCOES

#########################################################################################################

echo "++ VERIFICANDO ARQUIVOS DE SAIDA DO MODELO ENSEMBLE T${TRC}L${LV}\n"

#ARQUIVOS GFCT
DIR=${ROPERM}/model/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/GFCT/${YYYY}/${MM}/${DD}
ARQN=GFCT${PERT}${LABELI}
siztot=1329213430 # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=125        # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

#ARQUIVOS GFGH
DIR=${ROPERM}/model/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/GFGH/${YYYY}/${MM}/${DD}
ARQN=GFGH${PERT}${LABELI}
siztot=84037341  # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=3         # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

#ARQUIVOS GPRG
DIR=${ROPERM}/model/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/GPRG/${YYYY}/${MM}/${DD}
ARQN=GPRG${PERT}${LABELI}
#siztot=247495490  # USE O LSOMA PARA OBTER O VALOR CORRETO
#qtdtot=45         # USE O LSOMA PARA OBTER O VALOR CORRETO
siztot=1           # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=1           # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

echo "\n++ VERIFICANDO ARQUIVOS DE POS-PROCESSAMENTO DO MODELO ENSEMBLE T${TRC}L${LV}\n"
#ARQUIVOS GPOS
DIR=${ROPERM}/pos/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/GPOS/${YYYY}/${MM}/${DD}
ARQN=GPOS${PERT}${LABELI}
siztot=1764855944 # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=187        # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

#ARQUIVOS GFGN
DIR=${ROPERM}/pos/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/GRH/${YYYY}/${MM}/${DD}
ARQN=GFGN${PERT}${LABELI}
siztot=18215726 # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=2        # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

#ARQUIVOS GFGN
DIR=${ROPERM}/pos/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/GRH/${YYYY}/${MM}/${DD}
ARQN=[PLI][rod][ece][fan]???${PERT}${LABELI}
siztot=63133   # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=3       # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

if [ "$PERT" = "01N" ]; then # FAZ VERIFICACAO DO ENSMED E PLUMES APENAS PARA SE PERT=01N

echo "\n++ VERIFICANDO ARQUIVOS DE ENSEMBLE MEDIO DO MODELO ENSEMBLE T${TRC}L${LV}\n"

#ARQUIVOS GPOSENM
DIR=${ROPERM}/ensmed/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/ENSMED/${YYYY}/${MM}/${DD}
ARQN=GPOSENM${LABELI}
siztot=1764650894 # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=187        # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

echo "\n++ VERIFICANDO ARQUIVOS PLUMES MEDIO DO MODELO ENSEMBLE T${TRC}L${LV}\n"

#ARQUIVOS PLUMES
DIR=${ROPERM}/plumes/dataout/T${TRC}L${LV}
DIRBG=${BANGU}/PLUMES/${YYYY}/${MM}/${DD}
ARQN=*${LABELI}2
siztot=667018627 # USE O LSOMA PARA OBTER O VALOR CORRETO
qtdtot=32        # USE O LSOMA PARA OBTER O VALOR CORRETO
ve $DIR $DIRBG $ARQN $siztot $qtdtot

fi

if [ "$LIMPA" = "s" ]; then

      # VARIAVEL TEMPO APRESENTADA EM MINUTOS: 720 = 30 horas, 2880 = 120 horas, etc.

      # PLUMES
      DIR=${ROPERM}/plumes/dataout/T${TRC}L${LV}
      ARQN='*20'
      TEMPO=1440
      limpa ${DIR} *${ARQN} ${TEMPO}


      # ENSMED
      DIR=${ROPERM}/ensmed/dataout/T${TRC}L${LV}
      ARQN=GPOSENM
      TEMPO=840
      limpa ${DIR} ${ARQN} ${TEMPO}

      # GFCT
      DIR=${ROPERM}/model/dataout/T${TRC}L${LV}
      ARQN=GFCT${PERT}
      TEMPO=480 # NAO MANTEM NADA NA AREA DO DK
      limpa ${DIR} ${ARQN} ${TEMPO}

      # GFGH
      DIR=${ROPERM}/model/dataout/T${TRC}L${LV}
      ARQN=GFGH${PERT}
      TEMPO=480 # NAO MANTEM NADA NA AREA DO DK
      limpa ${DIR} ${ARQN} ${TEMPO}

      # GPRG
      DIR=${ROPERM}/model/dataout/T${TRC}L${LV}
      ARQN=GPRG${PERT}
      TEMPO=2880
      limpa ${DIR} ${ARQN} ${TEMPO}
      
      # GFGN
      DIR=${ROPERM}/pos/dataout/T${TRC}L${LV}
      ARQN=GFGN${PERT}
      TEMPO=480
      limpa ${DIR} ${ARQN} ${TEMPO}
      
      # GPOS
      DIR=${ROPERM}/pos/dataout/T${TRC}L${LV}
      ARQN=GPOS${PERT}
      TEMPO=1300
      limpa ${DIR} ${ARQN} ${TEMPO}
fi


exit 0
