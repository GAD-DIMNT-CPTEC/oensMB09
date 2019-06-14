#!/bin/ksh
#
# CRIADO POR ALEX A. FERNANDES
# DATA: 31/03/2009
# EXTRAI CAMPOS DO ARQUIVO COMPACTADO PARA ARMAZENAMENTO USANDO HEAD TAIL
# EXTRAI TODOS OS CAMPOS DE UMA RODADA, OU VARIAVEIS A ESCOLHA
# USO DE ARQUIVO DE CONFIGURACAO PARA EXTRACAO
# 
# DEVE SER USADO NOS ARQUIVOS HISTORICOS DO TIGGE
#
# FUNCOES: VERIFICA_ENTRADA, EXTRAI_TUDO E EXTRAI_PARCIAL.
#
# EXEMPLO DE ARQUIVO DE CONFIGURACAO 
#
#VARIAVEIS DE CONFIGURACAO
#OPTION: P OU C - PARCIAL OU COMPLETA (EXTRAI TODOS OS CAMPOS DOS ARQUIVOS QUE ESTIVEREM PRESENTES NO DIRETORIO DE ENTRADA)
#LABELI LABELF: DATA INICIAL e DATA FINAL DA EXTRACAO
#INTERVALO: INTERVALO ENTRE RODADAS 12 para RODADAS 00Z e 12Z ou 24 PARA RODADAS DE ACORDO COM A DATA PASSADA.
#INT1 INT2: PREVISOES A SEREM EXTRAIDAS. 0000 0168 EXTRAI TODAS AS PREVISOES DE 0000 a 0168.
#MEMBROS: 000 001 002 003 004 005 006 007 008 009 010 011 012 013 014
#NIVEIS: para superficie 0000 e 1000 0925 0850 0700 0500 0300 02500 0200 0050
#VARIAVEIS: v u t 2t 10v 10u q gh tp tcw tcc st ssr sp skt sf orog msl lsm 
#DIRCATIN: DIRETORIO DE LEITURA DOS ARQUIVOS CONCATENADOS (COMPRIMIDOS)
#DIRCATOUT: DIRETORIO PARA CRIACAO DOS ARQUIVOS GRIB2 (EXPANDIDOS)
#ARQINITIGGE: z_tigge_c_sbsj_ EM CASO DE ARQUIVOS OPERACIONAIS.
#EX:
#
#p
#2009033000 2009033000
#24
#0000 0168
#000
#0000 0925 0500
#u v gh
#${ROPERM}/tigge/dataout/concat
#${ROPERM}/tigge/dataout/concatout
#z_tigge_c_sbsj_

echo "INICIANDO... \(set \-x\)"
set -x

. ../include/config.sx6

set +x

function Verifica_Entrada {
#typeset -RZ4 INT1 INT2 NIVEIS
#typeset -RZ3 MEMBROS

read OPTION
read LABELI LABELF
read INTERVALO
read INT1 INT2
read MEMBROS
read NIVEIS
read VARIAVEIS
read DIRCATIN
read DIRCATOUT
read ARQINITIGGE

# VERIFICACAO DAS DATAS DE ENTRADA E SAIDA              
if [ \( ${#LABELI} -eq 10 \) -a \( ${#LABELF} -eq 10 \) ]; then
      .
else
      echo "ERRO NA DEFINICAO DE LABELI LABELF (yyyymmddhh)"
      exit 2
fi

# VERIFICACAO DO INTERVALO ENTRE AS RODADAS A SEREM EXTRAIDAS
if [ \( ${INTERVALO} -ne 24 \) -a \( ${INTERVALO} -ne 12 \) ]; then
      echo "INTERVALO DAS RODADAS INCORRETO: DEVE SER 12 ou 24".
      exit 3
fi

# VERIFICACAO DO INTERVALO DE PREVISAO 000
if [ \( `echo "$INT1/6" | bc -l | cut -d. -f2` -ne 0 \) -a `echo "$INT2/6" | bc -l | cut -d. -f2` ]; then
      echo "INICIO E FIM DAS PREVISOES DEVEM SER MULTIPLOS DE 6 (0000, 0006, 0012, 0018, etc)."
      exit 4
fi

echo " "
echo $LABELI $LABELF
echo $INTERVALO
echo $INT1 $INT2
echo $MEMBROS
echo $NIVEIS
echo $VARIAVEIS
echo $DIRCATIN
echo $DIRCATOUT
echo $ARQINITIGGE

#VERIFICACAO DOS MEMBROS
if [ -n "$MEMBROS" ]; then
      if [ "$MEMBROS" = "all" -o "$MEMBROS" = "ALL" ]; then
            MEMBROS="000 001 002 003 004 005 006 007 008 009 010 011 012 013 014"
      fi
else
      echo "ERRO: MEMBROS NAO ENCONTRADAS"
      exit 3
fi 

#VERIFICACAO DOS NIVEIS
if [ -n "$NIVEIS" ]; then
      if [ "$NIVEIS" = "all" -o "$NIVEIS" = "ALL" ]; then
            NIVEIS="0000 1000 0925 0850 0700 0500 0300 0250 0200 0050"
      fi
else
      echo "ERRO: NIVEIS NAO ENCONTRADAS"
      exit 4
fi 

#VERIFICACAO DOS VARIAVEIS
if [ -n "$VARIAVEIS" ]; then
      if [ "$VARIAVEIS" = "all" -o "$VARIAVEIS" = "ALL" ]; then
            VARIAVEIS="v u t 2t 10v 10u q gh tp tcw tcc st ssr sp skt sf orog msl lsm"
      fi
else
      echo "ERRO: VARIAVEIS NAO ENCONTRADAS"
      exit 5
fi 

if [ ! -d $DIRCATIN  ]; then
      echo "ERRO: DIRETORIO DE ENTRADA NAO ENCONTRADO"
      exit 6
fi 
if [ ! -d $DIRCATOUT ]; then
      echo "ERRO: DIRETORIO DE SAIDA NAO ENCONTRADO"
      exit 7
fi 
if [ -s $ARQINITIGGE ]; then
      echo "ERRO: ARQUIVOS NAO ENCONTRADOS"
      exit 8
fi 

echo " "

}

function Extrai_Parcial {

labeli=$LABELI
while [ $labeli -le $LABELF ]; do
      labels=`echo $labeli | cut -c 1-8`
      hh=`echo $labeli | cut -c 9-10`
      typeset -RZ3 membros
      for membros in $MEMBROS
      do
            typeset -RZ4 i
            typeset -RZ4 j
            i=$INT1; j=$INT2 #INTERVALO DE PREVISAO
            if [ `echo $NIVEIS | grep 0000 | wc -l` -eq 1 ]; then
                  slpl=sl
            else
                  slpl=pl
            fi

            if [ \( `echo $NIVEIS | grep 0000  | wc -l` -eq 1 \) -a \
                 \( `echo $NIVEIS | grep 1000  | wc -l` -ge 1 \)  -o \
                 \( `echo $NIVEIS | grep  925  | wc -l` -ge 1 \) -o \
                 \( `echo $NIVEIS | grep  850  | wc -l` -ge 1 \) -o \
                 \( `echo $NIVEIS | grep  700  | wc -l` -ge 1 \) -o \
                 \( `echo $NIVEIS | grep  500  | wc -l` -ge 1 \) -o \
                 \( `echo $NIVEIS | grep  300  | wc -l` -ge 1 \) -o \
                 \( `echo $NIVEIS | grep  250  | wc -l` -ge 1 \) -o \
                 \( `echo $NIVEIS | grep  200  | wc -l` -ge 1 \) -o \
                 \( `echo $NIVEIS | grep   50  | wc -l` -ge 1 \) \
                 ]; then
                  slpl="??"
            fi
            while [ $i -le $j ]; do
                  for arqcat in `find $DIRCATIN -name "$ARQINITIGGE*${labeli}00*${slpl}_${i}_${membros}.dsct" -print`
                  do
                        arqgrbtigge=`echo $arqcat | sed -e s%dsct%grib%g`
#                        echo "+ EXTRAINDO ARQUIVOS DE: $arqcat"
                        for niveis in $NIVEIS
                        do
                              for variaveis in $VARIAVEIS
                              do
                                    rec=`grep ${niveis}_${variaveis}. $arqcat | wc -l`
                                    if [ $rec -ge 1 ]; then
                                          rec=`grep -n ${niveis}_${variaveis}. $arqcat | cut -d: -f1`
                                          recname=`grep -n ${niveis}_${variaveis}. $arqcat | cut -d: -f2`
                                          echo "${niveis}_${variaveis}.grib" $arqcat $rec
                                          irec=0
                                          jrec=0
                                          reci=1
#irec INCREMENTO DO TAMANHO DOS REGISTROS
#jrec TAMANHO DO CAMPO DESEJADO (ULTIMO CAMPO LIDO)
#reci INCREMENTO DOS CAMPOS
#rec POSICAO DO CAMPO DESEJADO NO ARQUIVO .GRIB
#recname NOME DO CAMPO A SER EXTRAIDO DE ACORDO COM ARQUIVO .DSCT
#recat TAMANHO DO CAMPO QUE ESTAH SENDO LIDO NO MOMENTO, USADO PARA O INCREMENTO DOS BYTES
#                                          
# A CONCATENACAO FUNCIONA SOMANDO TODOS OS REGISTROS ANTERIORES AO CAMPO DESEJADO.
# ESTA SOMA DE BYTES, INDICA QUANTOS BYTES TEM ATEH O CAMPO DESEJADO + O TAMANHO DO CAMPO
# NA ULTIMA LEITURA DO LOOP DE REGISTROS, EH GRAVADO O TAMANHO DO CAMPO EM JREC.
# USAMOS head PARA DEFINIR O TOTAL DE BYTES QUE DEVEM SER LIDOS ATEH O CAMPO E
# tail PARA LER APENAS OS ULTIMOS XXXXX BYTES CORRESPONDENTES AO CAMPO DESEJADO.                                          
                                          while [ $reci -le $rec ]; do
                                                recat=`cat $arqcat | head -$reci | tail -1 | cut -d: -f2`
                                                let irec=$irec+$recat
                                                if [ $reci -le $rec ]; then                                          
                                                      let jrec=$recat
                                                fi
                                                let reci=$reci+1
                                          done
#                                          echo ${niveis}_${variaveis}. $arqcat
#                                          echo "EXTRAINDO ${DIRCATOUT}/${recname}"
                                          cat ${arqgrbtigge} | head --byte=${irec} | tail --byte=${jrec} > ${DIRCATOUT}/${recname}
                                    fi
                              done                        
                        done
                        
                  done
            let i=$i+6
            done
      done
      set -x
labeli=`date -d "$labels ${hh}:00 $INTERVALO hours" +"%Y%m%d%H"`
set +x
done

}

function Extrai_Tudo {

echo "+ DIRETORIO DE ENTRADA: ${DIRCATIN}\n+ DIRETORIO DE SAIDA: ${DIRCATOUT}"
echo "+ TODOS OS ARQUIVOS CONTIDOS NO DIRETORIO DE ENTRADA SERAO EXPANDIDOS\nPARA O DIRETORIO DE SAIDA...\n"

for arqdsct in `find ${DIRCATIN} -name "${ARQINITIGGE}${LABELI}*dsct" -print`
do
    rinc=0
    arqgrbtigge=`echo $arqdsct | sed -e s%"dsct"%"grib"%g`
    for arq in `cat $arqdsct`
    do
        ARQSAID=`echo $arq | cut -d: -f1`
        registr=`echo $arq | cut -d: -f2`
        let rinc=$rinc+${registr}  

        echo "GERANDO: ${DIRCATOUT}/${ARQSAID}\c"
        cat ${arqgrbtigge} | head --byte=${rinc} | tail --byte=${registr} > ${DIRCATOUT}/${ARQSAID}

        # MODULO DE TESTE DOS ARQUIVOS - NAO USAR OPERACIONALMENTE"
#        cmp ${DIRCATOUT}/${ARQSAID} /rede/nas/modoper/tigge/z_tigge_c_sbsj_${LABELI}0000_glob/${ARQSAID}
        if [ $? -eq 0 ]; then
            echo " OK!"
        else
            echo " ERRO!"
        fi
        # FIM DO MODULO DE TESTE
    done
done
}

Verifica_Entrada

if [ $OPTION = "p" -o $OPTION = "P" ]; then
      echo "EXECUTANDO EXTRACAO PARCIAL..."
      Extrai_Parcial
else
      if [ $OPTION = "c" -o $OPTION = "C" ]; then
            echo "EXECUTANDO EXTRACAO COMPLETA..."
            Extrai_Tudo
      else
            echo "OPCAO INCORRETA: c/p\n- C PARA EXTRACAO COMPLETA\n- P PARA EXTRACAO PARCIAL COM ARQUIVO DE CONFIGURACAO (VEJA EX. NO CODIGO)"
      fi
fi
exit 0

