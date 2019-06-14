#!/bin/ksh


echo "INICIANDO... \(set \-x\)"
set -x

. ../include/config.sx6

arqin=$1
#LER ARQUIVO DE PARAMETROS PARA EXTRACAO.

# EX DE ARQIN
# TODAS AS VARIAVEIS: 
cat << EOF > /tmp/io8761923.txt
DATAINI:20090330
DATAFIM:20090330
MEMBROS:000
RODADAS:00
TPREVIS:0360
VARIAVE:all
NIVEISS:all
DIRENTR:/gfs/dk20/modoper/tempo/global/oens/tigge/dataout/concat
DIRSAID:/gfs/dk19/io_dop/users/alex/oenspro/tigge/dataout/concatout
ARQINIS:z_tigge_c_sbsj
EOF

if [ -s $arqin ]; then
      echo "EXTRAINDO COM ARQUIVO DE EXEMPLO..."
      sleep 05      
      arqin=/tmp/io8761923.txt
fi

#cat /tmp/io8761923.txt
#rm -f /tmp/io8761923.txt

set +x
typeset -RZ3 MEMBROS
typeset -RZ4 NIVEISS
typeset -RZ4 irod

# CAPTURA VARIAVEIS DO ARQUIVO DE DEFINICAO DE EXTRACAO
DATAINI=`cat $arqin | grep DATAINI | cut -d: -f2` # DATA DE INICIO DA EXTRACAO
DATAFIM=`cat $arqin | grep DATAFIM | cut -d: -f2` # DATA FINAL DA EXTRACAO
set -A MEMBROS `cat $arqin | grep MEMBROS | cut -d: -f2` # MEMBROS A SEREM EXTRAIDOS (000,001...014 ou all), separados por espaco em branco.
set -A RODADAS `cat $arqin | grep RODADAS | cut -d: -f2` # QUAIS RODADAS 00 ou 12 ou 00,12
TPREVIS=`cat $arqin | grep TPREVIS | cut -d: -f2` # TEMPO DE PREVISAO, MAX 360
set -A VARIAVE `cat $arqin | grep VARIAVE | cut -d: -f2` # VARIAVEIS SEPARADAS POR ESPACO E EM MINUSCULAS. all PARA TODAS AS VARIAVEIS
set -A NIVEISS `cat $arqin | grep NIVEISS | cut -d: -f2` # NIVEIS. Use all para todos os niveis
DIRSAID=`cat $arqin | grep DIRSAID | cut -d: -f2` # DIRETORIO DE SAIDA ONDE OS ARQUIVOS DEVEM SER ESCRITOS
DIRENTR=`cat $arqin | grep DIRENTR | cut -d: -f2` # DIRETORIO DE ENTRADA ONDE OS ARQUIVOS DEVEM SER ESCRITOS
ARQINIS=`cat $arqin | grep ARQINIS | cut -d: -f2` # PREFIXO DO ARQUIVO DE ENTRADA DSCT

if [ "${VARIAVE[0]}" = "all" ]; then
   set -A VARIAVE gh q t u v 10u 10v lsm msl orog skt sf st 2t sp ssr tcc tcw tp 
fi

mkdir -p ${DIRSAID}

if [ `echo "${MEMBROS[*]}" | grep "all" | wc -l` -ge 1 ]; then
   typeset -RZ3 MEMBROS
   set -A MEMBROS 000 001 002 003 004 005 006 007 008 009 010 011 012 013 014
fi
if [ `echo "${NIVEISS[*]}" | grep "all" | wc -l` -ge 1 ]; then
   typeset -RZ4 NIVEISS
   set -A NIVEISS 1000 0925 0850 0700 0500 0300 0250 0200 0050 0000
fi

#INICIA VERIFICACAO DE VARIAVEIS
if [ ${#RODADAS[*]} -eq 2 ]; then
   icd=12
   set -A RODADAS 00 12
else
   icd=24
fi
HH=${RODADAS[0]}

# VERIFICA PROBLEMAS NA DATA INICIAL
   if [ `echo ${DATAINI} | wc -c` -ne 9 ]; then
         echo "ERRO NA DEFINICAO DE DATA INICIAL (yyyymmdd)."
         exit 3
   fi
# VERIFICA PROBLEMAS NA DATA FINAL
   if [ `echo ${DATAFIM} | wc -c` -ne 9 ]; then
         echo "ERRO NA DEFINICAO DE DATA FINAL (yyyymmdd)."
         exit 3
   fi

echo "\n+ INTERVALO DE EXTRACAO: $DATAINI - $DATAFIM"
echo "+ MEMBROS: ${MEMBROS[*]}"
echo "+ RODADAS: ${RODADAS[*]}"
echo "+ HORARIO PREVISAO: ${TPREVIS[*]}"
echo "+ VARIAVEIS PARA EXTRACAO: ${VARIAVE[*]}"
echo "+ NIVEIS PARA EXTRACAO: ${NIVEISS[*]}"
echo "+ DIRETORIO DE ENTRADA: ${DIRSAID}"
echo "+ DIRETORIO DE SAIDA: ${DIRENTR}"
echo "+ PREFIXO DO ARQUIVO DESCRITOR: ${ARQINIS}"
echo "\n"

#INICIA LOOPS DE EXTRACAO.
#0 - LOOP DE DATA
idata=${DATAINI}
while [ $idata -le ${DATAFIM} ]; do

#1 - LOOP DE RODADA
irod=0
while [ $irod -lt ${#RODADAS[*]} ]; do

#2 - LOOP DE MEMBROS 
imemb=0
while [ $imemb -lt ${#MEMBROS[*]} ]; do

   if [ `echo ${MEMBROS[$imemb]} | wc -c` -ne 4 ]; then
         echo "ERRO NA DEFINICAO DO MEMBRO (000 001 002 003 ... 014)."
         exit 3
   fi
   # ATRIBUI VALOR CF ou PF DE ACORDO COM O MEMBRO PARA CRIACAO DO NOME DO ARQUIVO
   echo "+ MEMBRO: ${MEMBROS[$imemb]}"
   if [ ${MEMBROS[$imemb]} -eq 000 ]; then
      cfpf=cf
   else
      cfpf=pf
   fi
   
#3 - LOOP DE NIVEIS
        iniv=0
        while [ $iniv -lt ${#NIVEISS[*]} ]; do
           if [ ${NIVEISS[$iniv]} -eq 0000 ]; then
               slpl=sl
           else
               slpl=pl
           fi
               echo "+ ARQUIVO DE ENTRADA: ${ARQINIS}_${idata}${RODADAS[$irod]}0000_glob_prod_${cfpf}_${slpl}_ - ${NIVEISS[$iniv]}_${VARIAVE[$ivar]}"
        let iniv=$iniv+1
        done
        

let imemb=imemb+1
done
#2 - FIM

let irod=$irod+1
done
#1 - FIM DO LOOP

idata=`date -d "${idata} ${HH}:00 + ${icd} hours" +"%Y%m%d"`
HH=`date -d "${idata} ${HH}:00 + ${icd} hours" +"%H"`
done
#0




exit 0























#1 - LOOP DE MEMBROS 
   imemb=0
   while [ $imemb -lt ${#MEMBROS[*]} ]; do

      if [ `echo ${MEMBROS[$imemb]} | wc -c` -ne 4 ]; then
            echo "ERRO NA DEFINICAO DO MEMBRO (000 001 002 003 ... 014)."
            exit 3
      fi
      # ATRIBUI VALOR CF ou PF DE ACORDO COM O MEMBRO PARA CRIACAO DO NOME DO ARQUIVO
      if [ ${MEMBROS[$imemb]} -eq 000 ]; then
         cfpf=cf
      else
         cfpf=pf
      fi


#2 - LOOP DE VARIAVEIS
      ivars=0
      while [ $ivars -lt ${#VARIAVE[*]} ]; do
      # VERIFICACAO DAS VARIAVEIS EM SINGLE LEVEL OU PRESSURE LEVEL
      # DEFINE VARIAVEL plsl PARA CRIACAO DO NOME DO ARQUIVO A SER EXTRAIDO
         if [ \( "${VARIAVE[$ivars]}" = "u" \) -o\
          \( "${VARIAVE[$ivars]}" = "v" \) -o\
          \( "${VARIAVE[$ivars]}" = "gh" \) -o\
          \( "${VARIAVE[$ivars]}" = "t" \) -o\
          \( "${VARIAVE[$ivars]}" = "q" \) ]; then
             plsl=pl
#             if [ "${NIVEISS[0]}" = "all" ]; then
#                typeset -RZ4 NIVEISS
#                set -A NIVEISS 1000 0925 0850 0700 0500 0300 0250 0200 0050
#             else
#                typeset -RZ4 NIVEISS
#                set -A NIVEISS `cat $arqin | grep NIVEISS | cut -d: -f2` # NIVEIS SEPARADOS POR VIRGULA. EM CASO DE VARIAVEIS SL USAR 0000
#             fi
         else
             plsl=sl
             typeset -RZ4 NIVEISS
             set -A NIVEISS 0000
         fi
      
#3 - LOOP DE NIVEIS
         iniv=0
         while [ $iniv -lt ${#NIVEISS[*]} ]; do
#        
            irod=0
            while [ $irod -le ${TPREVIS} ]; do
#               echo ${idata}${HH} - ${MEMBROS[$imemb]} - ${VARIAVE[$ivars]} - $plsl - ${NIVEISS[$iniv]}
         
               #GERA O NOME DO ARQUIVO .dsct A SER BUSCADO
               ARQDSCT="${ARQINIS}_${idata}${HH}0000_glob_prod_${cfpf}_${plsl}_${irod}_${MEMBROS[$imemb]}.dsct"
               #GERA O NOME DO ARQUIVO .grib A SER BUSCADO
               ARQGRIB="${ARQINIS}_${idata}${HH}0000_glob_prod_${cfpf}_${plsl}_${irod}_${MEMBROS[$imemb]}.grib"
               #GERA O NOME DO ARQUIVO .grib A SER extraIDO/CRIADO
               ARQSAID="${ARQINIS}_${idata}${HH}0000_glob_prod_${cfpf}_${plsl}_${irod}_${MEMBROS[$imemb]}_${NIVEISS[$iniv]}_${VARIAVE[$ivars]}.grib"
#               echo $ARQDSCT
               #regarq captura em qual registro do arquivo concatenado estah o arquivo a ser extraido
               regarq=`grep -n "${ARQINIS}_${idata}${HH}0000_glob_prod_${cfpf}_${plsl}_${irod}_${MEMBROS[$imemb]}_${NIVEISS[$iniv]}_${VARIAVE[$ivars]}.grib" ${DIRENTR}/${ARQDSCT} | cut -d: -f1`
               #regsiz captura o tamanho do campo a ser extraido do arquivo concatenado
               regsiz=`cat ${DIRENTR}/${ARQDSCT} | head -${regarq} | tail -1 | cut -d: -f2`
               #regnam captura o nome do arquivo que deve ser extraido
               regnam=`cat ${DIRENTR}/${ARQDSCT} | head -${regarq} | tail -1 | cut -d: -f1`
               
               # EXTRACAO DO ARQUIVO
               i=1
               registro=0
               while [ $i -le ${regarq} ]; do
               # Como os arquivos em grib2 nem sempre possuem o mesmo tamanho, todos os registros anteriores ao registro desejado sao 
               #incrementados para descobrir onde termina o arquivo a ser concatenado.
               # Esta informacao eh utilizada no cat extrator do arquivo, no proximo passo do script.
               
               #rtmp eh o tamanho do registro desejado.
               #registro eh a soma do tamanho de todos os registros incluindo rtmp (o registro desejado)
                  rtmp=`cat ${DIRENTR}/${ARQDSCT} | head -${i} | tail -1 | cut -d: -f2`
                  if [ -s $rtmp ]; then
                     rtmp=0
                  fi
                  registro=`echo "${registro}+${rtmp}" | bc -l`
                  let i=i+1
               done
               
               # Verifica se regsiz for diferente de zro. Caso o registro seja zero, o arquivo desejado nao existe na serie concatenada.
               if [ $regsiz -gt 0 ]; then
               # O cat eh o comando para a extracao do arquivo de dentro do arquivo concatenado
                  cat ${DIRENTR}/${ARQGRIB} | head --byte=${registro} | tail --byte=${rtmp} > ${DIRSAID}/${ARQSAID}
                  echo "EXTRAINDO: ${DIRSAID}/${ARQSAID}"
               else
                  echo "ARQUIVO NAO DISPONIVEL PARA A SERIE..."
               fi
               
            let irod=irod+6
            done
         
         let iniv=iniv+1
         done
#3 - FIM

      let ivars=ivars+1
      done
#2 - FIM

   let imemb=imemb+1
   done
#1 - FIM

idata=`date -d "${idata} ${HH}:00 + ${icd} hours" +"%Y%m%d"`
HH=`date -d "${idata} ${HH}:00 + ${icd} hours" +"%H"`
done

exit 0
