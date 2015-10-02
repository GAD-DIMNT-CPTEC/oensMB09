#!/bin/ksh

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

LABELS=`echo $LABELI | cut -c 1-8`
LABELF=`date -d "$LABELS 15 days" +"%Y%m%d${HH}"`

GRADS=/usr/local/grads/bin
WGRIB18=${OPERM}/tigge/bin/wgrib.tupay
ARQINI=z_tigge_c_sbsj_${LABELI}0000_glob
DIRINI=sbsj_${LABELI}00_glob_prod
tiggescratch=${tiggedirmopora}/scratch/${DIRINI}
tiggeoutgoing=${tiggedirmopora}/outgoing

# VERIFICA A POSSIBILIDADE DE TER OCORRIDO NOVA INSERCAO DOS DADOS VIA NOVA RODADA OU COPIA.
#  GERALMENTE NO DIRETORIO SENT, QUANDO A RODADA FOR INSERIDA MAIS DE UMA VEZ,
# TEREMOS UM DIRETORIO ${DIRINI}.ALGUMANUMERACAO
# POR ISSO VERIFICAMOS O ULTIMO DIRETORIO GERADO, PARA NAO REENVIARMOS INFINITAMENTE O MESMO DADO
tiggesentdi=`ssh -l ldm ${tiggeldm} "ls -ltr $tiggedirmopora/sent/ | grep ${LABELI} | tail -1"`
tiggesentdir=`echo $tiggesentdi | awk '{print $8}'`

      typeset -ZR3 MEMB
      set -A m AVN 01N 02N 03N 04N 05N 06N 07N 01P 02P 03P 04P 05P 06P 07P
      MEMB=000
      # VERIFICACAO NO DIRETORIO QUE FOI REENVIADO OS MEMBROS QUE ESTAVAM COM PROBLEMAS.
      scp ldm@${tiggeldm}:$tiggedirmopora/sent/$DIRINI/${ARQINI}_prod.manifest ./
      scp ldm@${tiggeldm}:$tiggedirmopora/sent/$tiggesentdir/${ARQINI}_prod.manifest ./${ARQINI}_prod.manifest.new
      while [ $MEMB -le 014 ]; do
      
            if [ $MEMB -eq 000 ]; then
                  comp=3350
            else
                  comp=3228
            fi
            a=`cat ./${ARQINI}_prod.manifest | grep _${MEMB}_ | grep prod | wc -l`
            if [ $a -eq $comp ]; then # 
                  echo "+++ MEMBRO ${m[$MEMB]} OK"  
            else
                  if [ $a -eq 0 ]; then
                        sa=` cat ./${ARQINI}_prod.manifest.new | grep _${MEMB}_ | grep prod | wc -l`
                        if [ $sa -eq $comp ]; then
                              echo "+++ MEMBRO ${m[$MEMB]} OK"  
                        else
                              echo "+++ MEMBRO ${m[$MEMB]} ERRO"
                              echo "REENVIO COM PROBLEMAS ${m[$MEMB]}"
                              ccc=0
                              for arq in `cat ${ARQINI}_prod.manifest.new`
                              do
                                    if [ `cat ${ARQINI}_prod.manifest | grep $arq | wc -l` -ne 1 ];then
                                          echo "ARQUIVO FALTANDO: $arq"
                                    fi
                              let ccc=ccc+1
                              done
                        fi
                  fi 
                  echo "ERRO NO MEMBRO ${m[$MEMB]} TOTAL=$ccc"
#                  smslabel ${m[$MEMB]} "+++ ERR"
                  f=0
            fi
      let MEMB=${MEMB}+1
      done
      rm -f ${ARQINI}_prod.manifest

exit 0
