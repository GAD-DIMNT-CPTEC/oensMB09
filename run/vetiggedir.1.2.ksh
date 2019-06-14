#!/bin/ksh

. ../include/config.sx6

LABELI=$1
if [ -s $LABELI ]; then
      echo "ERRO: FALTA PARAMETRO.\n YYYYMMDDHH s/n"
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
COPY=$2
if [ -s $COPY ]; then
      echo "ERRO: FALTA PARAMETRO.\n YYYYMMDDHH s/n - s faz copia / n nao faz copia"
      exit 1
fi

LABELS=`echo $LABELI | cut -c 1-8`
LABELF=`date -d "$LABELS 15 days" +"%Y%m%d${HH}"`

GRADS=/usr/local/grads/bin
WGRIB18=${OPERM}/tigge/bin/wgrib.tupay
ARQINI=z_tigge_c_sbsj_${LABELI}0000_glob
DIRINI=sbsj_${LABELI}00_glob_prod
tiggescratch=${tiggedirmopora}/scratch/${DIRINI}
tiggeoutgoing=${tiggedirmopora}/outgoing

# testa: funcao que verifica os diretorios, excluindo-se resend, e compara o numero de arquivos inseridos.
function testa {
      typeset -ZR3 MEMB
      set -A m AVN 01N 02N 03N 04N 05N 06N 07N 01P 02P 03P 04P 05P 06P 07P
      MEMB=000
      # VERIFICACAO NO DIRETORIO QUE FOI REENVIADO OS MEMBROS QUE ESTAVAM COM PROBLEMAS.
      
            # dirmo = todos os diretorios de reenvio + envio
            # ni = numero de diretorios de reenvio ( nao contando resend)
            dirmo=`ssh -l ldm ${tiggeldm} "ls $tiggedirmopora/sent/ | grep ${LABELI}00_ | grep -v resend"`
            ni=`ssh -l ldm ${tiggeldm} "ls $tiggedirmopora/sent/ | grep ${LABELI}00_ | grep -v resend | wc -l"`
            nmembok=0
            mo=
            msa1=
            msa2=
      while [ $MEMB -le 014 ]; do
      
            if [ $MEMB -eq 000 ]; then
                  comp=3350
            else
                  comp=3228
            fi
            
            nia=1
            a=0
            # SOMA O NUMERO DE ARQUIVOS EM TODOS OS MANIFEST PARA VERIFICAR O NUMERO DE ARQUIVOS ENVIADOS POR MEMBROS.
            while [ $nia -le $ni ]; do
                  dir=`echo $dirmo | cut -d" " -f${nia}`
                  if [ $MEMB -eq 000 ]; then
                        scp ldm@${tiggeldm}:$tiggedirmopora/sent/$dir/${ARQINI}_prod.manifest ./${ARQINI}_prod.manifest.$nia
                  fi
                  atemp=`cat ./${ARQINI}_prod.manifest.${nia} | grep _${MEMB}_ | grep prod | wc -l`
                  a=`echo $a + $atemp | bc -l`
            let nia=$nia+1
            done            
            
            if [ \( $a -ge $comp \) ]; then
                  echo "+++ MEMBRO ${m[$MEMB]} OK - $a ARQUIVOS"  
                      
                  smslabel Rodada "${LABELI} `date`"
                  smslabel STATUS "ARQUIVO RODADO..."
                  smslabel Info "+++ MEMBRO ${m[$MEMB]} OK"
                  let nmembok=$nmembok+1
                  mo=$MEMB
            else
                  echo "REENVIO COM PROBLEMAS ${m[$MEMB]}"
                  smslabel Rodada "${LABELI} `date`"
                  smslabel STATUS "REENVIO COM PROBLEMAS, POSSIVEL PROBLEMA NO POS"
                  smslabel Info "REFAZER MEMBRO E TIGGE DO MEMBRO ${m[$MEMB]}..." "RODAR COMANDO ssh ldm@${tiggeldm} \"mv $tiggescratch $tiggeoutgoing\" APOS COMPLETAR AS TAREFAS ACIMA"
                  msa1="$msa ${m[$MEMB]}"
                  msa2="$msa $MEMB"
                  f=0
            fi
      let MEMB=${MEMB}+1
      done
      if [ $nmembok -eq 15 ];then
            echo "Todos os Membros OK, Saindo..."
            rm -f ./*_prod.manifest*
            exit 0
      else
#            echo "problemas em um membro, tentando reenvio..."
            smslabel Rodada "${LABELI} `date`"
            smslabel STATUS "REENVIO COM PROBLEMAS, POSSIVEL PROBLEMA NO POS"
            smslabel Info "REFAZER MEMBRO E TIGGE DO MEMBRO(S) $msa" "RODAR COMANDO ssh ldm@${tiggeldm} \"mv $tiggescratch $tiggeoutgoing\" APOS COMPLETAR AS TAREFAS ACIMA" 

            scp ldm@${tiggeldm}:$tiggedirmopora/sent/$dir/${ARQINI}_prod.manifest ./${ARQINI}_prod.manifest

            for MEMB in `echo $mo`; do
            #MEMB = MEMBRO OK
            #mo =   MEMBRO COM PROBLEMAS
                  for arq in `cat ./${ARQINI}_prod.manifest | grep _${MEMB}_ | grep prod`; do
                        atemp=`echo $arq | sed -e s%"_${MEMB}_"%"_${msa2}_"%g | sed -e s%" "%""%g`
                        #echo $arq $atemp
                        a=`cat ./${ARQINI}_prod.manifest | grep $atemp | wc -l`
                        if [ $a -eq 0 ]; then
                              echo "+++ FALTANDO: $atemp"
                        fi
                  done
            done
            
            rm -f ./*_prod.manifest*
            exit 8
      fi
}

#COPIA
function copia {

typeset -ZR3 MEMB
set -A m AVN 01N 02N 03N 04N 05N 06N 07N 01P 02P 03P 04P 05P 06P 07P
MEMB=000

scp ldm@${tiggeldm}:$tiggedirmopora/sent/$DIRINI/${ARQINI}_prod.manifest ./

awq=0
while [ $MEMB -le 014 ]; do

      if [ $MEMB -eq 000 ]; then
            comp=3350
      else
            comp=3228
      fi
      echo "${m[$MEMB]} - `cat ${ARQINI}_prod.manifest | grep _${MEMB}_ | grep prod | wc -l` -ne $comp"
      if [ `cat ${ARQINI}_prod.manifest | grep _${MEMB}_ | grep prod | wc -l` -ne $comp ]; then
            echo "+++ MEMBRO ${m[$MEMB]} COM ERROS, RODANDO NOVAMENTE TIGGE MEMBRO ${m[$MEMB]}"
            
            smslabel Rodada "${LABELI} `date`"
            smslabel STATUS "ARQUIVO COM PROBLEMAS NO MEMBRO ${m[$MEMB]}..."
            smslabel Info "TENTANDO RODAR NOVAMENTE..."
            
            cp -rpfv ${BANGU}/GPOS/${YYYY}/${MM}/${DD}/GPOS${m[$MEMB]}${LABELI}* ${ROPERM}/pos/dataout/T${TRC}L${LV}/
            cd ${OPERM}/run
            ./runrectigge.1.4.sx6 ${LABELI} ${m[$MEMB]}
            ./vescptigge.ksh ${LABELI} $MEMB
            rm -f ${ROPERM}/pos/dataout/T${TRC}L${LV}/GPOS${m[$MEMB]}${LABELI}*
            
            echo " POSSIVEL PROBLEMA NO ENVIO, VERIFICAR SE ECMWF RECEBERA CORRETAMENTE..."
            smslabel Info "POSSIVEL PROBLEMA NO ENVIO, VERIFICAR SE ECMWF RECEBERA CORRETAMENTE..."

      else
            smslabel Info "ARQUIVOS ENVIADOS COM SUCESSO!"
            echo "    OK"
            let awq=awq+1
      fi
let MEMB=${MEMB}+1
done
rm -f ${ARQINI}_prod.manifest
echo "REENVIANDO..."

if [ awq -ge 14 ]; then
      echo "POSSIVEL PROBLEMA CORRIGIDO"
      smslabel STATUS "PROBLEMAS NO ENVIO, AVISAR ECMWF SOBRE POSSIVEIS PROBLEMAS" "VERIFICAR PAGINA: http://tigge.ecmwf.int/tigge/d/tigge_history/"
      exit 1
else
      echo "POSSIVEL PROBLEMA CORRIGIDO"
      smslabel STATUS "PROBLEMAS NO ENVIO, AVISAR ECMWF SOBRE POSSIVEIS PROBLEMAS" "VERIFICAR PAGINA: http://tigge.ecmwf.int/tigge/d/tigge_history/"
      exit 1
fi
# MOVE O REENVIO
ssh ldm@${tiggeldm} "mv $tiggescratch $tiggeoutgoing"        

}

function venv {

df=`ssh -l ldm ${tiggeldm} "ls -ltr $tiggedirmopora/outgoing/${DIRINI} | grep 'b:' | wc -l"`
echo $df

}
#FLAGS #0 desligado 1 ligado
resent=0
sending=0
sent=0
f=1 #flag do resent


# VERIFICA SE JAH HOUVE REENVIO ANTERIOR
df=`ssh -l ldm ${tiggeldm} "ls -ltr $tiggedirmopora/sent/ | grep ${LABELI}00_ | grep -v resend | wc -l"`
if [ $df -ge 4 ]; then
      echo "LIMITE DE REENVIOS ATINGIDO... SAINDO"
      exit 0
else
      echo "LIMITE DE REENVIOS OK ($df) - MAXIMO 4"
fi

# VERIFICA SE O ARQUIVO ESTAH SENDO ENVIADO
if [ \( `ssh -l ldm ${tiggeldm} "ls -ltr $tiggeoutgoing | grep $DIRINI | wc -l"` -ge 1 \) ]; then
      echo " ENVIANDO... ESPERAR"
      adsf=`ssh ${tiggeldm} -l ldm "ls -ltr $tiggeoutgoing/${DIRINI} | grep 'b\:' | wc -l"`
      echo "FAZENDO O ENVIO de $DIRINI ... ESPERAR\n$adsf ARQUIVOS INSERIDOS DE 48542"
      smslabel Rodada "${LABELI} `date`"
      smslabel STATUS "Aguardar Mudança no STATUS"
      smslabel Info "FAZENDO O ENVIO de $DIRINI ... ESPERAR" "       $adsf ARQUIVOS INSERIDOS DE 48542"
      sending=1
      exit 0
fi

# QUANDO OCORRE UM RESENT AUTOMATICO, NAO EXISTE MANIFEST, E ISSO PODE PROVOCAR FALHAS NO SCRIPT.
# NESTE CASO VERIFICAR A PAGINA COM MAIS ATENCAO
if [ \( `ssh -l ldm ${tiggeldm} "ls -ltr $tiggedirmopora/sent/ | grep ${LABELI}00_ | grep resend | wc -l"` -gt 0 \) ]; then
      echo "resend realizado"
      resent=1
fi

if [ `ssh -l ldm ${tiggeldm} "ls -ltr $tiggedirmopora/sent/${DIRINI} | wc -l"` -gt 0 ]; then
      echo ".manifest encontrado"
      sent=1
fi

#TESTANDO ARQUIVOS MANIFEST
testa
exit 0
################################################################################
# AREA DE COPIA
if [ "$COPY" = "s" ]; then
      copia
fi
################################################################################


exit 0


exit 0
