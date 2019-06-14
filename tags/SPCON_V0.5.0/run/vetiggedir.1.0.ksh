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

# VERIFICA SE JAH HOUVE REENVIO ANTERIOR
df=`ssh -l ldm mopora "ls -ltr $tiggedirmopora/sent/ | grep $LABELI | wc -l"`
if [ $df -ge 3 ]; then
      echo "REENVIO JAH REALIZADO"
else
      echo "LIMITE DE REENVIOS OK - $df"
fi

# VERIFICA SE O ARQUIVO ESTAH SENDO ENVIADO
if [ \( `ssh -l ldm mopora "ls -ltr $tiggeoutgoing | grep $DIRINI | wc -l"` -ge 1 \) ]; then
      echo " ENVIANDO... ESPERAR"
      adsf=`ssh mopora -l ldm "ls -ltr $tiggeoutgoing/${DIRINI} | grep 'b\:' | wc -l"`
      smslabel Rodada "${LABELI} `date`"
      smslabel STATUS "Aguardar Mudança no STATUS"
      smslabel Info "FAZENDO O ENVIO de $DIRINI ... ESPERAR" "       $adsf ARQUIVOS INSERIDOS DE 48542"
      
      exit 0
fi

# QUANDO OCORRE UM RESENT AUTOMATICO, NAO EXISTE MANIFEST, E ISSO PODE PROVOCAR FALHAS NO SCRIPT.
# NESTE CASO VERIFICAR A PAGINA COM MAIS ATENCAO
if [ \( `ssh -l ldm mopora "ls -ltr $tiggedirmopora/sent/ | grep ${LABELI}00_ | grep resend | wc -l"` -gt 0 \) ]; then
      echo " IMPOSSIVEL LER OUTPUT (POSSIVEL RESEND OCORRENDO), MONITORAR PAGINA: http://tigge.ecmwf.int/tigge/d/tigge_history/"
      smslabel STATUS "MONITORAR PAGINA: http://tigge.ecmwf.int/tigge/d/tigge_history/" " RESEND EM ANDAMENTO, REINICIAR TASK EM ALGUMAS HORAS"
      smslabel Info "IMPOSSIVEL LER OUTPUT (POSSIVEL RESEND OCORRENDO)"
      smslabel Rodada "Verificar `date`"
      exit 2
fi

# VERIFICA A POSSIBILIDADE DE TER OCORRIDO NOVA INSERCAO DOS DADOS VIA NOVA RODADA OU COPIA.
#  GERALMENTE NO DIRETORIO SENT, QUANDO A RODADA FOR INSERIDA MAIS DE UMA VEZ,
# TEREMOS UM DIRETORIO ${DIRINI}.ALGUMANUMERACAO
# POR ISSO VERIFICAMOS O ULTIMO DIRETORIO GERADO, PARA NAO REENVIARMOS INFINITAMENTE O MESMO DADO
tiggesentdi=`ssh -l ldm mopora "ls -ltr $tiggedirmopora/sent/ | grep ${LABELI} | tail -1"`
tiggesentdir=`echo $tiggesentdi | awk '{print $8}'`

f=1 # CASO f=1 O SCRIPT REENVIARA OS DADOS MESMO DEPOIS DE TER ENVIADO MAIS DE UMA VEZ
if [ ! \( $tiggesentdir = $DIRINI \) ]; then
      echo "ARQUIVO RODADO...."
      echo "VERIFICANDO REENVIO"
      smslabel Rodada "${LABELI} `date`"
      smslabel STATUS "ARQUIVO RODADO...."
      smslabel Info "VERIFICANDO REENVIO"
      
      typeset -ZR3 MEMB
      set -A m AVN 01N 02N 03N 04N 05N 06N 07N 01P 02P 03P 04P 05P 06P 07P
      MEMB=000
      # VERIFICACAO NO DIRETORIO QUE FOI REENVIADO OS MEMBROS QUE ESTAVAM COM PROBLEMAS.
      
#      scp ldm@mopora:$tiggedirmopora/sent/$tiggesentdir/${ARQINI}_prod.manifest ./
#      scp ldm@mopora:$tiggedirmopora/sent/${DIRINI}/${ARQINI}_prod.manifest ./${ARQINI}_prod.manifest.old

            # dirmo = todos os diretorios de reenvio + envio
            # ni = numero de diretorios de reenvio ( nao contando resend)
            dirmo=`ssh -l ldm mopora "ls $tiggedirmopora/sent/ | grep $LABELI"`
            ni=`ssh -l ldm mopora "ls $tiggedirmopora/sent/ | grep $LABELI | wc -l"`
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
                        scp ldm@mopora:$tiggedirmopora/sent/$dir/${ARQINI}_prod.manifest ./${ARQINI}_prod.manifest.$nia
                  fi
                  atemp=`cat ./${ARQINI}_prod.manifest.${nia} | grep _${MEMB}_ | grep prod | wc -l`
                  a=`echo $a + $atemp | bc -l`
            let nia=$nia+1
            done            
            
            if [ \( $a -ge $comp \) ]; then
                  echo "+++ MEMBRO ${m[$MEMB]} OK - $a"  
                      
                  smslabel Rodada "${LABELI} `date`"
                  smslabel STATUS "ARQUIVO RODADO..."
                  smslabel Info "+++ MEMBRO ${m[$MEMB]} OK"
#                  smslabel ${m[$MEMB]} "+++ OK"
            else
                  echo "REENVIO COM PROBLEMAS ${m[$MEMB]}"
                  smslabel Rodada "${LABELI} `date`"
                  smslabel STATUS "REENVIO COM PROBLEMAS, POSSIVEL PROBLEMA NO POS"
                  smslabel Info "REFAZER APENAS MEMBRO E TIGGE DO MEMBRO ${m[$MEMB]}..." "RODAR COMANDO ssh ldm@mopora \"mv $tiggescratch $tiggeoutgoing\" APOS COMPLETAR AS TAREFAS ACIMA"
#                  smslabel ${m[$MEMB]} "+++ ERR"
                  f=0
            fi
      let MEMB=${MEMB}+1
      done
      rm -f ${ARQINI}_prod.manifest
else
      echo "PRIMEIRO SENT"
                  smslabel Rodada "${LABELI} `date`"
                  smslabel STATUS "ARQUIVO RODADO..."
                  smslabel Info "VERIFICANDO PRIMEIRO SENT..."
      f=0
fi

echo "f=$f"

# VERIFICA MEMBRO A MEMBRO NO MANIFEST DO SENT (PRIMEIRO SENT - $DIRINI), CASO ALGUM ARQUIVO ESTEJA FALTANDO, TENTA RODAR NOVAMENTE O MEMBRO E 
# COPIA-LOS PARA A MOPORA.
rm -f *.manifest*

if [ $f -eq 0 ]; then
typeset -ZR3 MEMB
set -A m AVN 01N 02N 03N 04N 05N 06N 07N 01P 02P 03P 04P 05P 06P 07P
MEMB=000

scp ldm@mopora:$tiggedirmopora/sent/$DIRINI/${ARQINI}_prod.manifest ./

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
#            smslabel ${m[$MEMB]} "+++ ??"
            
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
      echo "TODOS ARQUIVOS CORRETOS"
      smslabel STATUS "TODOS OS MEMBROS CORRETOS"
else
      smslabel STATUS "POSSIVEL PROBLEMA CORRIGIDO" "VERIFICAR PAGINA: http://tigge.ecmwf.int/tigge/d/tigge_history/"
fi
# MOVE O REENVIO
ssh ldm@mopora "mv $tiggescratch $tiggeoutgoing"        

fi


exit 0
