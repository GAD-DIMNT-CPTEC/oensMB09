#!/bin/ksh

LABELI=$1 #YYYYMMDDHH

log=`ssh -l ldm mopora "ls -ltr /usr/local/ldm/tigge/data/sent/sbsj_${LABELI}00_glob_prod | wc -l"`
if [ $log -ge 48545 ]; then
      echo "\nARQUIVOS ENVIADOS COM SUCESSO... VERIFICAR PAGINA PARA CONFIRMACAO DE RECEBIMENTO"
else
      echo "\nARQUIVOS POSSIVELMENTE COM PROBLEMAS DE ENVIO ($log), OLHANDO DIRETORIOS RESEND"
      log=`ssh -l ldm mopora "ls -ltr /usr/local/ldm/tigge/data/sent/sbsj_${LABELI}00_glob_prod* | wc -l"`
      if [ $log -ge 48545 ]; then
            echo "\nARQUIVOS ENVIADOS COM SUCESSO (RESEND=$log)... VERIFICAR PAGINA PARA CONFIRMACAO DE RECEBIMENTO"
      else
            echo "ERRO NO ENVIO, POSSIVELMENTE ECMWF NAO RECEBERA..."
            exit 2
      fi
fi      

exit 0
