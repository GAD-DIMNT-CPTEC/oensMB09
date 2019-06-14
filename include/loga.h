if [ `echo "%SMSNAME%" | grep Pega_Analise | wc -l` -ge 1 ]; then
      rm -f /local_disk/dk00/logs_sms/%SUITE%.${LABELI}.log
      touch /local_disk/dk00/logs_sms/%SUITE%.${LABELI}.log
fi
di=`date +"%%Y%%m%%d%%H%%M%%S"`
echo "I:%SMSNAME%:${LABELI}:${di}" >> /local_disk/dk00/logs_sms/%SUITE%.${LABELI}.log
