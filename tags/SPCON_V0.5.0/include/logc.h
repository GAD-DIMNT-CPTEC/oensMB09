df=`date +"%%Y%%m%%d%%H%%M%%S"`
echo "A:%SMSNAME%:${LABELI}:${df}" >> /local_disk/dk00/logs_sms/%SUITE%.${LABELI}.log
