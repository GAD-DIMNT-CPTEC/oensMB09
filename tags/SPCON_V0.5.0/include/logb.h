df=`date +"%%Y%%m%%d%%H%%M%%S"`
echo "F:%SMSNAME%:${LABELI}:${df}" >> /local_disk/dk00/logs_sms/%SUITE%.${LABELI}.log
