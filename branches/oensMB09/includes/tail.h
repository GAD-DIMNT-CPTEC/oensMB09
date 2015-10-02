echo '----'
echo "FIM da task  %SMSNAME% =>" `date +%%s` "("`date`")"
echo '----'
date

touch ${path_log_sms}/${SMSDATEHH}_${arqn_log_sms} 
chmod 755 ${path_log_sms}
echo "E:$(date +'%%Y%%m%%d%%H%%M%%S'):%SMSNAME%" >> ${path_log_sms}/${arqn_log_sms}
cat ${path_log_sms}/${arqn_log_sms} ${path_log_sms}/${SMSDATEHH}_${arqn_log_sms} > ${path_log_sms}/${SMSDATEHH}_${arqn_log_sms}.tmp
mv ${path_log_sms}/${SMSDATEHH}_${arqn_log_sms}.tmp ${path_log_sms}/${SMSDATEHH}_${arqn_log_sms}
rm -f ${path_log_sms}/${arqn_log_sms}

smscomplete  # Notify SMS of a normal end
trap 0       # Remove all traps
exit 0       # End the shell
