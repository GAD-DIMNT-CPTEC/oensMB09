#! /bin/bash

date
echo '----'
echo "INICIO da task  %SMSNAME% =>" `date +%%s` "("`date`")"
echo '----'

set -e # stop the shell on first error
set -u # fail when using an undefined variable
set -x # echo script lines as they are executed

# Defines the three variables that are needed for any
# communication with SMS

export SMS_PROG=%SMS_PROG%  # SMS Remote Procedure Call number
export SMSNODE=%SMSNODE%    # The name sms that issued this task
export SMSNAME=%SMSNAME%    # The name of this current task
export SMSPASS=%SMSPASS%    # A unique password
export SMSTRYNO=%SMSTRYNO%  # Current try number of the task

export PATH=$PATH:/stornext/home/modoper/sms/bin

export path_log_sms=$(dirname /stornext/online7/pnt/preoper/tempo/sms_logs/%SMSNAME%); mkdir -p ${path_log_sms}
export arqn_log_sms=$(basename %SMSNAME%).log

# Tell SMS we have stated
# The SMS variable SMSRID will be set to parameter of smsinit
# Here we give the current PID.

smsinit $$ 

# Defined a error hanlder
ERROR() {
	date
        echo "             |        |"
        echo "             |        |"
	echo "------------------------------------"
	echo "       ----------------------"
	echo "              --------"
	echo "                 --"
	set +e                     # Clear -e flag, so we don't fail
   echo "A:$(date +'%%Y%%m%%d%%H%%M%%S'):%SMSNAME%" >> ${path_log_sms}/${arqn_log_sms}
	smsabort                   # Notify SMS that something went wong
	trap 0                     # Remove the trap
	exit 0                     # End the script
}

# Trap any calls to exit and errors caught by the -e flag

trap ERROR 0

# Trap any signal that may cause the script to fail

trap '{ echo "Killed by a signal"; ERROR ; }' 1 2 3 4 5 6 7 8 10 12 13 15

#-------------------------------------------------------#
# INICIO DA VERIFICACAO DE HORA E DATA
chmod 755 ${path_log_sms}

if [ %SMSTRYNO% -eq 1 ]; then
   echo "I:$(date +'%%Y%%m%%d%%H%%M%%S'):%SMSNAME%" >> ${path_log_sms}/${arqn_log_sms}
else
   echo "R:$(date +'%%Y%%m%%d%%H%%M%%S'):%SMSNAME%" >> ${path_log_sms}/${arqn_log_sms}
fi

