
if [ "%MOD%" = "o" ]; then
    FCT=360
    HINT=06
#   if [ "$(echo %FAMILY% | cut -d/ -f1)" = "main" ]; then
   if [ "1" = "1" ]; then   #DSM  -  Estou tentando rodar a previsao do dia 01/11 das 12Z sendo que 
                            #        jah foi rodada a do dia 02/11 das 00Z, neste caso o TIGGE e
                            #        os produtos estao ignorando o SMSDATE setado e executando sempre o
                            #        02/11 00Z - Alex, favor verificar como realmente deve ser este if
   	if [ $( echo "%SMSDATE%" | wc -c) -ge 11 ]; then
         SMSDATEHH=%SMSDATE%
         SMSDATE=$(echo $SMSDATEHH | cut -c 1-8)
         HH=$(echo $SMSDATEHH | cut -c 9-10)
   	else
    		if [ "%TASK%" == "runPre" ]; then
   		     echo "USANDO ULTIMA DATA DO DIRETORIO POS..."
   			  smslabel Info " USANDO A ULTIMA DATA DO DIRETORIO ${WORK_HOME}/oensMB09/pos/dataout/%RESOL%/"
   	        lfile=$(ls -l --full-time ${WORK_HOME}/oensMB09/pos/dataout/%RESOL%/ | tail -1 | awk '{print $9}')
   			if [ \( -d ${WORK_HOME}/oensMB09/pos/dataout/%RESOL%/$lfile \) -a \( 0$(du -sk ${WORK_HOME}/oensMB09/pos/dataout/%RESOL%/$lfile | awk '{print $1}') -ge 0400000 \) ]; then
   				lfile=$(basename $lfile)
   		      echo "ULTIMA DATA GERADA: $lfile"
   		      SMSDATE=$(echo $lfile | cut -c 1-8)
    	        	HH=$(echo $lfile | cut -c 9-10)
   	    	   SMSDATEHH=$(date -d "$SMSDATE ${HH}:00 ${HINT} hours" +"%%Y%%m%%d%%H")
   		      SMSDATE=$(echo $SMSDATEHH | cut -c 1-8)
   	        	HH=$(echo $SMSDATEHH | cut -c 9-10)
   			else
   				SMSDATEHH=$(basename $lfile)
		         SMSDATE=$(echo $SMSDATEHH | cut -c 1-8)
   	        	HH=$(echo $SMSDATEHH | cut -c 9-10)
	   		fi
		   else
			   lfile=$(ls -ltr --full-time %SMSHOME%/%SUITE%/main/%RESOL%/pre/runPre.[0-9]* | tail -1 | awk '{print $9}')
   			SMSDATEHH=$(grep -a "^SMSDATEHH=" $lfile | tail -1 | cut -d= -f2)
	         SMSDATE=$(echo $SMSDATEHH | cut -c 1-8)
           	HH=$(echo $SMSDATEHH | cut -c 9-10)
   			echo "DATA PRESENTE EM: $lfile -> $SMSDATEHH"
   		fi
       fi
   else
      sleep 30
      lfile=$(ls -l --full-time ${WORK_HOME}/oensMB09/pos/dataout/%RESOL%/ | tail -1 | awk '{print $9}')
   	lfile=$(basename $lfile)
      echo "ULTIMA DATA GERADA: $lfile"
   	SMSDATEHH=$lfile
   	SMSDATE=$(echo $lfile | cut -c 1-8)
    	HH=$(echo $lfile | cut -c 9-10)
   fi
	if [ $HH -eq 12 -o $HH -eq 00 ]; then
		LABELF=`date -d "${SMSDATE} $HH:00 $FCT hours" +"%%Y%%m%%d%%H"`
	else
		LABELF=`date -d "${SMSDATE} $HH:00 24 hours" +"%%Y%%m%%d%%H"`
	fi
else
	SMSDATEHH=%YMD%12
	SMSDATE=`echo $SMSDATEHH | cut -c1-8`
	HH=12
	LABELF=`date -d "${SMSDATE} $HH:00 180 hours" +"%%Y%%m%%d%%H"`
fi

echo SMSDATEHH=$SMSDATEHH
SMSDATE=$(echo $SMSDATEHH | cut -c 1-8)
HH=$(echo $SMSDATEHH | cut -c 9-10)
CASE=%RESOL%
cd %MODELHOME%
set -x
PATHA=`pwd`
export FILEENV=`find ${PATHA} -maxdepth 2 -name EnvironmentalVariablesOENS -print`
export PATHENV=`dirname ${FILEENV}`
. ${FILEENV} ${CASE} NMC

set -x
set +e
mkdir -p %MODELDK%/out_err/${CASE}/${SMSDATEHH}
set -e 


if [ $HH -eq 06 -o $HH -eq 18 ]; then
   if [ $(echo "%SMSNAME%" | egrep "NMC|runPre$|ctrl" | wc -l) -eq 0 ]; then
%include <tail.h>
   else
      NFDAYS=1
   fi
fi
   
