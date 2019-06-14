cat << EOF > /dev/null

##############################################################################################
# QUEUE CONTROL
#

NFILE=${JNAME}
cerr=0

while [ 1 ]
do

CONTRL=`qstat |grep ${NFILE} | wc -l`

if [ ${CONTRL} -eq 0 ]
then
        ERRNO=`qstat | grep "errno" | wc -l`

        if [ ${ERRNO} -ge 01 ]
        then
                smslabel Info "Can't connect to BatchServer"
        else
		cerr=$(($cerr+1))
                if [ ${cerr} -ge 3 ]
                then
                        smslabel Info "Verifing Files"
                        break
                fi
	fi
sleep 11
else
        EXS=`qstat | grep ${NFILE} | awk '{print $6}'`          # Captura o status da fila
        ERRNO=`qstat | grep "errno" | wc -l`

        if [ ${ERRNO} -ge 01 ]
        then
                smslabel Info "Can't connect to BatchServer"
        else

	        case ${EXS} in

	        QUE)                                                   # Status de Fila
	                echo "QUEUED..."
	                smslabel Info "Queued ..."
			cerr=0
	        ;; 
	        RUN)                                                   # Status de Rodando
	                TIME=`qstat | grep ${NFILE} | awk '{print $9}'` # Captura a quanto tempo estah rodando
	                TIME=`echo ${TIME} | cut -d"." -f1`
	                echo "RUNNING..."
	                echo "TIME: "${TIME}
	                       smslabel Info "Runing: ${TIME} Secs ..."
			cerr=0
	        ;;
	        STG)                                                   # Status de Stage
	                echo "STAGED..."
	                smslabel Info "Staged ..."
			cerr=0
	        ;;
	        *)                                                     # Default para outros status
	                echo "Status Unknow..."
	                smslabel Info "Status Unknow: ${EXS}"
			cerr=0
	        ;;
	
	        esac
        fi
sleep ${SLEEPTIM}
fi

done

EOF
