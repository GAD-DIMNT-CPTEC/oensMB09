#!/bin/ksh -x

. ../include/config.sx6

LABELI=$1 #YYYYmmddhh
LABELS=`echo $LABELI | cut -c 1-8`
HH=`echo $LABELI | cut -c 9-10`
LABELF=`date -d "$LABELS 15 days" +"%Y%m%d${HH}"`

PREFX=$2

dgrhdatain="${OPERM}/grh/datain"
nmlgrh="${dgrhdatain}/postgrh.${PREFX}.nml"


cat <<EOF> ${nmlgrh}
&InputDim
Mend=${TRC},            ! Model Spectral Horizontal Resolution
Kmax=${LV},             ! Number of Vertical Model Layers
mUTC=0,                 ! Diference in Hour to Greenwhich (if desired, if no set 0_i4)
DelT=600.0,             ! Model Time Step in Seconds
TMean=3600.0,           ! Time Interval in Seconds To Average Output (1 Hour)
LabelI='${LABELI}',     ! Initial Condition Date
LabelF='${LABELF}',     ! Final Forecast Date
Preffix='${PREFX}',     ! Preffix of File Names
GrdBox='    ',          ! Preffix Name To Skip Points (Use = 'Prox' to skip "Proxes" Points)
DirMain='${ROPERM}/'    ! Main Data Directory
/
EOF

ts=`date +"%s"`

cat <<EOF> ${OPERM}/run/setgrhpost.sx6
#!/bin/ksh -x
#PBS -l cpunum_prc=1
#PBS -l tasknum_prc=1
#PBS -l memsz_job=1gb
#PBS -l cputim_job=1000.0
#PBS -o turi-e:${OPERM}/run/setout/setgrhpost.${ts}.out
#PBS -j o

echo "cd ${OPERM}/grh/exec"
cd ${OPERM}/grh/exec

echo "./PostGridHistory < ${nmlgrh}"
./PostGridHistory < ${nmlgrh}

exit 0
EOF

chmod 700 ${OPERM}/run/setgrhpost.sx6
submit ${OPERM}/run/setgrhpost.sx6 ${QUEUE}

rm -f ${OPERM}/run/setgrhpost.sx6

if [ `ls -ltr ${ROPERM}/pos/dataout/T${TRC}L${LV}/GFGN${PREFX}${LABELI}${LABELF}M.grh.T${TRC}L${LV} | awk '{print $5}'` -lt 16813440 ];then
      echo "ERRO - GRH Binario nao criado"
      exit 1
fi

exit 0
