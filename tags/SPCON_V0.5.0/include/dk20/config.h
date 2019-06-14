TRC=126
LV=28
FSCT=15
NPERT=07
PREFX=NMC
MAQ=sx6
CALDATE=/gfs/home3/modoper/bin/caldate.3.0.1

. %SMSINCLUDE%/config.sx6

if [ \( `date +"%%k%%M"` -ge 0250 \) -a \( `date +"%%k%%M"` -lt 1530 \) ]; then
      HH=00
fi
if [ \( `date +"%%k%%M"` -ge 1500 \) -o \( `date +"%%k%%M"` -lt 0249 \) ]; then
      HH=12
fi

set -x
SMSDATE=%SMSDATE%
#if [ `ls -ltr %SMSINCLUDE%/datasrod.txt | wc -l` -ge 1 ]; then
#      SMSDATE=`cat %SMSINCLUDE%/datasrod.txt | head -1`
#      if [ "%TASK%" = "Pega_Analise" ]; then
#            nlindatarod=`cat %SMSINCLUDE%/datasrod.txt | wc -l`
#            nlindatarod2=`echo "${nlindatarod}-1" | bc -l`
#      
#            cat %SMSINCLUDE%/datasrod.txt | tail -${nlindatarod2} > %SMSINCLUDE%/datasrod.txt.temp
#            mv %SMSINCLUDE%/datasrod.txt.temp %SMSINCLUDE%/datasrod.txt
#            
#            if [ $nlindatarod -eq 0 ]; then
#                  rm -f %SMSINCLUDE%/datasrod.txt
#            fi
#      fi
#fi
echo $SMSDATE

if [ ${#SMSDATE} -eq 8 ]; then
      LABELI=%SMSDATE%${HH}
      if [ ! "`echo %TASK%`" = "Pega_Analise" ]; then
            ls=`ls -ltr %SMSHOME%/oens/T${TRC}/Pre/Pega_Analise.[0-9]* | tail -1 | awk '{print $9}'`
            LABELI=`cat $ls | grep "LABELI=" |head -1 | sed -e s%%"+ LABELI="%%""%%g`
            HH=`echo $LABELI | cut -c 9-10`
      	sleep 15
      fi
else
      LABELI=%SMSDATE%
      HH=`echo ${LABELI} | cut -c 9-10`
fi

echo %FAMILY% |cut -c 6-8; echo ${LABELI}
echo LABELI=$LABELI

if [ \( "%FAMILY1%" = "Verifica" \) -o \( "%FAMILY1%" = "Controle" \) -o \( "%FAMILY1%" = "Pre" \) -o \( "%TASK%" = "Rm_Tigge" \) -o \( "%FAMILY1%" = "Ens_Medio" \) -o \( "%FAMILY1%" = "Plumas" \) -o \( "%FAMILY1%" = "Pos" \) -o \( "%FAMILY1%" = "Produtos" \) ]
then
      PREFX=AVN
else
      PREFX=%FAMILY1%%TASK%
      sleep=0
      let sleep=10*%FAMILY1%
      set -x
      sleep $sleep
      set +x
fi

set -x
set -e
set -u

maqtigge=mopora

# PREPARA SCRIPTS PARA A RODADA (GERA LINKS)
      echo "removendo e criando LINKs..."
typeset -RZ2 i=01
f=0
set +e; set +x
while [ $i -le 07 ]; do
if [ $i -eq 01 ]; then
      test -L %SMSHOME%/oens/T${TRC}/Modelo/Membros/$i/P.sms
      if [ $? -ne 0 ]; then
            f=1
      fi
      test -L %SMSHOME%/oens/T${TRC}/Tigge/$i/P.sms
      if [ $? -ne 0 ]; then
            f=1
      fi
else
      test -L %SMSHOME%/oens/T${TRC}/Modelo/Membros/$i/N.sms
      if [ $? -ne 0 ]; then
            f=1
      fi
      test -L %SMSHOME%/oens/T${TRC}/Modelo/Membros/$i/P.sms
      if [ $? -ne 0 ]; then
            f=1
      fi
      test -L %SMSHOME%/oens/T${TRC}/Modelo/Membros/$i/Analise.sms
      if [ $? -ne 0 ]; then
            f=1
      fi
      test -L %SMSHOME%/oens/T${TRC}/Tigge/$i/N.sms
      if [ $? -ne 0 ]; then
            f=1
      fi
      test -L %SMSHOME%/oens/T${TRC}/Tigge/$i/P.sms
      if [ $? -ne 0 ]; then
            f=1
      fi
fi

let i=$i+1
done

echo "f=$f"
set -e; set -x

if [ $f -ne 0 ]; then

      echo "CRIANDO LINKS PARA .sms"
      rm -f %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/P.sms
      rm -f %SMSHOME%/oens/T${TRC}/Modelo/Membros/02/*
      rm -f %SMSHOME%/oens/T${TRC}/Modelo/Membros/03/*
      rm -f %SMSHOME%/oens/T${TRC}/Modelo/Membros/04/*
      rm -f %SMSHOME%/oens/T${TRC}/Modelo/Membros/05/*
      rm -f %SMSHOME%/oens/T${TRC}/Modelo/Membros/06/*
      rm -f %SMSHOME%/oens/T${TRC}/Modelo/Membros/07/*

      rm -f %SMSHOME%/oens/T${TRC}/Tigge/01/P.sms
      rm -f %SMSHOME%/oens/T${TRC}/Tigge/02/*
      rm -f %SMSHOME%/oens/T${TRC}/Tigge/03/*
      rm -f %SMSHOME%/oens/T${TRC}/Tigge/04/*
      rm -f %SMSHOME%/oens/T${TRC}/Tigge/05/*
      rm -f %SMSHOME%/oens/T${TRC}/Tigge/06/*
      rm -f %SMSHOME%/oens/T${TRC}/Tigge/07/*

      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/02/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/03/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/04/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/05/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/06/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/07/N.sms

      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/02/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/03/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/04/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/05/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/06/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/N.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/07/P.sms

      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/Analise.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/02/Analise.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/Analise.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/03/Analise.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/Analise.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/04/Analise.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/Analise.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/05/Analise.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/Analise.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/06/Analise.sms
      ln -s %SMSHOME%/oens/T${TRC}/Modelo/Membros/01/Analise.sms %SMSHOME%/oens/T${TRC}/Modelo/Membros/07/Analise.sms

      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/02/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/03/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/04/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/05/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/06/N.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/07/N.sms

      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/01/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/02/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/03/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/04/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/05/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/06/P.sms
      ln -s %SMSHOME%/oens/T${TRC}/Tigge/01/N.sms %SMSHOME%/oens/T${TRC}/Tigge/07/P.sms
else
      echo "LINKS JAH CRIADOS"
fi

set +e
set +u
set +x
%include <loga.h>
set -e
set -u
set -x
