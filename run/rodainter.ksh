#! /bin /ksh
# RODA O MODELO ENSEMBLE PROTOTIPO EM UM INTERVALO DE DATAS
# RODAINTER.KSH YYYYMMDDHH yyyymmddhh
# YYYYMMDDHH - DATA INICIAL
# yyyymmddhh - DATA FINAL

LABELI=$1
LABELF=$2

if [ -z $LABELI ]
then
	echo "ERRO - LABELI not set!"
	echo "RODAINTER.KSH YYYYMMDDHH yyyymmddhh"
	exit 0
fi

if [ -z $LABELF ]
then
	echo "ERRO - LABELF not set!"
	echo "RODAINTER.KSH YYYYMMDDHH yyyymmddhh"
	exit 0
fi

cd /gfs/home3/io_dop/tempo/global/oenspro/run
./rodatudo.ksh $LABELI $LABELF

exit 0
