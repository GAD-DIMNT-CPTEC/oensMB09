#! /bin/ksh

YYYY=$1
MM=$2
DD=$3
HH=$4
u1=T${TRC}L${LV}
u2=GPOS${PREFX}
u3=${BANGU}/GPOS
u4=/gfs/home3/modoper/tempo/global/oenspro/produtos/SKILL/CLIMA/

export F_UFMTENDIAN=12
./dayinty.T126.x $YYYY $MM $DD $HH $u1 $u2 $u3 $u4

exit 0
