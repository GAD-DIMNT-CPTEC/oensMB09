#! /bin/ksh

YYYY=$1
MM=$2
DD=$3
HH=$4
u1=$5
u2=$6
u3=$7
u4=$8

export F_UFMTENDIAN=12
./dayinty.T126.x $YYYY $MM $DD $HH $u1 $u2 $u3 $u4

exit 0
