#! /bin/ksh

YYYY=$1
MM=$2
DD=$3
HH=$4
DIRC=$5

export F_UFMTENDIAN=10,11,12
./dayinty_gau.T126.x $YYYY $MM $DD $HH $DIRC

exit 0
