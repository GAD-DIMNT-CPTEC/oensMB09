#! /bin/ksh

YYYY=$1
MM=$2
DD=$3
HH=$4

export F_UFMTENDIAN=10,11,12
./dayinty_gau.x $YYYY $MM $DD $HH

exit 0
