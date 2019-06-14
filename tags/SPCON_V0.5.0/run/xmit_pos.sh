#!/bin/ksh -x
#PBS -o turi-e:/gfs/home3/io_dop/users/alex/oenspro/run/setout/setposgrib2007092400.1190659596.out
#PBS -j o
#

#PARA RODAR NA TURI export F_UFMTENDIAN=18,19,10,11,22,23,24,49,83,82,81,27,25,26,33,99,61,45,42,52,50,80,51,53,92,93,43,77,0,31,91,66,32,21,20,74,88,89,36,37,38,39

# ORIGINAL
#AAF cd /gfs/home3/io_dop/users/alex/oenspro/run/exec
#AAF Post_grb < /gfs/home3/io_dop/users/alex/oenspro/run/POSTIN_T126L28a01N > /gfs/home3/io_dop/users/alex/oenspro/run/setout/xmit_pos01N.20070924_20070924.POS

ls -ltr /gfs/home3/io_dop/users/alex/oenspro/run/POSTIN_T126L28a01N
#NOVO POS - JPABLO
cd /gfs/home3/io_dop/users/alex/oenspro/tigge/bin
Postgrb < /gfs/home3/io_dop/users/alex/oenspro/run/POSTIN_T126L28a01N > /gfs/home3/io_dop/users/alex/oenspro/run/setout/xmit_pos01N.20070924_20070924.POS

echo "Post Ended..."
