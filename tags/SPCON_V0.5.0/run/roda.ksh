#! /bin/ksh -x

labeli=$1
labelf=$2
hh=$3

while [ $labeli -le $labelf ]
do
./fazlink.ksh $labeli 01N $hh
./fazlink.ksh $labeli 02N $hh
./fazlink.ksh $labeli 03N $hh
./fazlink.ksh $labeli 04N $hh
./fazlink.ksh $labeli 05N $hh
./fazlink.ksh $labeli 06N $hh
./fazlink.ksh $labeli 07N $hh
./fazlink.ksh $labeli 01P $hh
./fazlink.ksh $labeli 02P $hh
./fazlink.ksh $labeli 03P $hh
./fazlink.ksh $labeli 04P $hh
./fazlink.ksh $labeli 05P $hh
./fazlink.ksh $labeli 06P $hh
./fazlink.ksh $labeli 07P $hh
./fazlink.ksh $labeli NMC $hh
#./fazlink.ksh $labeli ENM $hh
labeli=`date -d "$labeli 1 day" +'%Y%m%d'`
done

exit 0
