#!/bin/csh -f

#set GRIBS = `ls -tr1 *.grb`
set GRIBS = `ls -1tr *.grb | cut -d"." -f1`

 foreach GRIB2 ($GRIBS)

 ../../bin/grib_convert cptec.rules_grib2 $GRIB2.grb $GRIB2.grib
 echo $GRIB2
 ../../bin/tigge_check $GRIB2.grib
# set TIGGENAME = `../../bin/tigge_name $GRIB2.grib`
# mv $GRIB2.grib $TIGGENAME 

 end

exit 0
