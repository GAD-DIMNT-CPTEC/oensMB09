#!/bin/ksh
CAL_DAY ()
{
yi=`date +"%Y"`
mi=`date +"%m"`
di=`date +"%d"`
#
let ybi=$yi%4
if [ $ybi = 0 ]
then
set -A md 31 29 31 30 31 30 31 31 30 31 30 31
else
set -A md 31 28 31 30 31 30 31 31 30 31 30 31
fi
#
#  nrdays = number of previous days to remove files
#
let dr=$di-$nrdays
let mr=$mi
let yr=$yi
#AMM let hr=$hi
if [ dr -le 0 ]
then
let nr=$mi-2
if [ nr -lt 0 ]
then let nr=11
fi
let dr=dr+md[$nr]
let mr=$mi-1
fi
if [ mr -le 0 ]
then
let mr=12
let yr=yr-1
if [ yr -lt 0 ]
then yr=99
fi
fi
if [ dr -lt 10 ]
then DR=0$dr
else DR=$dr
fi
if [ mr -lt 10 ]
then MR=0$mr
else MR=$mr
fi
if [ yr -lt 10 ]
then YR=0$yr
else YR=$yr
fi
}
nrdays=2
CAL_DAY
LABELR1=$YR$MR$DR
echo "LABELR1= "$LABELR1
nrdays=3
CAL_DAY
LABELR2=$YR$MR$DR
echo "LABELR2= "$LABELR2
nrdays=4
CAL_DAY
LABELR3=$YR$MR$DR
echo "LABELR3= "$LABELR3
#
RESOL=T126L28
HOME1=/gfs/home0/ensglob
OPERMOD=/gfs/home0/ensglob/oens
ROPERMOD=/gfs/dk02/ensglob/oens
#
rm -f ${HOME1}/COPY_BANGU.${LABELR1}.*.RUN.OK
rm -f ${HOME1}/COPY_BANGU.${LABELR2}.*.RUN.OK
rm -f ${HOME1}/COPY_BANGU.${LABELR3}.*.RUN.OK
rm -f ${OPERMOD}/*/output/*$LABELR1*
rm -f ${OPERMOD}/*/output/*$LABELR2*
rm -f ${OPERMOD}/*/output/*$LABELR3*
rm -f ${OPERMOD}/run/setout/set*$LABELR1*
rm -f ${OPERMOD}/run/setout/set*$LABELR2*
rm -f ${OPERMOD}/run/setout/set*$LABELR3*
rm -f ${OPERMOD}/run/set*
rm -f ${OPERMOD}/pre/dataout/T*/snwgrd*$LABELR1*
rm -f ${OPERMOD}/pre/dataout/T*/snwgrd*$LABELR2*
rm -f ${OPERMOD}/pre/dataout/T*/snwgrd*$LABELR3*
rm -f ${OPERMOD}/pre/dataout/T*/sstgwk*$LABELR1*
rm -f ${OPERMOD}/pre/dataout/T*/sstgwk*$LABELR2*
rm -f ${OPERMOD}/pre/dataout/T*/sstgwk*$LABELR3*
#
rm -f ${ROPERMOD}/*/output/*$LABELR1*
rm -f ${ROPERMOD}/*/output/*$LABELR2*
rm -f ${ROPERMOD}/*/output/*$LABELR3*
#
rm -f ${ROPERMOD}/model/datain/GANLNMC*$LABELR1*
rm -f ${ROPERMOD}/model/datain/GANLNMC*$LABELR2*
rm -f ${ROPERMOD}/model/datain/GANLNMC*$LABELR3*
rm -f ${ROPERMOD}/model/datain/snowfd*$LABELR1*
rm -f ${ROPERMOD}/model/datain/snowfd*$LABELR2*
rm -f ${ROPERMOD}/model/datain/snowfd*$LABELR3*
rm -f ${ROPERMOD}/model/datain/sstwkl*$LABELR1*
rm -f ${ROPERMOD}/model/datain/sstwkl*$LABELR2*
rm -f ${ROPERMOD}/model/datain/sstwkl*$LABELR3*
rm -f ${ROPERMOD}/model/datain/sstwkl.*$LABELR1*
rm -f ${ROPERMOD}/model/datain/sstwkl.*$LABELR2*
rm -f ${ROPERMOD}/model/datain/sstwkl.*$LABELR3*
#
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH01P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH01N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH02P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH02N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH03P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH03N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH04P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH04N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH05P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH05N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH06P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH06N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH07P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH07N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGHNMC$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH01P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH01N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH02P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH02N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH03P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH03N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH04P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH04N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH05P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH05N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH06P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH06N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH07P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH07N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGHNMC$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH01P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH01N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH02P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH02N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH03P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH03N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH04P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH04N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH05P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH05N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH06P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH06N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH07P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGH07N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFGHNMC$LABELR3*
#
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT01P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT01N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT02P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT02N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT03P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT03N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT04P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT04N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT05P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT05N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT06P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT06N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT07P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT07N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCTNMC$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT01P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT01N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT02P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT02N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT03P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT03N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT04P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT04N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT05P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT05N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT06P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT06N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT07P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT07N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCTNMC$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT01P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT01N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT02P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT02N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT03P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT03N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT04P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT04N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT05P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT05N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT06P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT06N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT07P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCT07N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GFCTNMC$LABELR3*
#
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG01P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG01N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG02P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG02N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG03P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG03N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG04P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG04N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG05P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG05N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG06P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG06N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG07P$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG07N$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRGNMC$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG01P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG01N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG02P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG02N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG03P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG03N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG04P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG04N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG05P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG05N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG06P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG06N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG07P$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG07N$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRGNMC$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG01P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG01N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG02P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG02N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG03P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG03N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG04P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG04N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG05P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG05N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG06P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG06N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG07P$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRG07N$LABELR3*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/GPRGNMC$LABELR3*
#
rm -f ${ROPERMOD}/model/dataout/${RESOL}/SizeTupanI*$LABELR1*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/SizeTupanI*$LABELR2*
rm -f ${ROPERMOD}/model/dataout/${RESOL}/SizeTupanI*$LABELR3*
#
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS01P$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS01N$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS02P$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS02N$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS03P$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS03N$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS04P$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS04N$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS05P$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS05N$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS06P$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS06N$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS07P$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS07N$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOSNMC$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS01P$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS01N$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS02P$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS02N$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS03P$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS03N$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS04P$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS04N$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS05P$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS05N$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS06P$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS06N$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS07P$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS07N$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOSNMC$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS01P$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS01N$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS02P$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS02N$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS03P$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS03N$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS04P$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS04N$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS05P$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS05N$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS06P$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS06N$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS07P$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOS07N$LABELR3*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/GPOSNMC$LABELR3*
#
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/SizeTupanII*$LABELR1*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/SizeTupanII*$LABELR2*
rm -f ${ROPERMOD}/pos/dataout/${RESOL}/SizeTupanII*$LABELR3*
#
rm -f ${ROPERMOD}/ensmed/dataout/${RESOL}/GPOENMC$LABELR1*
rm -f ${ROPERMOD}/ensmed/dataout/${RESOL}/GPOENMC$LABELR2*
rm -f ${ROPERMOD}/ensmed/dataout/${RESOL}/GPOENMC$LABELR3*
rm -f ${ROPERMOD}/ensmed/dataout/${RESOL}/SizeTupanII*$LABELR1*
rm -f ${ROPERMOD}/ensmed/dataout/${RESOL}/SizeTupanII*$LABELR2*
rm -f ${ROPERMOD}/ensmed/dataout/${RESOL}/SizeTupanII*$LABELR3*
#
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/CBNV$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/CONT$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/NEVE$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PNEV$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PREC$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PSLC$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PSNM$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TADL$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TEMS$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TOPO$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/UMRS$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/UVES$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/VVES$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/VSUT$LABELR1*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/LOCMM$LABELR1*
#
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/CBNV$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/CONT$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/NEVE$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PNEV$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PREC$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PSLC$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PSNM$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TADL$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TEMS$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TOPO$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/UMRS$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/UVES$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/VVES$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/VSUT$LABELR2*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/LOCMM$LABELR2*
#
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/CBNV$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/CONT$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/NEVE$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PNEV$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PREC$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PSLC$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/PSNM$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TADL$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TEMS$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/TOPO$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/UMRS$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/UVES$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/VVES$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/VSUT$LABELR3*
rm -f ${ROPERMOD}/plumes/dataout/${RESOL}/LOCMM$LABELR3*
#
