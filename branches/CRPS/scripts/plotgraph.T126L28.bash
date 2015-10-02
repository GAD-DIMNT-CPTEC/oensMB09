#!/bin/bash

#OUTPUTDIR=/scratch2/grupos/ensemble_g/home/chris.castro/EPS/CRPS1.0/output
#DATAINDIR=/scratch1/grupos/ensemble_g/home/chris.castro/EPS/CRPS1.0/datain
#DATAOUTDIR=/stornext/grupos/ensemble_g/EPS/CRPS1.0/dataout/T126L28
#FIGSDIR=/stornext/home/chris.castro/EPS/CRPS1.0/figs
OUTPUTDIR=/scratchin/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/output
DATAINDIR=/scratchin/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/datain
DATAOUTDIR=/scratchin/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/dataout
FIGSDIR=/scratchin/grupos/assim_dados/home/carlos.bastarz/ensemble_g/oens_new/CRPS1.0/figs
CURRENTDIR=`pwd`

fctlag=24h
#firstdate=2008120112
firstdate=2015030300
###lastdate=2009030112
#lastdate=2008123112
lastdate=2015033112
targetdate=${firstdate}
rm -f ${DATAOUTDIR}/crpsfilesin.aave.txt;touch ${DATAOUTDIR}/crpsfilesin.aave.txt;


while [ "${targetdate}" -ne "${lastdate}" ]
do
set -x
ls -1 ${DATAOUTDIR}/CRPS4CPTECEPS.ForecastFor${targetdate}.aave.ctl | awk '{print "open "$1}' >> ${DATAOUTDIR}/crpsfilesin.aave.txt
set +x
targetdate=`${HOME}/scripts/caldate.3.1.2 ${targetdate} + ${fctlag} "yyyymmddhh"`
done


rm -f ${DATAOUTDIR}/Figs.gs
cat <<EOGS > ${DATAOUTDIR}/Figs.gs
rc=gsfallow("on")
'reinit'
'exec crpsfilesin.aave.txt'
rc=countctls()
rc=getvar(1)
rc=xsure()
'q dims';say result;
'set stat on'
'set t 2 16'
_currctl=1
nrec=0
'soma=const('_var',0)'
*------------------------------------------------------------
while(_currctl<=_lastctl)
   'set dfile '_currctl
   say 'soma=soma+('_var'.'_currctl')'
   'soma=soma+('_var'.'_currctl')'
   _currctl=_currctl+1
endwhile
*------------------------------------------------------------
*say "Number of records written:"nrec
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'set x 1';'set y 1';
'crpseps=soma/'_lastctl

'set rgb 20 150 150 150'
'set display color white';'c';
'set ylopts 1 5 0.20'
'set xlopts 1 5 0.20'
'set x 1'
'set y 1'
'set t 2 16'
_currctl=1
*------------------------------------------------------------
while(_currctl<=_lastctl)
   'set dfile '_currctl
   'set grads off'
   'set ccolor 20'
   'set cthick 5'
   'set cmark 0'
   'set vrange 0.5 4.5'
   'set xlabs 1|2|3|4|5|6|7|8|9|10|11|12|13|14|15'
   'd crpsf.'_currctl
   'set cmark 3'
   'set digsize 0.1'
   'd crpsc.'_currctl
   _currctl=_currctl+1
endwhile
*------------------------------------------------------------
'set cmark 2'
'set digsize 0.17'
'set cthick 7'
'd crpseps'
'gxyat ${FIGSDIR}/Fig.CRPSF.HN.TQ0126L028.png'
'c'

'set ylopts 1 5 0.20'
'set xlopts 1 5 0.20'
'set cmark 2'
'set digsize 0.17'
'set cthick 7'
'set xlabs 1|2|3|4|5|6|7|8|9|10|11|12|13|14|15'
'set grads off'
'd 1-(crpseps/crpsc.1)'
'set cmark 3'
'set digsize 0.08'
'set ccolor 1'
'd 1-(crpseps/crpsc.1)'
'gxyat ${FIGSDIR}/Fig.CRPSS.HN.TQ0126L028.png'

EOGS
cd ${DATAOUTDIR}
grads -lc "run Figs.gs"
######DATAOUTDIR=/scratch2/grupos/ensemble_g/home/chris.castro/EPS/CRPS1.0/dataout
