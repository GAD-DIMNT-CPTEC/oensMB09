#!/bin/bash

HH=00
DOMAIN="TR"
####DATAOUTDIR=/stornext/grupos/ensemble_g/EPS/CRPS1.0/dataout/TQ0126L028/semvies/${HH}
DATAOUTDIR=/stornext/online1/ensemble_g/TIGGE/CRPS1.0/dataout/TQ0126L028/semvies/${HH}/${DOMAIN}
DATAOUTDIR=/stornext/home/chris.castro/EPS/CRPS1.0/dataout

FIGSDIR=~/EPS/CRPS1.0/figs; mkdir -p ${FIGSDIR}
CURRENTDIR=`pwd`

fctlag=24h
firstdate=20081216${HH}
 lastdate=20090301${HH}
targetdate=${firstdate}
hhtgt=`echo $targetdate | cut -c 9-10`

rm -f ${DATAOUTDIR}/crpsfilesin.aave.${hhtgt}.txt;touch ${DATAOUTDIR}/crpsfilesin.aave.${hhtgt}.txt;
while [ "${targetdate}" -ne "${lastdate}" ]
do
set -x
ls -1 ${DATAOUTDIR}/CRPS4CPTECEPS.ForecastFor${targetdate}.aave.ctl | awk '{print "open "$1}' >> ${DATAOUTDIR}/crpsfilesin.aave.${hhtgt}.txt
set +x
targetdate=`${HOME}/Tools/caldate.3.0.2 ${targetdate} + ${fctlag} "yyyymmddhh"`
done

rm -f ${DATAOUTDIR}/Figs.${hhtgt}.gs
cat <<EOGS > ${DATAOUTDIR}/Figs.${hhtgt}.gs
rc=gsfallow("on")
'reinit'
'exec crpsfilesin.aave.${hhtgt}.txt'
rc=countctls()
rc=getvar(1)
rc=xsure()
'q dims';say result;
'set stat on'
'set t 2 16'
_currctl=1
nrec=0
'soma=const('_var',0)'
'soma2=const('_var',0)'
*------------------------------------------------------------
while(_currctl<=_lastctl)
   'set dfile '_currctl
   'q file '_currctl;say result;
   say 'soma=soma+('_var'.'_currctl')'
   'soma=soma+('_var'.'_currctl')'
   'soma2=soma2+(crpsc.'_currctl')'
   'set vrange 0 50'
   'set cmark 0'
   'd soma'
   'q dims';say result;
   _currctl=_currctl+1
endwhile
*------------------------------------------------------------
*say "Number of records written:"nrec
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'set x 1';'set y 1';
'crpseps=soma/'_lastctl
'crpscli=soma2/'_lastctl

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
   'set vrange 0.0 3.0'
   'set xlabs 1|2|3|4|5|6|7|8|9|10|11|12|13|14|15'
   'd crpsf.'_currctl
   'set cmark 0'
   'set digsize 0.1'
   'd crpsc.'_currctl
   _currctl=_currctl+1
endwhile
*------------------------------------------------------------
'set cmark 2'
'set digsize 0.17'
'set cthick 7'
'd crpseps'
'gxyat ${FIGSDIR}/Fig.CRPSF.${hhtgt}.${DOMAIN}.TQ0126L028.comvies.png'
'c'

'set ylopts 1 5 0.20'
'set xlopts 1 5 0.20'
'set vrange -0.25 0.8'
'define zero=const('_var',0)'
'set cstyle 1'
'set cmark 0'
'set cthick 10'
'set ccolor 1'
'set xlabs 1|2|3|4|5|6|7|8|9|10|11|12|13|14|15'
'set grads off'
'd zero'

'set cmark 2'
'set cstyle 3'
'set ccolor 1'
'set digsize 0.17'
'set cthick 7'
'set xlabs 1|2|3|4|5|6|7|8|9|10|11|12|13|14|15'
'set grads off'
'set vrange -0.25 0.8'
'd 1-(crpseps/crpscli)'

'set cmark 3'
'set cstyle 0'
'set digsize 0.08'
'set ccolor 1'
'set grads off'
'set vrange -0.25 0.8'
'd 1-(crpseps/crpscli)'

'gxyat ${FIGSDIR}/Fig.CRPSS.${hhtgt}.${DOMAIN}.TQ0126L028.comvies.png'

EOGS
cd ${DATAOUTDIR}
grads -lc "run Figs.${hhtgt}.gs"


