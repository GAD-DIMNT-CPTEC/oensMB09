say 'Enter with: resol trc labeli nblst dirbct dirfig'
pull ctime

resol =subwrd(ctime,1)
trc   =subwrd(ctime,2)
labeli=subwrd(ctime,3)
nblst =subwrd(ctime,4)
dirbct=subwrd(ctime,5)
dirfig=subwrd(ctime,6)

dirbct=dirbct%'/'
dirfig=dirfig%'/'

*
* Set colors for figures
*

'rgbset.gs'

*
* List of ctl files
*

flist=dirbct%'/'%filefct%labeli%'.'%trc
say 'flist='flist

*
* Looping to produce all figures
*

nf=1
while (nf <= nblst)

  'reinit'

*
* Open Ctl
*

   rec=read(flist)
   ctlfile=sublin(rec,2)
  
   say 'open 'ctlfile''
  'open 'ctlfile''

  'q files'
  say result 

*
* Obtain the date of the forecasting
*

  'set t 1'
  'q time'
   var1=subwrd(result,3)
   yy2=substr(var1,9,4)
   mes=substr(var1,6,3)
   dd2=substr(var1,4,2)
   hh2=substr(var1,1,2)
   if (mes = 'JAN');mm2='01';endif
   if (mes = 'FEB');mm2='02';endif
   if (mes = 'MAR');mm2='03';endif
   if (mes = 'APR');mm2='04';endif
   if (mes = 'MAY');mm2='05';endif
   if (mes = 'JUN');mm2='06';endif
   if (mes = 'JUL');mm2='07';endif
   if (mes = 'AUG');mm2='08';endif
   if (mes = 'SEP');mm2='09';endif
   if (mes = 'OCT');mm2='10';endif
   if (mes = 'NOV');mm2='11';endif
   if (mes = 'DEC');mm2='12';endif
   labelf=yy2''mm2''dd2''hh2

*
* Set Grads environment
*

  'set grid off'
  'set display color white'
  'c'
  'set gxout shaded'
  'set mpdset brmap_hires'

  'set lat -60 15'
  'set lon -101.25 -11.25'
  
*
* Plot the figures
*

  'set vpage 0.5 5.5 4.0 8.0'
  'set grads off'
  'set clevs 5 35 65 95'
  'set ccols 0 20 39 49 58'
  'd smth9(prob1*100)'
  'run cbarn.gs'
  'draw title Precipitacao acumulada em 24 hrs > 1.0 mm'
  
  'set vpage 5.5 10.5 4.0 8.0'
  'set grads off'
  'set clevs 5 35 65 95'
  'set ccols 0 20 39 49 58'
  'd smth9(prob5*100)'
  'run cbarn.gs'
  'draw title Precipitacao acumulada em 24 hrs > 5.0 mm'
  
  'set vpage 0.5 5.5 0.0 4.0'
  'set grads off'
  'set clevs 5 35 65 95'
  'set ccols 0 20 39 49 58'
  'd smth9(prob10*100)'
  'run cbarn.gs'
  'draw title Precipitacao acumulada em 24 hrs > 10.0 mm'
  
  'set vpage 5.5 10.5 0.0 4.0'
  'set grads off'
  'set clevs 5 35 65 95'
  'set ccols 0 20 39 49 58'
  'd smth9(prob20*100)'
  'run cbarn.gs'
  'draw title Precipitacao acumulada em 24 hrs > 20.0 mm'
  
  'set vpage off'
  'set string 1 c 6'
  'set strsiz 0.11 0.12'
  'draw string 5.5 8.3 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'resol''
  'draw string 5.5 8.1 Previsao de Probabilidades (%) -  A partir de: 'labeli'Z  Valido para: 'labelf'Z' 
*  'printim 'dirfig'prec'labeli''labelf'.png png x900 y700'
  'printim 'dirfig'prec'labeli''labelf'.png png x1200 y1000'
*  '!/usr/bin/convert -trim 'dirfig'prec'labeli''labelf'.png 'dirfig'prec'labeli''labelf'.png'
  '!/home/carlos.bastarz/bin/convert -trim 'dirfig'prec'labeli''labelf'.png 'dirfig'prec'labeli''labelf'.png'

   nf=nf+1
endwhile
   
'quit'

   
   
   
   
   
   
   
   
   
