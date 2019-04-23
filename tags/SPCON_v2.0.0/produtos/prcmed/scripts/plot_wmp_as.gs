say 'put datei nweek case dirfig dirbia dirscr'
pull ctime

say 'ctime='ctime

datei=subwrd(ctime,1)
nweek=subwrd(ctime,2)
case=subwrd(ctime,3)
dirfig=subwrd(ctime,4)
dirbia=subwrd(ctime,5)
dirscr=subwrd(ctime,6)
dirfig=dirfig%'/'
dirbia=dirbia%'/'
dirscr=dirscr%'/'

yyi=substr(datei,1,4)
mmi=substr(datei,5,2)
ddi=substr(datei,7,2)
hhi=substr(datei,9,2)

*
*   Open ctls
*
'run openctl.gs'

'set display color white'
'c'
'set grid off'
'set mpdset brmap_hires'

lati= -60.00; latf= 15.00;
loni=-101.25; lonf=-11.25;
'set lat 'lati' 'latf
'set lon 'loni' 'lonf

*
*   Get the end dates
*
t=1
datei2.1=datei
'q files'
while (t <= nweek)

  clm=3*t
  linfl=sublin(result,clm)
  wrdfl=subwrd(linfl,2)
  datef.t=substr(wrdfl,68,10)

  if (t>=2)
    tm1=t-1
    datei2.t=datef.tm1
  endif

  t=t+1
endwhile

*
*   Plot the pictures
*
t=1
while (t <= nweek)
clm=t
fct=t+nweek

'c'

yyf=substr(datef.t,1,4)
mmf=substr(datef.t,5,2)
ddf=substr(datef.t,7,2)
hhf=substr(datef.t,9,2)

yyi2=substr(datei2.t,1,4)
mmi2=substr(datei2.t,5,2)
ddi2=substr(datei2.t,7,2)
hhi2=substr(datei2.t,9,2)

'set parea 0.1 10.7 0.3 7.5'

'set grads off'
'run rgbset.gs'
'set gxout shaded'
'set map 1 1 5'
'set clevs -35 -30 -25 -20 -15 -10 -5 0 5 10 15 20 25 30 35'
*'set ccols 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95'
*'set ccols 95 94 93 92 91 90 89 88 87 86 85 84 83 82 81 80'
'colors  -levs -35 -30 -25 -20 -15 -10 -5 0 5 10 15 20 25 30 35 -kind maroon->white->darkgreen'
'pfct=prec.'fct'(t=1)'
'pclm=lterp(prec.'clm'(t=1),pfct)'
'anml=pfct-pclm'
'd anml'
'run cbar.gs'

'set font 0'
'set string 1 c 10'
'set strsiz 0.14 0.16'
*'draw string 5.5 8.3 CPTEC ENSEMBLE FORECAST - Precipitation anomaly'
'draw string 5.9 8.2 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'case''
'draw string 5.9 7.925 Anomalia Media Semanal de Precipitacao - Inicio da Previsao: 'ddi'/'mmi'/'yyi' 'hhi'Z'
'set string 1 c 6'
'set strsiz 0.13 0.18'
*'draw string 5.5 7.95 Week 't' Forecast from 'mmi'/'ddi'/'yyi', valid end date 'mmf'/'ddf'/'yyf'' 
'draw string 5.4 7.65 Valida para o periodo: 'ddi2'/'mmi2'/'yyi2' 'hhi2'Z  a  'ddf'/'mmf'/'yyf' 'hhf'Z' 
'set string 1 l 5'
'set strsiz 0.13 0.13'
*'draw string 9.8 1.0 (mm/day)'
'draw string 9.8 1.0 (mm/dia)'
say '++++ 'nweek' - 'fct' - 't' - 'datef.t

'printim 'dirfig'prec_anomaly_'t'week.png png x800 y600'
'printim 'dirfig'prec_anomaly_as'datei''datef.t'.png png x640 y480'
'!/usr/bin/convert -trim 'dirfig'prec_anomaly_as'datei''datef.t'.png  'dirfig'prec_anomaly_as'datei''datef.t'.png'

'printim 'dirfig'lprec_anomaly_as_'t'week.png png x800 y600'
'printim 'dirfig'prec_anomaly_as_'t'week.png png x640 y480'
'!/usr/bin/convert -trim 'dirfig'prec_anomaly_as_'t'week.png  'dirfig'prec_anomaly_as_'t'week.png'

t=t+1
endwhile

'quit'
*
