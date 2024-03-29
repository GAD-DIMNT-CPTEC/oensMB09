say 'put datei datef nweek case trc dirfig'
pull ctime

say 'ctime='ctime

datei=subwrd(ctime,1)
datef=subwrd(ctime,2)
nweek=subwrd(ctime,3)
case =subwrd(ctime,4)
trc  =subwrd(ctime,5)
dirfig=subwrd(ctime,6)
dirfig=dirfig%'/'
convert=subwrd(ctime,7)

yyi=substr(datei,1,4)
mmi=substr(datei,5,2)
ddi=substr(datei,7,2)
hhi=substr(datei,9,2)

yyf=substr(datef,1,4)
mmf=substr(datef,5,2)
ddf=substr(datef,7,2)
hhf=substr(datef,9,2)

*
*   Obtain Ctl files and final dates
*

lstfct=filefct''datei'.'trc
t=1
while (t <= nweek)
   rec=read (lstfct)
   opnfct.t=sublin(rec,2)
   say 'opnfct.'t'='opnfct.t
  'open 'opnfct.t
  'q time'
   pent.t=subwrd(result,3) 
  'close 1'
   t=t+1
endwhile

*
*   Open ctls
*

t=1
while (t <= nweek)
  'open 'opnfct.t
   t=t+1
endwhile

'set display color white'
'c'
'set grid off'
'set mpdset mres'

lati=-20; latf= 20;
loni=  0; lonf=360;
'set lat 'lati' 'latf
'set lon 'loni' 'lonf

'set lev 200'

*
*   Plot the figures
*

'run rgbset.gs'
'set gxout shaded'
'set map 1 1 5'
'set xlab off'

'c'
t=1
while (t <= nweek)
fct=t

'set parea 'position(fct)
'set grads off'
'set ylint 10'
'set clevs -2 -1.5 -1.25 -1 -0.75 -0.5 -0.25 0.25 0.5 0.75 1 1.25 1.5 2'
'set ccols 37 36 35 34 33 32 31 0 21 22 23 24 25 26 27 28'
'pfct=potv.'fct'(t=1)*1e-7'
'd pfct'

t=t+1
endwhile

'run cbarn.gs 0.7 0 4.25 5.2'

'set font 0'
'set string 2 c 10'
'set strsiz 0.14 0.14'
*'draw string 4.25  9.95 CHI 200 hPa 15-DAY forecast ('pent.1'-'pent.nweek')'
*'draw string 4.25  9.70 (based on CPTEC ENSEMBLE FORECAST)'
'draw string 4.25  9.80 Potencial de Velocidade em 200 hPa - 15 dias de previsao'
'draw string 4.25  9.60 (baseado nas Previsoes de Tempo por Ensemble do CPTEC/INPE)'
'set strsiz 0.09 0.09'
'set string 2 c 6 90'
*'draw string 8.1 9.0 ANALYSIS'
*'draw string 8.1 8.0 DAY5 FCST'
*'draw string 8.1 7.0 DAY10 FCST'
*'draw string 8.1 6.0 DAY15 FCST'
'draw string 8.1 9.0 ANALISE'
'draw string 8.1 8.0 PREV DIA5'
'draw string 8.1 7.0 PREV DIA10'
'draw string 8.1 6.0 PREV DIA15'
'set strsiz 0.08 0.08'
'set string 4 c 6 90'

*'draw string 8.3 9.0 'pent.1''
*'draw string 8.3 8.0 'pent.2''
*'draw string 8.3 7.0 'pent.3''
*'draw string 8.3 6.0 'pent.4''
 date=mon_eng2port(pent.1)
'draw string 8.3 9.0 'date''
 date=mon_eng2port(pent.2)
'draw string 8.3 8.0 'date''
 date=mon_eng2port(pent.3)
'draw string 8.3 7.0 'date''
 date=mon_eng2port(pent.4)
'draw string 8.3 6.0 'date''

'set line 1 1 6'
'draw line 0.5 6.5 8.4 6.5' 
'draw line 0.5 7.5 8.4 7.5' 
'draw line 0.5 8.5 8.4 8.5' 

'printim 'dirfig'chi_evol'datei''datef'.png png x768 y1024'
'!'convert' -trim 'dirfig'chi_evol'datei''datef'.png 'dirfig'chi_evol'datei''datef'.png'

*'q pos'
'quit'

*
* Functions
*
function position(n)
fig.1=' 0.50 8.00 7.00 11.00'
fig.2=' 0.50 8.00 6.00 10.00'
fig.3=' 0.50 8.00 5.00  9.00'
fig.4=' 0.50 8.00 4.00  8.00'
return fig.n


function mon_eng2port(date)
  st1=substr(date,1,5)
  mme=substr(date,6,3)
  st2=substr(date,9,4)
  if (mme = 'JAN' | mme = 'jan'); mmp='JAN';endif
  if (mme = 'FEB' | mme = 'feb'); mmp='FEV';endif
  if (mme = 'MAR' | mme = 'mar'); mmp='MAR';endif
  if (mme = 'APR' | mme = 'apr'); mmp='ABR';endif
  if (mme = 'MAY' | mme = 'may'); mmp='MAI';endif
  if (mme = 'JUN' | mme = 'jun'); mmp='JUN';endif
  if (mme = 'JUL' | mme = 'jul'); mmp='JUL';endif
  if (mme = 'AUG' | mme = 'aug'); mmp='AGO';endif
  if (mme = 'SEP' | mme = 'sep'); mmp='SET';endif
  if (mme = 'OCT' | mme = 'oct'); mmp='OUT';endif
  if (mme = 'NOV' | mme = 'nov'); mmp='NOV';endif
  if (mme = 'DEC' | mme = 'dec'); mmp='DEZ';endif

  date=st1%mmp%st2
  
return date
