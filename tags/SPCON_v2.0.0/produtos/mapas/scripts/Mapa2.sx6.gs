*************************************************
* Gera mapas das variaveis p/ America do Sul    *
*            Luiz A. Candido                    *
*              Maio/2003                        *
*************************************************
_io=0
'set vpage 0 8.5 0 11'
'set parea 0.4 8.35 0.4 10.6'
pull argumentos
labi=subwrd(argumentos,1)
_nreg=subwrd(argumentos,2)
ndias=subwrd(argumentos,3)
nctl_icn=subwrd(argumentos,4)
nctl_fct=subwrd(argumentos,5)
nctl_fctold=subwrd(argumentos,6)
*
*   Regioes do Globo
*
regioes='Pnor Psul Trop SAmr'
*nomectls='gposicn'labi'.ctl gposfct'labi'.ctl'
nomectls=nctl_icn' 'nctl_fct
variaveis='omeg potv fcor advct advcv conh uvel'
output='Omeg PotV Fcor AdvT AdvV ConH Wind'
niveis='925 850 500 200'
no.1='850 500 200   1'
no.2='925 200   1   1'
no.3='925 200   1   1'
no.4='925 850 500   1'
no.5='925 850 500   1'
no.6='925 850   1   1'
no.7='925 850 500 200' 
factor1=' 1.0  1.0E-6  1.0E-7    86400.0 86400.0E5 86400.0E3  1.0' 
factor2='+0.0 +0.0000    +0.0000    +0.0    +0.000    +0.0   +0.0' 
tempo1='2 6 10 14 18 21 26 30 34 38 42 46'
tempo2='5 9 13 17 21 25 29 33 36 40 44 48'
'run rgb.gs'
'run font.gs 3'
regiao=subwrd(regioes,_nreg)
_lonW=-180
_lonE=180
if(_nreg=1)
_latS=30
_latN=90
_proj='nps'
_polo='PN'
endif
if(_nreg=2)
_latS=-90
_latN=-30
_proj='sps'
_polo='PS'
endif
if(_nreg=3)
_latS=-45
_latN=45
_proj='latlon'
_polo=' '
endif
if(_nreg=4)
_lonW='-100'
_lonE='-20'
_latS='-60'
_latN='15'
_proj='latlon'
_polo=' '
endif
nctl=1
while(nctl<=2)
ctl=subwrd(nomectls,nctl)
'open 'ctl''
  nv=1
  while(nv<=7)
  nlev=1
  while(nlev<=4)
  nivel=subwrd(no.nv,nlev)
  rc=subwrd(no.nv,nlev+1)
  if(rc=1)
    nlev=4
  endif
  _nvl=nivel
******************************************
if(nctl=1)
  'set lev 'nivel
  'set lon '_lonW' '_lonE
  'set lat '_latS' '_latN  
  'dx=6.37e6*3.1416*cdiff(lon,x)*cos(lat*3.1416/180)/180'
  'dy=6.37e6*3.1416*cdiff(lat,y)/180'
  nome=subwrd(variaveis,nv)
  nomeout=subwrd(output,nv)
  fac1=subwrd(factor1,nv)
  fac2=subwrd(factor2,nv)
  var3='vvel'
  rotina=labtime(1)
  labf=_lab
  if(nv=4)
  rotina=AdvT(1,advct)
  endif
  if(nv=5)
  rotina=AdvV(1,advcv)
  endif
  if(nv=6)
  rotina=Conv(1,conh)
  endif
  rotina=def(nome,1,fac1,fac2,var)
  rotina=varsgs(nv,1,labi,labf,var,var3)
  figura=nomeout%nivel%regiao%labf
  rotina=gif(figura)
  'undefine var'
endif
******************************************
if(nctl=2)
  k=1
  while(k<=ndias)
  tm1=subwrd(tempo1,k)
  tm2=subwrd(tempo2,k)
  'set t 'tm2
  rotina=labtime(tm2)
  labf=_lab
  'set lev 'nivel
  'set lon '_lonW' '_lonE
  'set lat '_latS' '_latN  
  'dx=6.37e6*3.1416*cdiff(lon,x)*cos(lat*3.1416/180)/180'
  'dy=6.37e6*3.1416*cdiff(lat,y)/180'
  nome=subwrd(variaveis,nv)
  nomeout=subwrd(output,nv)
  fac1=subwrd(factor1,nv)
  fac2=subwrd(factor2,nv)
  var3='vvel'  
  if(nv=4)
  rotina=AdvT(tm2,advct)
  endif
  if(nv=5)
  rotina=AdvV(tm2,advcv)
  endif
  if(nv=6)
  rotina=Conv(tm2,conh)
  endif
  rotina=def(nome,tm2,fac1,fac2,var)
  rotina=varsgs(nv,tm2,labi,labf,var,var3)
  figura=nomeout%nivel%regiao%labf
  rotina=gif(figura)
  'undefine var' 
  k=k+1
  endwhile
endif
******************************************
nlev=nlev+1
endwhile
nv=nv+1
endwhile
'close 1'
nctl=nctl+1
endwhile

'quit'
*************************************************
function def(var,tm,mult,sum,var1)
'set lon '%_lonW' '%_lonE
'set lat '%_latS' '%_latN
'set t 'tm
'define 'var1'='mult%'*'%var%sum
return
*************************************************
function subtitle(labi,labf)
'query w2xy '%_lonW%' '%_latN
yt=subwrd(result,6)
'set strsiz 0.145'
'set string 1 bc 6'
'draw string 4.25 'yt+0.7' CPTEC/INPE/MCT - GLOBAL MODEL - T213L42'
if (labi=labf)
'draw string 4.25 'yt+0.4' ANALYSIS: 'labi
'draw string 4.25 'yt+0.1' '_title
else
'draw string 4.25 'yt+0.4' FORECAST FROM: 'labi' VALID FOR: 'labf
'draw string 4.25 'yt+0.1' '_title
endif
return 
**********************************************************************
function gif(figura) 
_io=_io+1
'printim 'figura'.png png'
'!/usr/bin/convert -trim 'figura'.gif gif87:'figura'.gif'
'set grads on'
return
**********************************************************************
function labtime(tin)
'set t 'tin
'query dims'
ltime=sublin(result,5);time=subwrd(ltime,6)
yy=substr(time,9,4);mh=substr(time,6,3);dd=substr(time,4,2);hh=substr(time,1,2)
frase1='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
frase2='jan feb mar apr may jun jul aug sep oct nov dec'
frase3='01 02 03 04 05 06 07 08 09 10 11 12'
n=1
while(n<=12)
mes1=subwrd(frase1,n)
mes2=subwrd(frase2,n)
if (mh=mes1 | mh=mes2)
mm=subwrd(frase3,n)
endif
n=n+1
endwhile
_lab=yy%mm%dd%hh
return
*****************************************************
function AdvT(tm,advct)
* Adveccao de temperatura
'set lon '%_lonW' '%_lonE
'set lat '%_latS' '%_latN
'set lev '%_nvl
'set t 'tm
'advct=-uvel*cdiff(temp,x)/dx-vvel*cdiff(temp,y)/dy'
return
*****************************************************
function AdvV(tm,advcv)
* Adveccao de vorticidade
'set lon '%_lonW' '%_lonE
'set lat '%_latS' '%_latN
'set lev '%_nvl
'set t 'tm
'advcv=-uvel*cdiff(vort,x)/dx-vvel*cdiff(vort,y)/dy'
return
*****************************************************
function Conv(tm,conh)
* Convergencia de umidade
'set lon '%_lonW' '%_lonE
'set lat '%_latS' '%_latN
'set lev '%_nvl
'set t 'tm
'conh=-cdiff(uvel*umes,x)/dx-cdiff(vvel*umes,y)/dy)'
return
*****************************************************
function varsgs(nv,tm,labi,labf,var1,var2)
'set display color white'
'clear'
'set t 'tm
_preench='contour'
nivel=_nvl
rotina=title(nivel,nv)
'set grads off'
'set map 15 1 6'
'set mpdset mres'
'set lon '%_lonW' '%_lonE
'set lat '%_latS' '%_latN
'set lev '%_nvl
if(nv=7)
'set gxout stream'
'set strmden 4'
'set cthick 6'
'set clevs '_clvs
'display 'var1';'var2';mag('var1','var2')'
'run cbarn.gs 1.1 0 4.25 0.4'
else
***********************
'set gxout '_preench
if (_preench!="contour")
'set clevs '_clvs
'set ccols '_cor
else 
'set clab on'
endif
'display smth9('var1')'
if (_preench!="contour")
'query dims'
lx=sublin(result,2)
ly=sublin(result,3)
xa=subwrd(lx,6)
xb=subwrd(lx,8)
ya=subwrd(ly,6)
yb=subwrd(ly,8)
'query gxinfo'
lx=sublin(result,3)
ly=sublin(result,4)
xc=subwrd(lx,4)
xd=subwrd(lx,6)
yc=subwrd(ly,4)
yd=subwrd(ly,6)
dy=0.0
if (xa=-120 | xb=0 | ya=-120 | yb=30)
dy=0.5375
endif
if (xa=-90 | xb=-30 | ya=-40 | yb=15)
dy=0.175
endif
ymid=0.25*yc+dy
'run cbarn.gs 1.1 0 4.25 'ymid
'set gxout contour'
'set ccolor 1'
'set clevs '_clvs
'set clab off'
'display smth9('var1')'
endif
endif
rotina=subtitle(labi,labf)
return
**************************************************
function title(nivel,nv)
if(nv=1 & nivel > 200)
_preench='shaded'
_title='`3w`0 at 'nivel' hPa (Pa/s)'
_clvs='-0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7'
_cor='79 77 75 73 71 69 67 65 50 51 53 55 57 58 59 60'
endif
if(nv=1 & nivel = 200)
_preench='shaded'
_title='`3w`0 at 200 hPa (Pa/s)'
_clvs='-0.35 -0.3 -0.25 -0.2 -0.15 -0.1 -0.05 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35'
_cor='79 77 75 73 71 69 67 65 50 51 53 55 57 58 59 60'
endif
**************************
if(nv=2 & nivel > 200)
_preench='shaded'
_title='`3X`0 at 'nivel' hPa (10`a6`n m`a2`n/s)'
_clvs='-18 -16 -14 -12 -10 -8 -6 -4 -2 0 1 2 3 4 5 6 7'
_cor='79 78 77 76 75 73 71 69 67 65 50 51 52 53 54 55 56 57'
endif
if(nv=2 & nivel = 200)
_preench='shaded'
_title='`3X`0 at 'nivel' hPa (10`a6`n m`a2`n/s)'
_clvs='-10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16 18 20 22'
_cor='75 73 71 69 67 65 50 51 52 53 54 55 56 57 58 59 60 61'
endif
**************************
if(nv=3 & nivel > 200)
_preench='shaded'
_title='`3y`0 at 'nivel' hPa (10`a7`n m`a2`n/s)'
_clvs='-4.5 4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3'
_cor='59 58 57 56 55 54 53 52 51 50 66 67 68 69 70 71 72'
endif
if(nv=3 & nivel = 200)
_preench='shaded'
_title='`3y`0 at 'nivel' hPa (10`a7`n m`a2`n/s)'
_clvs='-5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13'
_cor='55 54 53 52 51 50 66 67 68 69 70 71 72 73 74 75 76 77 78 79'
endif
**************************
if(nv=4 & nivel > 200)
_preench='shaded'
_title='ADV OF TEMP at 'nivel' hPa (K/day)'
_clvs='-16 -14 -12 -10 -8 -6 -4 -2 0 2 4 6 8 10 12 14 16'
_cor='58 57 56 55 54 53 52 51 50 65 66 67 68 69 70 71 72 73'
endif
**************************
if(nv=5 & nivel > 200)
_preench='shaded'
_title='ADV OF VORT AT 'nivel' hPa (10`a-5`n/s/day)'
_clvs='-18 -14 -10 -6 -4 -2 1 0 1 2 4 6 10 14 18'
_cor='73 72 71 70 69 68 67 66 51 52 53 54 55 56 57 58'
endif
**************************
if(nv=6 & nivel > 200)
_preench='shaded'
_title='CONV OF HUM at 'nivel' hPa (g/Kg/day)'
_clvs='-21 -18 -15 -12 -9 -6 -3 0 3 6 9 12 15 18 21'
_cor='79 77 75 73 71 69 67 65 50 51 52 53 54 55 56 57 58'
endif
**************************
if(nv=7 & nivel>200)
_title='HORIZONTAL WIND at 'nivel' hPa (m/s)'
_clvs='2.5 5 7.5 10 12.5 15 17.5 20 22.5 25 27.5 30 32.5 35'
endif
if(nv=7 & nivel=200)
_title='HORIZONTAL WIND at 'nivel' hPa (m/s)'
_clvs='5 10 15 20 25 30 35 40 45 50 55 60 65 70'
endif
return
