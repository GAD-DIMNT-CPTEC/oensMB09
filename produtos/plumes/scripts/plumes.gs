say 'Enter with: labeli labelf gname trlv ps nmembers fileloc dirbct dirfig tmin tmax'
pull ctime

labeli  =subwrd(ctime,1)
labelf  =subwrd(ctime,2)
gname   =subwrd(ctime,3)
trlv    =subwrd(ctime,4)
ps      =subwrd(ctime,5)
nmembers=subwrd(ctime,6)
fileloc =subwrd(ctime,7)
dirbct  =subwrd(ctime,8)
dirfig  =subwrd(ctime,9)
tmin    =subwrd(ctime,10)
tmax    =subwrd(ctime,11)

dirbct=dirbct%'/'
dirfig=dirfig%'/'

yyi=substr(labeli,1,4)
mmi=substr(labeli,5,2)
ddi=substr(labeli,7,2)
hhi=substr(labeli,9,2)

rec=read(dirbct%fileloc)
auxnloc=sublin(rec,2)
status=sublin(rec,1)
say auxnloc' 'status
nloc=subwrd(auxnloc,1)
rec=read(dirbct%fileloc)

*
* Open ctl's
*

say 'routine=openctl('dirbct','labeli','labelf','trlv','ps')'
routine=openctl(dirbct,labeli,labelf,trlv,ps)

*
* Produce Figures
*

'set t 'tmin' 'tmax

say 'nloc='nloc
loc=0
while (loc < nloc)
   loc=loc+1
  'clear'
  'set x 'loc
   say 'routine=dimet('dirbct','dirfig','fileloc','loc','ps','nmembers','trlv','labeli','labelf','hhi','tmin','tmax')'
   routine=dimet(dirbct,dirfig,fileloc,loc,ps,nmembers,trlv,labeli,labelf,hhi,tmin,tmax)
   say 'Made Figure:' loc
*  'q pos'
endwhile



*-----------------------------------------------------
*
*     Function dimet
*
*-----------------------------------------------------
function dimet(dirbct,dirfig,fileloc,loc,ps,nmembers,trlv,labeli,labelf,hhi,tmin,tmax)
*
'set display color white'
'clear'
*
'set vpage 0 8.5 9.5 11'
'set grads off'
*
rec=read(dirbct%fileloc)
titaux=sublin(rec,2)
say 'titaux:'titaux
local=substr(titaux,6,40)
say 'state=estado('local')'
state=estado(local)
say 'state='state
lonlat=substr(titaux,47,11)
taux=substr(titaux,59,17)
haux=substr(titaux,77,17)
paux=substr(titaux,95,17)
waux=substr(titaux,113,17)
tetamax=subwrd(taux,1);tetamin=subwrd(taux,2);
hmax=subwrd(haux,1);hmin=subwrd(haux,2);
pmax=subwrd(paux,1);pmin=subwrd(paux,2);
wmax=subwrd(waux,1);wmin=subwrd(waux,2);
*

'set t 'tmin
say 'routine=title('local','lonlat','loc','trlv','ps')'
rec=title(local,lonlat,loc,trlv,hhi,ps)
*'q pos'

'set t 'tmin' 'tmax
'set vpage 0 8.5 7.8 9.9'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
say 'routine=prec('nmembers')'
routine=prec(nmembers)
*'q pos'

*
* Temperature
*
'set vpage 0 8.5 5.9 8.0'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
say 'routine=plota(tems,'tetamin','tetamax','_inttem','tmin','tmax')'
routine=plota(tems,tetamin,tetamax,_inttem,tmin,tmax)
*'q pos'

*
* Relative Humidity
*
'set vpage 0 8.5 4.0 6.1'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
say 'routine=plota(umrs,'hmin','hmax','_intumr','tmin','tmax')'
routine=plota(umrs,hmin,hmax,_intumr,tmin,tmax)
*'q pos'

*
* wind speed
*
'set vpage 0 8.5 2.1 4.2'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
say 'routine=plota(vento,'wmin','wmax','_intvsu','tmin','tmax')'
routine=plota(vento,wmin,wmax,_intvsu,tmin,tmax)
*'q pos'

*
* pressure
*
*
'set vpage 0 8.5 0.2 2.3'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
if (ps=reduzida)
say 'routine=plota(psnm,'pmin','pmax','_intpss','tmin','tmax')'
routine=plota(psnm,pmin,pmax,_intpss,tmin,tmax)
else
say 'routine=plota(pslc,'pmin','pmax','_intpss','tmin','tmax')'
routine=plota(pslc,pmin,pmax,_intpss,tmin,tmax)
endif
*'q pos'

label=labeli%labelf
lab=lonlat
*

*  Note: convert as it is do not work for jpeg
*        if want to use jpeg comment statment for convert
*        therefore the ouput jpeg will not be cropped

say 'printim 'dirfig''state'/'lab'.png png x620 y760'
    'printim 'dirfig''state'/'lab'.png png x620 y760'
*say '!/usr/bin/convert -trim 'dirfig''state'/'lab'.png 'dirfig''state'/'lab'.png'
say '!/home/carlos.bastarz/bin/convert -trim 'dirfig''state'/'lab'.png 'dirfig''state'/'lab'.png'
*    '!/usr/bin/convert -trim 'dirfig''state'/'lab'.png 'dirfig''state'/'lab'.png'
    '!/home/carlos.bastarz/bin/convert -trim 'dirfig''state'/'lab'.png 'dirfig''state'/'lab'.png'

return


*-----------------------------------------------------
*
*     Function title 
*
*-----------------------------------------------------
function title(local,lonlat,loc,trlv,utc,ps)  

loi=substr(lonlat,1,3)
lof=substr(lonlat,4,2)
lo=substr(lonlat,6,1)
lai=substr(lonlat,7,2)
laf=substr(lonlat,9,2)
la=substr(lonlat,11,1)
lalo=loi%':'%lof%lo%'-'%lai%':'%laf%la

say ' '
say 'Plotting localization number = 'loc'  Place: 'local

'set string 8 l 6'
'set strsiz .13 .14'
'draw string 0.4 1.3 PROBABILITY PLUMES - GLOBAL ENSEMBLE FORECAST - 'trlv
'draw string 0.4 1.1 CPTEC:'
'draw string 1.4 1.1 'lalo
'draw string 3.4 1.1 'local

'q dims'
tm = sublin(result,5)
tim = subwrd(tm,6)
tmm = substr(tim,4,9)

'set strsiz .125 .13'
'draw string 0.4 0.9 'tmm' 'utc'Z: Greenwhich Meridian Time: Vertical Dotted Line: Midnight'

'display topo'
tpg=subwrd(result,4)
tpg=math_format('%4.0f',tpg)
'set string 6 l 5'
'set strsiz 0.12 0.13'
'set strsiz 0.09 0.098'
'draw string 0.45 0.41 Model Altitude: 'tpg' m'

'set line 4'
'draw recf 0.45 0.50 0.85 0.72'
'set line 3'
'draw recf 1.85 0.50 2.25 0.72'
'set line 7'
'draw recf 3.25 0.50 3.65 0.72'
'set line 12'
'draw recf 4.65 0.50 5.05 0.72'
'set line 8'
'draw recf 6.05 0.50 6.45 0.72'
'set line 1 1 7'
'draw line 6.05 0.38 6.45 0.38' 
*
'set strsiz .09 .098'
'draw string 0.85 0.6 1 - 20 %'
'draw string 2.25 0.6 20 - 40 %'
'draw string 3.65 0.6 40 - 60 %'
'draw string 5.05 0.6 60 - 80 %'
'draw string 6.47 0.61 80 - 100 %'
'draw string 6.48 0.41 Control Forecast'
*
return






*-----------------------------------------------------
*
*     Function prec  
*
*-----------------------------------------------------
function prec(nmemb)
*

'set dfile '_fldprc

'set vrange 0 5'
'set ylint 1'
'set grads off'
'set gxout line'

status=0
n=1
while (n <= nmemb)
'set y 'n
'set ccolor 2'
'set cmark 0'
np=_fldprc
*'display smth9(prec.'np')'
'display prec.'np''
np=_fldnev
'set gxout stat'
*'display smth9(neve.'np')'
'display neve.'np''
lnv=sublin(result,8)
nv=subwrd(lnv,5)
'set gxout line'
'set string 6 l 5'
'set strsiz 0.12 0.13'
'set line 0'
'draw recf 0.75 1.80 8.5 2.0'
if (nv > 0.0001)
status=1
'set ccolor 9'
'set cmark 0'
*'display smth9(neve.'np')'
'display neve.'np''
endif
n=n+1
endwhile

np=1
'set dfile 1'
'set y 1'
if (status = 1)
'set cmark 0'
'set ccolor 1'
'set ccolor 2'
'set cthick 6'
'set vrange 0 5'
'set xlab on'
*'display smth9(prec.'np')'
'display prec.'np''
'set cmark 0'
'set ccolor 9'
'set vrange 0 5'
*'display smth9(neve.'np')'
'display neve.'np''
'draw string 0.7 1.90 Ensemble Members of Precipitation (red) and Snow Fall (purple) (mm/h)'
else
'set cmark 0'
'set ccolor 1'
'set ccolor 2'
'set cthick 6'
'set vrange 0 5'
'set xlab on'
*'display smth9(prec.'np')'
'display prec.'np''
'draw string 0.7 1.90 Ensemble Members of Precipitation (mm/h)'
endif

*'q pos'
return




*-----------------------------------------------------
*
*    Function plota 
*
*-----------------------------------------------------
function plota(campo,ymin,ymax,interv,tmin,tmax)

*Os numeros "_fld???" estao relacionados a ordem de
*abertura dos arquivos na funcao "openctl" 

if (campo = 'tems');nfl=_fldtem;endif;
if (campo = 'umrs');nfl=_fldumr;endif;
if (campo = 'vento');nfl=_fldvsu;endif;
if ( (campo = 'pslc') | (campo = 'psnm') );nfl=_fldpss;endif;

'set gxout shaded'
'set grads off'
'set mproj off'

if (campo = 'tems')
   y1 = math_nint(ymin-1)
   y2 = math_nint(ymax+1)
endif
if (campo = 'umrs')
   y1 = math_nint(ymin-3)
   y2 = math_nint(ymax+3)
   if (y1 < 0);  y1=0;  endif; 
   if (y2 > 100);y2=100;endif; 
endif
if (campo = 'vento')
   y1 = math_nint(ymin-1)
   y2 = math_nint(ymax+1)
   if (y1 < 0);  y1=0;  endif; 
endif
if ( (campo = 'pslc') | (campo = 'psnm'))
   y1 = math_nint(ymin-5)
   y2 = math_nint(ymax+5)
endif
yint=math_nint((y2-y1)/5)

'set dfile 'nfl
'set lat 'y1' 'y2
'set xyrev on'
'set yaxis 'y1' 'y2' 'yint
'set xlab off'

say 'y1='y1
say 'y2='y2
say 'yint='yint
say 'tmin='tmin
say 'tmax='tmax

*'set xlab off'
'set clevs 1 20 40 60 80'
'set ccols 0 4 3 7 12 8'
'd smth9(prob.'nfl')*100'

'set dfile 1'
'set y 1'
'set xyrev off'
'set gxout line'
'set cmark 0'
'set ccolor 1'
'set cthick 7'
'set vrange 'y1' 'y2' 'yint
'set xlab on'
if (campo != 'vento')
'd smth9('campo'.1)'
else
'd smth9(mag(uves.1,vves.1))'
endif

'set string 6 l 5'
'set strsiz .12 .13'
'set line 0'
'draw recf 0.75 1.80 8.5 2.0'
if (campo = 'tems')
'draw string 0.7 1.90 Surface Temperature (`aO`nC) - Probability for 'interv' deg intervals'
else
if (campo = 'umrs')
'draw string 0.7 1.90 Relative Humidity (%) - Probability for 'interv'% intervals'
else
if (campo = 'vento')
'draw string 0.7 1.90 Surface Wind (m/s) - Probability for 'interv' m/s intervals'
else
if (campo = 'pslc')
'draw string 0.7 1.90 Surface Pressure (hPa) - Probability for 'interv' hPa intervals'
'draw recf 0 0 8.5 0.1'
else
if (campo = 'psnm')
'draw string 0.7 1.90 Mean Sea Level Pressure (hPa) - Probability for 'interv' hPa intervals '
'draw recf 0 0 8.5 0.1'
endif
endif
endif
endif
endif
*
*'q pos'
return





*-----------------------------------------------------
*
*     Function estado 
*
*-----------------------------------------------------
function estado(local)
*
est.1='AC'
est.2='AL'
est.3='AM'
est.4='AP'
est.5='BA'
est.6='CE'
est.7='DF'
est.8='ES'
est.9='GO'
est.10='MA'
est.11='MG'
est.12='MS'
est.13='MT'
est.14='PA'
est.15='PB'
est.16='PE'
est.17='PI'
est.18='PR'
est.19='RJ'
est.20='RN'
est.21='RO'
est.22='RR'
est.23='RS'
est.24='SC'
est.25='SE'
est.26='SP'
est.27='TO'
*
i=1
c=substr(local,i,1)
while (c != '(')
i=i+1
c=substr(local,i,1)
if (i > 40)
break
endif
endwhile
j=1
c=substr(local,j,1)
while (c != ')')
j=j+1
c=substr(local,j,1)
if (j > 40)
break
endif
endwhile
if (i > 40 | j > 40)
state='ZZ'
else
i=i+1
j=j-i
state=substr(local,i,j)
k=0
l=0
while (k < 27)
k=k+1
if (state = est.k)
l=1
endif
endwhile
endif
if (l = 0)
state='WW'
endif
*
return state



*-----------------------------------------------------
*
*     Function openctl 
*
*-----------------------------------------------------

function openctl(dirbct,labeli,labelf,trlv,ps)

cnt='CONT'%labeli%labelf'.'%trlv'.ctl'
_fldcnt=1
_intcnt=0
fl2='PREC'%labeli%labelf'.'%trlv'.ctl'
_fldprc=2
_intprc=0
fl3='NEVE'%labeli%labelf'.'%trlv'.ctl'
_fldnev=3
_intnev=0
fl4='TEMS'%labeli%labelf'.'%trlv'.ctl'
_fldtem=4
_inttem=1.0
fl5='UMRS'%labeli%labelf'.'%trlv'.ctl'
_fldumr=5
_intumr=3.0
fl6='VSUT'%labeli%labelf'.'%trlv'.ctl'
_fldvsu=6
_intvsu=2.0
if (ps = reduzida)
   fl7='PSNM'%labeli%labelf'.'%trlv'.ctl'
   _fldpss=7
   _intpss=3.0
else
   fl7='PSLC'%labeli%labelf'.'%trlv'.ctl'
   _fldpss=7
   _intpss=3.0
endif

say 'open 'dirbct%cnt
say 'open 'dirbct%fl2
say 'open 'dirbct%fl3
say 'open 'dirbct%fl4
say 'open 'dirbct%fl5
say 'open 'dirbct%fl6
say 'open 'dirbct%fl7
'open 'dirbct%cnt
'open 'dirbct%fl2
'open 'dirbct%fl3
'open 'dirbct%fl4
'open 'dirbct%fl5
'open 'dirbct%fl6
'open 'dirbct%fl7

return


