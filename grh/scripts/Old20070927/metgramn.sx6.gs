'reinit'
*
say 'Entre data inicial'
pull labeli
*labeli=2004010412
*
say 'Entre data final'
pull labelf
*labelf=2004011612
*
say 'Entre prefixo do nome'
pull nomea
*nomea=GFGN
*
say 'Entre resolucao'
pull trlv
*trlv=T126L28
*
say 'Entre reduzida para pressao reduzida'
say 'Entre psuperf  para pressao a surpeficie'
pull ps
*ps=psuperf
*
say 'Entre data para remocao de arquivos antigos'
pull labelr
*labelr=2003120912
*
say 'Entre com o numero de membros do ensemble'
pull nmemb
*nmemb=15
*
say 'Entre com o diretorio onde estao os ctls das plumas de probabilidade'
pull dirbct
dirbct=dirbct%'/'
*
yyi=substr(labeli,1,4)
mmi=substr(labeli,5,2)
ddi=substr(labeli,7,2)
hhi=substr(labeli,9,2)
*
*
*********
*
* nome do arquivo com prefixos dos pontos
*nomeb='prefixn'%labeli%labelf%'.'%trlv
nomeb=dirbct%'LOCMM'%labeli%labelf%'.'%trlv
* nomes dos arquivos com identificacao e local dos pontos
*nomec='identn'%labeli%labelf%'.'%trlv
*nomed='localn'%labeli%labelf%'.'%trlv
*
say 'nomeb='nomeb
say 'nmemb= 'nmemb
npert=(nmemb-1)/2
*n=1
*p=1
*while (n <= _npert)
*if (n < 10);n='0'n;endif
*_nomectl.p = _nomea%n%'P'%_labeli%_labelf%'M.unf.'%_trlv%'.ctl'
*say _nomectl.p
*n=n+1
*p=p+1
*endwhile
*n=1
*while (n <= _npert)
*if (n < 10);n='0'n;endif
*_nomectl.p = _nomea%n%'N'%_labeli%_labelf%'M.unf.'%_trlv%'.ctl'
*say _nomectl.p
*n=n+1
*p=p+1
*endwhile
*_nomectl.p = _nomea%'NMC'%_labeli%_labelf%'M.unf.'%_trlv%'.ctl'
*
*
rec=read(nomeb)
auxnloc=sublin(rec,2)
nloc=subwrd(auxnloc,1)
rec=read(nomeb)
*rec=read(nomec)
*nloc1=sublin(rec,2)
*rec=read(nomed)
*nloc2=sublin(rec,2)
*
*Abre os ctl's
*
say 'routine=openctl('dirbct','labeli','labelf','trlv','ps')'
routine=openctl(dirbct,labeli,labelf,trlv,ps)
say 'abriu os ctls'
*'q pos'
*
*Set tmin e tmax
*
tmin=1
tmax=360
'set t 'tmin' 'tmax
*
*Search intervals
*
'q files'
say result
linet=sublin(result,11)
lineh=sublin(result,14)
linev=sublin(result,17)
linep=sublin(result,20)
arqt=subwrd(linet,2)
arqh=subwrd(lineh,2)
arqv=subwrd(linev,2)
arqp=subwrd(linep,2)
say 'arqt='arqt
say 'arqh='arqh
say 'arqv='arqv
say 'arqp='arqp
l=1
while (l <= 10)
rect=read(arqt)
rech=read(arqh)
recv=read(arqv)
recp=read(arqp)
l=l+1
endwhile
rec=close(arqt)
rec=close(arqh)
rec=close(arqv)
rec=close(arqp)
intt=subwrd(rect,6)
inth=subwrd(rech,6)
intv=subwrd(recv,6)
intp=subwrd(recp,6)
say 'intt='intt
say 'inth='inth
say 'intv='intv
say 'intp='intp
intervt=math_format('%4.1f',intt)
intervh=math_format('%4.1f',inth)
intervv=math_format('%4.1f',intv)
intervp=math_format('%4.1f',intp)
say 'intervt='intervt
say 'intervh='intervh
say 'intervv='intervv
say 'intervp='intervp
*
say 'nloc='nloc
loc=0
while (loc < nloc)
   loc=loc+1
   'clear'
*   'set lon 'loc
   'set x 'loc
   say 'routine=dimet('nomeb','loc','ps','nmemb','trlv','labeli','labelf','labelr','tmin','tmax','intervt','intervh','intervv','intervp')'
   routine=dimet(nomeb,loc,ps,nmemb,trlv,labeli,labelf,labelr,tmin,tmax,intervt,intervh,intervv,intervp)
   say 'Fez o grafico:' loc
*   'q pos'
endwhile






*-----------------------------------------------------
*
*     Function dimet
*
*-----------------------------------------------------
function dimet(nomeb,loc,ps,nmemb,trlv,labeli,labelf,labelr,tmin,tmax,intervt,intervh,intervv,intervp)
*
'set display color white'
'clear'
*
'set vpage 0 8.5 9.5 11'
'set grads off'
*
rec=read(nomeb)
titaux=sublin(rec,2)
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
say 'routine=title('local','lonlat','loc','trlv','ps')'
rec=title(local,lonlat,loc,trlv,ps)
*'q pos'
*
'set t 'tmin' 'tmax
*
'set vpage 0 8.5 7.8 9.9'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
say 'routine=prec('nmemb')'
routine=prec(nmemb)
*'q pos'
*
*deltap=2.5;deltam=2.5;dinc=0.49999;ddec=0.5;dval=1;
*routine=plumasp(tems,deltap,deltam,dinc,ddec,dval)
*deltap=0.0;deltam=2.5;dinc=0.49999;ddec=0.5;dval=3;
*routine=plumasp(umrs,deltap,deltam,dinc,ddec,dval)
*deltap=2.5;deltam=0.0;dinc=0.49999;ddec=0.5;dval=2;
*routine=plumasp(vento,deltap,deltam,dinc,ddec,dval)
*deltap=3.5;deltam=3.5;dinc=0.49999;ddec=0.5;dval=3;
*if (ps=reduzida)
*routine=plumasp(psnm,deltap,deltam,dinc,ddec,dval)
*else
*routine=plumasp(pslc,deltap,deltam,dinc,ddec,dval)
*endif
*
*routine=closectl()
*
* Temperature
*
'set vpage 0 8.5 5.9 8.0'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
*'q files'
*say result
*linet=sublin(result,11)
*arqt=subwrd(linet,2)
*say 'arqt='arqt
*l=1
*while (l <= 10)
*rec=read(arqt)
*l=l+1
*endwhile
*intt=subwrd(rec,6)
*say 'intt='intt
*interv=math_format('%4.1f',intt)
*say 'interv='interv
say 'routine=plota(tems,'tetamin','tetamax','intervt','tmin','tmax')'
routine=plota(tems,tetamin,tetamax,intervt,tmin,tmax)
*'q pos'
*
* Relative Humidity
*
'set vpage 0 8.5 4.0 6.1'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
*'q files'
*say result
*linet=sublin(result,14)
*arqt=subwrd(linet,2)
*say 'arqt='arqt
*l=1
*while (l <= 10)
*rec=read(arqt)
*l=l+1
*endwhile
*intt=subwrd(rec,6)
*interv=math_format('%4.1f',intt)
*say 'interv='interv
say 'routine=plota(umrs,'hmin','hmax','intervh','tmin','tmax')'
routine=plota(umrs,hmin,hmax,intervh,tmin,tmax)
*'q pos'
*
* wind speed
*
'set vpage 0 8.5 2.1 4.2'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
*'q files'
*say result
*linet=sublin(result,17)
*arqt=subwrd(linet,2)
*say 'arqt='arqt
*l=1
*while (l <= 10)
*rec=read(arqt)
*l=l+1
*endwhile
*intt=subwrd(rec,6)
*interv=math_format('%4.1f',intt)
*say 'interv='interv
say 'routine=plota(vento,'wmin','wmax','intervv','tmin','tmax')'
routine=plota(vento,wmin,wmax,intervv,tmin,tmax)
*'q pos'
*
* pressure
*
*
'set vpage 0 8.5 0.2 2.3'
'set parea 0.7 8.0 0.3 1.75'
'set grads off'
*'q files'
*say result
*linet=sublin(result,20)
*arqt=subwrd(linet,2)
*say 'arqt='arqt
*l=1
*while (l <= 10)
*rec=read(arqt)
*l=l+1
*endwhile
*intt=subwrd(rec,6)
*interv=math_format('%4.1f',intt)
*say 'interv='interv
if (ps=reduzida)
say 'routine=plota(psnm,'pmin','pmax','intervp','tmin','tmax')'
routine=plota(psnm,pmin,pmax,intervp,tmin,tmax)
else
say 'routine=plota(pslc,'pmin','pmax','intervp','tmin','tmax')'
routine=plota(pslc,pmin,pmax,intervp,tmin,tmax)
endif
*'q pos'
*
*'set vpage 0 8.5 0.0 1.95'
*'set parea 0.7 8.0 0.3 1.6'
*'set grads off'
*routine=cbnv()
*
label=labeli%labelf
*rec=read(nomeb)
*lab=sublin(rec,2)
lab=lonlat
*
taga=gif
tagb=gif
tagc=gif87
*
*  Note: convert2 as it is do not work for jpeg
*        if want to use jpeg comment statment for convert2
*        therefore the ouput jpeg will not be cropped
*taga=jpg
*tagb=jpg
*tagc=jpeg
*
*  Note: if have problems with wi use staments below:
*say '!./meta2gif_gx -r -x 510 -y 660 -i meteogram -o 'lab''label'.gif'
*'!./meta2gif_gx -r -x 510 -y 660 -i meteogram -o '_state'/'lab''label'.gif'
*'!/home/grads/bin/gxgif -r -x 620 -y 760 -i meteogram -o '_state'/'lab''label'.gif'
*say '!./convert2 -crop 0x0 '_state'/'lab''label'.gif 'tagc':'_state'/'lab''label'.'tagb
*'!./convert2 -crop 0x0 '_state'/'lab''label'.gif 'tagc':'_state'/'lab''label'.'tagb
*
*say 'wi '_state'/'lab''label'.'taga
*'wi '_state'/'lab''label'.'taga
*say '!./convert2 -crop 0x0 '_state'/'lab''label'.'taga' 'tagc':'_state'/'lab''label'.'tagb
*'!./convert2 -crop 0x0 '_state'/'lab''label'.'taga' 'tagc':'_state'/'lab''label'.'tagb
*
say 'printim 'state'/'lab''label'.'taga' gif x620 y760'
'printim 'state'/'lab''label'.'taga' gif x620 y760'
say '!./convert2 -crop 0x0 'state'/'lab''label'.'taga' 'tagc':'state'/'lab''label'.'tagb
'!./convert2 -crop 0x0 'state'/'lab''label'.'taga' 'tagc':'state'/'lab''label'.'tagb
*
*'!rm -f meteogram'
if (loc=1)
'!rm -f puttag.'label'.out'
'!rm -f deltag.'label'.out'
endif
'!echo put 'state'/'lab''label'.'taga' >> puttag.'label'.out'
'!echo mdelete 'state'/'lab''labelr'*.'taga' >> deltag.'label'.out'
*
return









*-----------------------------------------------------
*
*     Function title 
*
*-----------------------------------------------------
function title(local,lonlat,loc,trlv,ps)
*
*rec=read(nomeb)
*local=sublin(rec,2)
*state=estado(local)
*rec=read(nomed)
*lonlat=sublin(rec,2)
*loi=substr(lonlat,1,3)
*lof=substr(lonlat,4,2)
*lo=substr(lonlat,6,1)
*lai=substr(lonlat,7,2)
*laf=substr(lonlat,9,2)
*la=substr(lonlat,11,1)
*lalo=loi%':'%lof%lo%'-'%lai%':'%laf%la
*
loi=substr(lonlat,1,3)
lof=substr(lonlat,4,2)
lo=substr(lonlat,6,1)
lai=substr(lonlat,7,2)
laf=substr(lonlat,9,2)
la=substr(lonlat,11,1)
lalo=loi%':'%lof%lo%'-'%lai%':'%laf%la
*
say ' '
say 'Plotando localizacao numero = 'loc'  Local: 'local
*
'set string 8 l 6'
'set strsiz .13 .14'
'draw string 0.4 1.3 PROBABILITY PLUMES - GLOBAL ENSEMBLE FORECAST - 'trlv
'draw string 0.4 1.1 CPTEC:'
'draw string 1.4 1.1 'lalo
'draw string 3.4 1.1 'local
*
'query files'
lbin=sublin(result,3)
bin=subwrd(lbin,2)
say 'bin='bin
utc=substr(bin,82,2)
say "+++UTC=>"utc
'set t 1'
'q dims'
tm = sublin(result,5)
tim = subwrd(tm,6)
tmm = substr(tim,4,9)
*
*Provisoriamente retira o topo
*
*if (ps!=reduzida)
*'display topo'
*_tpg=subwrd(result,4)
*endif
*
'set strsiz .125 .13'
'draw string 0.4 0.9 'tmm' 'utc'Z: Greenwhich Meridian Time: Vertical Dotted Line: Midnight'
*
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
'display smth9(prec.2)'
'set gxout stat'
'display smth9(neve.3)'
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
'display smth9(neve.3)'
endif
n=n+1
endwhile
if (status = 1)
'draw string 0.7 1.90 Ensemble Members of Precipitation (red) and Snow Fall (purple) (mm/h)'
else
'set cmark 0'
'set ccolor 1'
'set cthick 7'
'set vrange 0 5'
'set xlab on'
'display smth9(prec.1)'
'draw string 0.7 1.90 Ensemble Members of Precipitation (mm/h)'
endif
*
*'q pos'
return




*-----------------------------------------------------
*
*     Function plota 
*
*-----------------------------------------------------
*function plota(campo,yint,interv,tmin,tmax)
function plota(campo,ymin,ymax,interv,tmin,tmax)
*
*'open 'campo'.ctl'
*
*Os numeros "nfl" estao relacionados a ordem de
*abertura dos arquivos na funcao "openctl" 
*
if (campo = 'tems');nfl=4;endif;
if (campo = 'umrs');nfl=5;endif;
if (campo = 'vento');nfl=6;endif;
if ( (campo = 'pslc') | (campo = 'psnm'));nfl=7;endif;

'set gxout shaded'
'set grads off'
'set mproj off'

*'set dfile 1'
*'set t 1'
*'set y 1'
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

'set t 'tmin' 'tmax
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

*'q dims'
*liny=sublin(result,3)
*y1=subwrd(liny,6)
*y2=subwrd(liny,8)
*'set yaxis 'y1' 'y2' 'yint
'set xlab off'
'set clevs 1 20 40 60 80'
'set ccols 0 4 3 7 12 8'
'd smth9(prob.'nfl')*100'
*'close 1'
*'open '_nomectl._nmemb
*'set lon '_loc
*AMM 'set t 1 last'
*'set t 1 168'
*
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
*'close 1'
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
*     Function plumasp 
*
*-----------------------------------------------------
function plumasp(campo,deltap,deltam,dinc,ddec,dval)
*
*obtem os valores maximo e minimo
_min=1e30
_max=-1e30
n=1
while (n <= _nmemb)
'set gxout stat'
if (campo != 'vento')
'd 'campo'.'n''
else
'd mag(uves.'n',vves.'n')'
endif
lin8=sublin(result,8)
varn=subwrd(lin8,4)
varx=subwrd(lin8,5)
if (_min > varn);_min=varn;endif
if (_max < varx);_max=varx;endif
n=n+1
endwhile
_max=_max+deltap
_min=_min-deltam
say 'min= '_min
say 'max= '_max
*
if (_min <= -10.0)
imin=substr(_min,1,3)
else
if (_min < 0.0 & _min > -10.0)
imin=substr(_min,1,2)
else
if (_min <= -10.0)
imin=substr(_min,1,3)
else
if (_min >= 0.0 & _min < 10.0)
imin=substr(_min,1,1)
else
if (_min >= 10.0 & _min < 100.0)
imin=substr(_min,1,2)
else
if (_min >= 100.0 & _min < 1000.0)
imin=substr(_min,1,3)
else
if (_min >= 1000.0 & _min < 10000.0)
imin=substr(_min,1,4)
endif
endif
endif
endif
endif
endif
endif
*
if (_max <= -10.0)
imax=substr(_max,1,3)
else
if (_max < 0.0 & _max > -10.0)
imax=substr(_max,1,2)
else
if (_max >= 0.0 & _max < 10.0)
imax=substr(_max,1,1)
else
if (_max >= 10.0 & _max < 100.0)
imax=substr(_max,1,2)
else
if (_max >= 100.0 & _max < 1000.0)
imax=substr(_max,1,3)
else
if (_max >= 1000.0 & _max < 10000.0)
imax=substr(_max,1,4)
endif
endif
endif
endif
endif
endif
say 'imin= 'imin
say 'imax= 'imax
*
nintx=(imax-imin)/dval+1
*
if (nintx < 10.0)
nint=substr(nintx,1,1)
else
if (nintx >= 10.0 & nintx < 100.0)
nint=substr(nintx,1,2)
else
if (nintx >= 100.0 & nintx < 1000.0)
nint=substr(nintx,1,3)
endif
endif
endif
say 'numero de intervalos para 'campo': 'nint
*
*calcula a probabilidade para cada intervalo 
*
vali=imin-ddec
valf=imin+dval-1+dinc
*
m=1
while (m <= nint)
*say 'int= 'vali' a 'valf
'define prob'm'=0.0'
n=1
while (n <= _nmemb)
if (campo != 'vento')
'define valint=maskout(maskout('campo'.'n','campo'.'n'-'vali'),('valf')-'campo'.'n')'
else
'define valint=maskout(maskout(mag(uves.'n',vves.'n'),mag(uves.'n',vves.'n')-'vali'),'valf'-mag(uves.'n',vves.'n'))'
endif
'define freq'm''n'=const(const(valint,1),0.0,-u)'
'define prob'm'=prob'm'+freq'm''n''
n=n+1
endwhile
'define prob'm'=prob'm'/'_nmemb''
vali=vali+dval
valf=valf+dval
m=m+1
endwhile
*
'!rm 'campo'.bin'
'set fwrite 'campo'.bin'
'set gxout fwrite'
m=1
while (m <= nint)
'd prob'm''
m=m+1
endwhile
'disable fwrite'
'q time'
tin=subwrd(result,3)
routine=fazctl(campo,nint,imin,tin,dval)
*
say 'terminou a rotina plumasp'
return





*-----------------------------------------------------
*
*     Function fazctl 
*
*-----------------------------------------------------
*
function fazctl(name,ny,min,hinic,dval)
*
'q dims'
linetime=sublin(result,5)
ntimes=subwrd(linetime,13)
'!rm 'name'.ctl'
rec=write(name'.ctl','DSET ^'name'.bin')
rec=write(name'.ctl','*',append)
rec=write(name'.ctl','UNDEF -2.56E33',append)
rec=write(name'.ctl','*',append)
rec=write(name'.ctl','TITLE GRID POINT HISTORY',append) 
rec=write(name'.ctl','*',append)
rec=write(name'.ctl','XDEF  'ntimes' LINEAR  1 1',append)
rec=write(name'.ctl','YDEF   'ny' LINEAR 'min' 'dval,append)
rec=write(name'.ctl','ZDEF     1 LINEAR 1000 1',append)
rec=write(name'.ctl','TDEF     1 LINEAR  'hinic'  1HR',append)
rec=write(name'.ctl','*',append)
rec=write(name'.ctl','VARS     1',append)
rec=write(name'.ctl','prob  0 99 probabilidade ',append)
rec=write(name'.ctl','ENDVARS',append)
rec=close(name'.ctl')
*
return



*-----------------------------------------------------
*
*     Function openctl 
*
*-----------------------------------------------------
function openctl(dirbct,labeli,labelf,trlv,ps)
*
cnt='CONT'%labeli%labelf'.'%trlv'.ctl'
fl2='PREC'%labeli%labelf'.'%trlv'.ctl'
fl3='NEVE'%labeli%labelf'.'%trlv'.ctl'
fl4='TEMS'%labeli%labelf'.'%trlv'.ctl'
fl5='UMRS'%labeli%labelf'.'%trlv'.ctl'
fl6='VSUT'%labeli%labelf'.'%trlv'.ctl'
if (ps = reduzida)
   fl7='PSNM'%labeli%labelf'.'%trlv'.ctl'
else
   fl7='PSLC'%labeli%labelf'.'%trlv'.ctl'
endif
*
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
*
*p=1
*while (p <= _nmemb)
*'open '_nomectl.p
*p=p+1
*endwhile
*
return




*-----------------------------------------------------
*
*     Function closectl 
*
*-----------------------------------------------------
function closectl()
*
p=_nmemb
while (p >= 1)
'close 'p
p=p-1
endwhile
*
return
*
