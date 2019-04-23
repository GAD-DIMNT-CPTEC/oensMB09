'reinit'

pull argumentos

_labeli=subwrd(argumentos,1);_labelf=subwrd(argumentos,2);_nomea=subwrd(argumentos,3);_trlv=subwrd(argumentos,4)
_ps=subwrd(argumentos,5);_labelr=subwrd(argumentos,6)
_time1=subwrd(argumentos,7);_time2=subwrd(argumentos,8);_prefix=subwrd(argumentos,9)

* nome do arquivo com prefixos dos pontos
_nomeb='/scratchout/oper/io/oensMB09/pos/dataout/TQ0126L028/2015040100/Preffix'%_prefix%_labeli%_labelf%'.'%_trlv

* nomes dos arquivos com identificacao e local dos pontos
_nomec='/scratchout/oper/io/oensMB09/pos/dataout/TQ0126L028/2015040100/Identif'%_prefix%_labeli%_labelf%'.'%_trlv
_nomed='/scratchout/oper/io/oensMB09/pos/dataout/TQ0126L028/2015040100/Localiz'%_prefix%_labeli%_labelf%'.'%_trlv
nomectl ='/scratchout/oper/io/oensMB09/pos/dataout/TQ0126L028/2015040100/'%_nomea%_labeli%_labelf%'M.grh.'%_trlv%'.ctl'
*say nomectl

_lonlat2ur="05038W2104S 04823W2137S 04823W2030S 04856W2211S 04715W2245S 04715W2030S 05004W2211S 04749W2245S 05111W2211S 04749W2104S 04930W2104S 04715W2318S"
_nlonlat2ur=12

_ndias=7
_ntimes=_ndias*24
say "abrindo o arquivo "nomectl
'open 'nomectl
'q file'
say result

say _time1' '_time2
'set time '_time1' '_time2

rec=read(_nomeb);nloc=sublin(rec,2)
rec=read(_nomec);nloc1=sublin(rec,2)
rec=read(_nomed);nloc2=sublin(rec,2)
_loc=0
while (_loc < nloc)
   _loc=_loc+1
say 'nloc ' nloc  _loc

   'clear';'set x '_loc
   'set time '_time1' '_time2
   routine=dimet()
   if (_faz=1)
      lixo=write('umrs_min'_labeli'.txt',_linha)
      lixo=write('umrs_min'_labeli'.txt','')
   endif  
endwhile
lixo=close('umrs_min'_labeli'.txt')

***********************************************
********* Grid History Maps Finalized *********
***********************************************
function dimet()
'set display color white';'clear'
'set vpage 0 8.5 10.2 11';'set grads off'
routine=title()
'set vpage 0 8.5 8.75 10.70';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=prec()
'set vpage 0 8.5 7.00 8.95';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=temp()
'set vpage 0 8.5 5.25 7.20';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=umrl()
if (_faz=1); routine=umrl_min(); endif 
'set vpage 0 8.5 3.50 5.45';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=wvel()
'set vpage 0 8.5 1.75 3.70';'set parea 0.7 8.0 0.3 1.6';'set grads off'
if (_ps=reduzida)
routine=psnm()
else
routine=pslc()
endif
'set vpage 0 8.5 0.0 1.95';'set parea 0.7 8.0 0.3 1.6';'set grads off'
routine=cbnv()
label=_labeli%_labelf
rec=read(_nomeb);lab=sublin(rec,2)
taga=png;tagb=png;tagc=png
say 'printim /'lab'.png png'
'printim /'lab'.png png' 
'!rm -f meteogram'
if (_loc=1)
'!rm -f puttag.'_labeli'.out'
'!rm -f deltag.'_labeli'.out'
endif
*AAF '!echo put '_state'/'lab''label'.'taga' >> puttag.'_labeli'.out'
return
************************************************
function title()
rec=read(_nomec);local=sublin(rec,2)
_state=estado(local)
rec=read(_nomed)
lonlat=sublin(rec,2);loi=substr(lonlat,1,3);lof=substr(lonlat,4,2)
lo=substr(lonlat,6,1);lai=substr(lonlat,7,2);laf=substr(lonlat,9,2);la=substr(lonlat,11,1)
lalo=loi%':'%lof%lo%'-'%lai%':'%laf%la
say ' '
say '   Plotando localizacao numero = '_loc'  Local: 'local
'set string 8 l 6';'set strsiz .13 .14'
'draw string 0.4 0.7 CPTEC:'
'draw string 1.4 0.7 'lalo
'draw string 3.4 0.7 'local
_faz=0

n=1
while (n<=_nlonlat2ur)
   latlonur=subwrd(_lonlat2ur,n)
   if (lonlat=latlonur)
      lixo=write('umrs_min'_labeli'.txt',lonlat' - 'local)
      _faz=1
      _linha=""
      n=9999
   endif
   n=n+1
endwhile

'query files'
lbin=sublin(result,3);bin=subwrd(lbin,2);utc=substr(bin,67,2)
'set t 5';'q dims'
tm = sublin(result,5);tim = subwrd(tm,6);tmm = substr(tim,7,9)
if (_ps!=reduzida)
'd topo'
_tpg=subwrd(result,4)
endif
'set strsiz .125 .13'
'draw string 0.4 0.5 'tmm' 'utc'Z (GMT)                           Vertical Grid Line: 'utc'Z'
'set time '_time1' '_time2
return

************************************************
function umrl()
'set gxout line';'set grads off';'set axlim 0 100';'set cmark 0';'set ylint 20';'set ccolor 4'
'd umrs'
'set string 6 l 5';'set strsiz .12 .13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Relative Humidity (%)'
return
************************************************
function umrl_min()
t=1;urmin=200; datamin=xx
while (t<=_ntimes)
'set t 't''
'q time'
data=subwrd(result,3)
'd umrs'
umid=subwrd(result,4)
if (umid<urmin)
urmin=umid
datamin=data
endif  
  
fimdia=math_fmod(t,24)
if (fimdia=0)
urmin=math_format('%5.1f',urmin)
_linha=_linha' 'datamin' 'urmin
urmin=200; datamin=xx
endif
t=t+1
endwhile
'set time '_time1' '_time2
return
************************************************
function cbnv()
'set gxout bar';'set bargap 0';'set barbase 0';'set vrange 0 100';'set ylint 20';'set grads off';'set ccolor 15'
'd cbnv'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw recf 0 0 8.5 0.1'
'draw string 1 1.75 Cloud Cover (%)'
return
************************************************
function snof()
'set gxout bar';'set bargap 0';'set barbase 0';'set vrange 0 10';'set ylint 2';'set grads off';'set ccolor 4'
'd neve'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Snow Fall (mm/h)'
return
************************************************
function prec()
'set gxout bar';'set bargap 0';'set barbase 0';'set vrange 0 5';'set ylint 1';'set grads off';'set ccolor 4'
'd prec'
'set gxout stat';'d neve'
lnv=sublin(result,8)
nv=subwrd(lnv,5)
'set gxout bar';'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
if (nv > 0.0001)
'set ccolor 3'
'd neve'
'draw string 1 1.75 Precipitation (blue) and Snow Fall (green) (mm/h)'
else
'draw string 1 1.75 Precipitation (mm/h)'
endif
'set string 8 l 6';'set strsiz .13 .14'
'draw string 7.1 1.75 '_trlv
return
************************************************
function psnm()
rotina=maxmin(psnm)
'set gxout line';'set vrange '_vmin' '_vmax'';'set cmark 0';'set ylint '_itvl'';'set ccolor 4';'set grads off'
'd psnm'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Mean Sea Level Pressure (hPa)'
return
************************************************
function pslc()
rotina=maxmin(pslc)
'set gxout line';'set vrange '_vmin' '_vmax'';'set cmark 0';'set ylint '_itvl'';'set ccolor 4';'set grads off'
'd pslc'
'set string 6 l 5';'set strsiz 0.12 0.13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Pressure (hPa)        Model Altitude: '_tpg' m'
return
************************************************
function wvel()
'set lev 1000 ';'set gxout vector';'set ylab off';'set grads off';'set arrowhead 0.075';'set z 0.5 1.5'
'd skip(uves,1,12);vves'
'set gxout line';'set grads off';'set z 1';'set ylab on';'set cmark 0';'set ylint 2';'set ccolor 4'
'd mag(uves,vves)'
'set string 6 l 5';'set strsiz .12 .13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Wind (m/s)'
return
************************************************
function temp()
rotina=maxmin(tems)
'set gxout line';'set vrange '_vmin' '_vmax'';'set grads off';'set ylint '_itvl'';'set ccolor 4';'set cmark 0'
'd tems'
'set string 6 l 5';'set strsiz .12 .13';'set line 0'
'draw recf 0.75 1.65 8.5 1.82'
'draw string 1 1.75 Surface Temperature (`aO`nC)'
return
************************************************
function maxmin(var)
'set t 1'

'd max('var',t=1,t='_ntimes',1)'

linha=sublin(result,2);imax=subwrd(linha,4)
'd min('var',t=1,t='_ntimes',1)'

linha=sublin(result,2);imin=subwrd(linha,4)
say linha
say imin

if(imin>0);imin=imin-1;else;imin=imin+1;endif
if(imax>0);imax=imax+1;else;imax=imax-1;endif
_vmax=math_nint(imax);_vmin=math_nint(imin)
_itvl=math_nint((imax-imin)/5)
'set time '_time1' '_time2
return
************************************************
function estado(local)
frase='AC AL AM AP BA CE DF ES GO MA MG MS MT PA PB PE PI PR RJ RN RO RR RS SC SE SP TO'
ne=1
while(ne<=27)
est.ne=subwrd(frase,ne)
ne=ne+1
endwhile

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
return state
***********************************************
