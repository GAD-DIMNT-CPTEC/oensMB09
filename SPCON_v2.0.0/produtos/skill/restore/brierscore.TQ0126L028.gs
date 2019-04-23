*tm=7
resol='TQ0126L028'
rec=read('nmdays.'resol'.n')
vart=sublin(rec,2)
tm=subwrd(vart,1)
rec=close('nmdays.'resol'.n')
'set t 1'
'set dfile 2'
'query dims'
time=sublin(result,5)
tim=subwrd(time,6)
lo=sublin(result,2)
lon1=subwrd(lo,6)
lon2=subwrd(lo,8)
la=sublin(result,3)
lat1=subwrd(la,6)
lat2=subwrd(la,8)
lv=sublin(result,4)
levl=subwrd(lv,6)
filename='brierscoreg.'tim'.'resol'.pr'
'set dfile 1'
*
loc=' Area:'
if (lat1=20 & lat2=80)
if (lon1=0 & lon2=360)
loc='Northern Hemisphere:'
endif
endif
if (lat1=-80 & lat2=-20)
if (lon1=0 & lon2=360)
loc='Southern Hemisphere:'
endif
endif
if (lat1=-20 & lat2=20)
if (lon1=0 & lon2=360)
loc='Tropical Region:'
endif
endif
if (lat1=-60 & lat2=15)
if (lon1=-101.25 & lon2=-11.25)
loc='South America:'
endif
endif
if (lon1=0 & lon2=360)
im=subwrd(lo,13)-1
lon2=lon2-360/im
endif
*
ll=loc' Lon: 'lon1' to 'lon2'  Lat: 'lat1' to 'lat2'  Level: 'levl' hPa'
lg='BRIER SCORE    FCT DAYS  INITIAL DATE'
*
say ' '
say ll
say lgt
say lg
rec=write(filename,' ',append)
rec=write(filename,ll,append)
rec=write(filename,lg,append)
*
rec=read('nmembers.'resol'.n')
members=sublin(rec,2)
rec=close('nmembers.'resol'.n')
rec=read('limiar.'resol'.n')
limiar=sublin(rec,2)
rec=close('limiar.'resol'.n')
*
*calcula 0 ou 1 a partir da analise
say 'limiar='limiar
*
*AMM 'define mod=abs(zgeo.2(t=1)-z.1(t=1))'
'define z1=lterp(z.1(t=1),zgeo.2(t=1))'
'define mod=abs(zgeo.2(t=1)-z1)'
'define verif=const(const(maskout(mod,mod-'limiar'),1),0.0,-u)'
'undefine mod'
*
*calcula frequencia prevista 
*
t=1
while (t <= tm)
'define prob't'=0'
t=t+1
endwhile
*
A0=2
n=3
while (n <= members+2)
t=1
while (t <= tm)
'set t 1'
df=A0+t
*
'define med=aave(zgeo.'df'(t=1),lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'display med'
med.t=subwrd(result,4)
'undefine med'
*AMM if (med.t != 'Operand')
*AMM 'define mod=abs(zgeo.'df'(t=1)-z.1(t=1))'
*AMM 'define freq=const(const(maskout(mod,mod-'limiar'),1),0.0,-u)'
*AMM 'define prob't'=prob't'+freq'
*AMM 'undefine mod'
*AMM 'undefine freq'
*AMM else
*AMM 'define prob't'=prob't'+1e20'
*AMM endif
if (med.t > 1e+15)
'define prob't'=prob't'+1e20'
else
*AMM 'define mod=abs(zgeo.'df'(t=1)-z.1(t=1))'
'define z1=lterp(z.1(t=1),zgeo.'df'(t=1))'
'define mod=abs(zgeo.'df'(t=1)-z1)'
'define freq=const(const(maskout(mod,mod-'limiar'),1),0.0,-u)'
'define prob't'=prob't'+freq'
'undefine mod'
'undefine freq'
endif
*
t=t+1
endwhile
*
A0=A0+tm
n=n+1
endwhile
*
*calcula a probabilidade prevista e o brier score
*
t=1
while (t <= tm)
*AMM if (med.t != 'Operand')
*AMM 'define prob't'=(prob't')/'members''
*AMM 'define bs't'=aave(pow(prob't'-verif,2),lon='lon1',lon='lon2',lat='lat1',lat='lat2'))'
*AMM else
*AMM 'define bs't'=1e20'
*AMM endif
if (med.t > 1e+15)
'define bs't'=1e20'
else
'define prob't'=(prob't')/'members''
'define bs't'=aave(pow(prob't'-verif,2),lon='lon1',lon='lon2',lat='lat1',lat='lat2'))'
endif
'undefine prob't''
t=t+1
endwhile
'undefine verif'
*
t=1
while (t <= tm)
ts=t-tm
'set t 'ts
'query dims'
tt=sublin(result,5)
tit=subwrd(tt,6)
*
'display bs't''
bsc=subwrd(result,4)
if (bsc > 1e15)
bsc='-99.99'
endif
bsc1=substr(bsc,1,6)
*
sk=' 'bsc1'            'tm-t+1'     'tit
say sk
rec=write(filename,sk,append)
'undefine bs't''
t=t+1
endwhile
*


