*tm=7
resol='T126'
rec=read(nmdays.resol.n)
vart=sublin(rec,2)
tm=subwrd(vart,1)
rec=close(nmdays.resol.n)
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
filename='spreadg.'tim'.'resol'.pr'
'set dfile 1'
*
loc=' Area:'
if (lat1=20 & lat2=80)
if (lon1=0 & lon2=360)
loc=' Northern Hemisphere:'
endif
endif
if (lat1=-80 & lat2=-20)
if (lon1=0 & lon2=360)
loc=' Southern Hemisphere:'
endif
endif
if (lat1=-20 & lat2=20)
if (lon1=0 & lon2=360)
loc=' Tropical Region:'
endif
endif
if (lat1=-60 & lat2=15)
if (lon1=-101.25 & lon2=-11.25)
loc=' South America:'
endif
endif
if (lon1=0 & lon2=360)
im=subwrd(lo,13)-1
lon2=lon2-360/im
endif
*
ll=loc' Lon: 'lon1' to 'lon2'  Lat: 'lat1' to 'lat2'  Level: 'levl' hPa'
lg=' SPREAD (m)    FCT DAYS INITIAL DATE'
*
say ' '
say ll
say lgt
say lg
rec=write(filename,' ',append)
rec=write(filename,ll,append)
rec=write(filename,lg,append)
*
t=1
while (t <= tm)
'define ensmz't'=0'
'define rmsmz't'=0'
t=t+1
endwhile
*
rec=read(nmembers.resol.n)
members=sublin(rec,2)
*
A0=2
n=3
while (n <= members+2)
t=1
while (t <= tm)
'set t 1'
df=A0+t
*say 'df='df
*
'define med=aave(zgeo.'df'(t=1),lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'display med'
med.t=subwrd(result,4)
'undefine med'
*AMM if (med.t != 'Operand')
*AMM 'define ensmz't'=ensmz't'+zgeo.'df'(t=1)'
*AMM else
*AMM 'define ensmz't'=ensmz't'+1e20'
*AMM endif
if (med.t > 1e+15)
'define ensmz't'=ensmz't'+1e20'
else
'define ensmz't'=ensmz't'+zgeo.'df'(t=1)'
endif
*
t=t+1
endwhile
*
A0=A0+tm
n=n+1
endwhile
*
t=1
while (t <= tm)
'define ensmz't'=(ensmz't')/'members''
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
*say 'df= 'df
say 'med.'t'= 'med.t
*
*AMM if (med.t != 'Operand')
*AMM 'define rms=pow(zgeo.'df'(t=1)-ensmz't',2)'
*AMM 'define rmsmz't'=rmsmz't'+rms'
*AMM 'undefine rms'
*AMM else
*AMM 'define rmsmz't'=rmsmz't'+1e20'
*AMM endif
if (med.t > 1e+15)
'define rmsmz't'=rmsmz't'+1e20'
else
'define rms=pow(zgeo.'df'(t=1)-ensmz't',2)'
'define rmsmz't'=rmsmz't'+rms'
'undefine rms'
endif
*
t=t+1
endwhile
*
A0=A0+tm
n=n+1
endwhile
*
t=1
while (t <= tm)
ts=t-tm
'set t 'ts
'query dims'
tt=sublin(result,5)
tit=subwrd(tt,6)
'define sqrtrms=sqrt(rmsmz't'/'members')'
'define rmsmz't'=aave(sqrtrms,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine sqrtrms'
*
'display rmsmz't''
zrm=subwrd(result,4)
'undefine rmsmz't''
if (zrm > 1e15)
zrm='-99.9'
endif
zcr=substr(zrm,1,5)
*
sk=' 'zcr'            'tm-t+1'     'tit
say sk
rec=write(filename,sk,append)
'undefine ensmz't''
'undefine rmsmz't''
t=t+1
endwhile
*


