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
filename='spreadw.'tim'.'resol'.pr'
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
lgt=' Zonal Wind     Meridional Wind'
lg=' SPREAD(m/s)      SPREAD(m/s)      FCT DAYS INITIAL DATE'
*
say ' '
say ll
say lgt
say lg
rec=write(filename,' ',append)
rec=write(filename,ll,append)
rec=write(filename,lgt,append)
rec=write(filename,lg,append)
*
t=1
while (t <= tm)
'define ensmu't'=0'
'define ensmv't'=0'
'define rmsmu't'=0'
'define rmsmv't'=0'
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
*
'define med=aave(uvel.'df'(t=1),lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'display med'
med.t=subwrd(result,4)
'undefine med'
*AMM if (med.t != 'Operand')
*AMM 'define ensmu't'=ensmu't'+uvel.'df'(t=1)'
*AMM 'define ensmv't'=ensmv't'+vvel.'df'(t=1)'
*AMM else
*AMM 'define ensmu't'=ensmu't'+1e20'
*AMM 'define ensmv't'=ensmv't'+1e20'
*AMM endif
if (med.t > 1e+15)
'define ensmu't'=ensmu't'+1e20'
'define ensmv't'=ensmv't'+1e20'
else
'define ensmu't'=ensmu't'+uvel.'df'(t=1)'
'define ensmv't'=ensmv't'+vvel.'df'(t=1)'
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
'define ensmu't'=(ensmu't')/'members''
'define ensmv't'=(ensmv't')/'members''
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
*AMM if (med.t != 'Operand')
*AMM 'define rmsu=pow(uvel.'df'(t=1)-ensmu't',2)'
*AMM 'define rmsmu't'=rmsmu't'+rmsu'
*AMM 'undefine rmsu'
*AMM 'define rmsv=pow(vvel.'df'(t=1)-ensmv't',2)'
*AMM 'define rmsmv't'=rmsmv't'+rmsv'
*AMM 'undefine rmsv'
*AMM else
*AMM 'define rmsmu't'=rmsmu't'+1e20'
*AMM 'define rmsmv't'=rmsmv't'+1e20'
*AMM endif
if (med.t > 1e+15)
'define rmsmu't'=rmsmu't'+1e20'
'define rmsmv't'=rmsmv't'+1e20'
else
'define rmsu=pow(uvel.'df'(t=1)-ensmu't',2)'
'define rmsmu't'=rmsmu't'+rmsu'
'undefine rmsu'
'define rmsv=pow(vvel.'df'(t=1)-ensmv't',2)'
'define rmsmv't'=rmsmv't'+rmsv'
'undefine rmsv'
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
'define sqrtrmsmu=sqrt(rmsmu't'/'members')'
'define sqrtrmsmv=sqrt(rmsmv't'/'members')'
'define rmsmu't'=aave(sqrtrmsmu,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define rmsmv't'=aave(sqrtrmsmv,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine sqrtrmsmu'
'undefine sqrtrmsmv'
*
'display rmsmu't''
zrmu=subwrd(result,4)
'undefine rmsmu't''
'display rmsmv't''
zrmv=subwrd(result,4)
'undefine rmsmv't''
if (zrmu > 1e15)
zrmu='-99.9'
zrmv='-99.9'
endif
zcru=substr(zrmu,1,5)
zcrv=substr(zrmv,1,5)
*
sk=' 'zcru'            'zcrv'               'tm-t+1'     'tit
say sk
rec=write(filename,sk,append)
'undefine ensmu't''
'undefine ensmv't''
'undefine rmsmu't''
'undefine rmsmv't''
t=t+1
endwhile
*


