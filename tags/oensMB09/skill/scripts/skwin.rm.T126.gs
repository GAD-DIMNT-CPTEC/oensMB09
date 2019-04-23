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
lv=sublin(result,4)
levl=subwrd(lv,6)
lo=sublin(result,2)
lon1=subwrd(lo,6)
lon2=subwrd(lo,8)
la=sublin(result,3)
lat1=subwrd(la,6)
lat2=subwrd(la,8)
filename='skwin.rmv.'tim'.'resol'.pr'
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
lgt=' Zonal Wind (m/s) Merid Wind (m/s) Magnitude (m/s)'
lg=' SKL(%) BIAS      SKL(%) BIAS      RMS       FCT DAYS INITIAL DATE'
say ' '
say ll
say lgt
say lg
rec=write(filename,' ',append)
rec=write(filename,ll,append)
*
'define u1=lterp(uvel.1(t=1),uvel.2(t=1))'
'define v1=lterp(vvel.1(t=1),vvel.2(t=1))'
'define uy=uvel.2(t=1)-u1'
'define vy=vvel.2(t=1)-v1'
*
t=1
while (t <= tm)
'define ensmu't'=0'
'define ensmv't'=0'
t=t+1
endwhile
*
rec=read(nmembers.resol.n)
members=1
*
np=3
A0=1
n=2
while (n <= np)
*
*AAF if (n <= np)
*AAF NORP='P'
*AAF filen=n-2
*AAF endif
*AAF if (n > np & n < (members+2))
*AAF NORP='N'
*AAF filen=n-np
*AAF endif
if (n = 2)
NORP='Control Forecast'
filen=''
else
NORP='RMV Forecast'
filen=''
endif
*
if (n = 3 )
rec=write(filename,' ',append)
rec=write(filename,lgt'   MEMBER: 'filen''NORP,append)
rec=write(filename,lg,append)
endif
*
t=1
while (t <= tm)
ts=t-tm
'set t 'ts
'query dims'
tt=sublin(result,5)
tit=subwrd(tt,6)
'set t 1'
df=A0+t
*
'define ux=uvel.'df'(t=1)-u1'
'define vx=vvel.'df'(t=1)-v1'
'define ucor=100*scorr(ux,uy,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define vcor=100*scorr(vx,vy,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'display ucor'
uco=subwrd(result,4)
*AMM if (uco != 'Operand')
*AMM 'define ensmu't'=ensmu't'+uvel.'df'(t=1)'
*AMM 'define ensmv't'=ensmv't'+vvel.'df'(t=1)'
*AMM else
*AMM 'define ensmu't'=ensmu't'+1e+20'
*AMM 'define ensmv't'=ensmv't'+1e+20'
*AMM endif
if (uco > 1e+15)
'define ensmu't'=ensmu't'+1e+20'
'define ensmv't'=ensmv't'+1e+20'
else
'define ensmu't'=ensmu't'+uvel.'df'(t=1)'
'define ensmv't'=ensmv't'+vvel.'df'(t=1)'
endif
'undefine ux'
'undefine vx'
'define ue=uvel.'df'(t=1)-uvel.2(t=1))'
'define ve=vvel.'df'(t=1)-vvel.2(t=1))'
'define ubias=aave(ue,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define vbias=aave(ve,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define rmw=aave(mag(ue,ve),lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine ue'
'undefine ve'
*
'display ubias'
ubia=subwrd(result,4)
'display vcor'
vco=subwrd(result,4)
'display vbias'
vbia=subwrd(result,4)
'display rmw'
rm=subwrd(result,4)
'undefine ucor'
'undefine ubias'
'undefine vcor'
'undefine vbias'
'undefine rmw'
*
*AMM if (uco = 'Operand')
if (uco > 1e+15)
uco='-99.9'
ubia='-99.9'
vco='-99.9'
vbia='-99.9'
rm='-99.9'
endif
*
ucr=substr(uco,1,5)
ubs=substr(ubia,1,5)
vcr=substr(vco,1,5)
vbs=substr(vbia,1,5)
rms=substr(rm,1,5)
*
sk=' 'ucr'  'ubs'     'vcr'  'vbs'     'rms'        'tm-t+1'     'tit
say sk
if (n = 3 )
rec=write(filename,sk,append)
endif
*
t=t+1
endwhile
*
A0=A0+tm
n=n+1
endwhile
*
'undefine u1'
'undefine v1'
'undefine u3'
'undefine v3'
'undefine uy'
'undefine vy'
*
