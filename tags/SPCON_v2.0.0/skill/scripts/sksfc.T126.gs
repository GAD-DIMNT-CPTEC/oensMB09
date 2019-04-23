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
filename='sksfc.'tim'.'resol'.pr'
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
lgt=' Sea Level Pres. (hPa)   Virtual Temperature (K)'
lg=' SKL(%) RMS    BIAS      SKL(%) RMS    BIAS      FCT DAYS INITIAL DATE'
*say ' '
*say ll
*say lgt
*say lg
rec=write(filename,' ',append)
rec=write(filename,ll,append)
*
'define z1a=lterp(zgeo.1(t=1,lev=500),zgeo.2(t=1))'
'define z1b=lterp(zgeo.1(t=1,lev=1000),zgeo.2(t=1))'
'define z1d=1.5422885*(z1a-z1b)'
'define z1e=z1b/(z1d)'
'define p1s=1000*exp(z1e)'
'undefine z1a'
'undefine z1b'
'undefine z1d'
'undefine z1e'
'define p2s=psnm.2(t=1)'
'define py=p2s-p1s'
*
'define ta=lterp(temp.1(t=1),temp.2(t=1))'
if (levl > 299)
'define sh=lterp(umes.1(t=1),umes.2(t=1))'
'define t1=ta*(1+0.608*sh)'
'undefine sh'
else
'define t1=ta'
endif
'undefine ta'
'define t2=temp.2(t=1)*(1+0.608*umes.2(t=1))'
'define ty=t2-t1'
*
t=1
while (t <= tm)
'define ensmp't'=0'
'define ensmt't'=0'
t=t+1
endwhile
*
rec=read(nmembers.resol.n)
members=sublin(rec,2)
*
np=2+(members-1)/2
A0=2
n=3
while (n <= members+2)
*
if (n <= np)
NORP='P'
filen=n-2
endif
if (n > np & n < (members+2))
NORP='N'
filen=n-np
endif
if (n = (members+2))
NORP='Control Forecast'
filen=''
endif
*
rec=write(filename,' ',append)
rec=write(filename,lgt'   MEMBER: 'filen''NORP,append)
rec=write(filename,lg,append)
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
'define p3s=psnm.'df'(t=1)'
'define px=p3s-p1s'
'define pz=p3s-p2s'
'define pcor=100*scorr(px,py,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine px'
'define pxy=aave(pz*pz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define prms=sqrt(pxy)'
'define pbias=aave(pz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'display pcor'
pco=subwrd(result,4)
*AMM if (pco != 'Operand')
*AMM 'define ensmp't'=ensmp't'+p3s'
*AMM else
*AMM 'define ensmp't'=ensmp't'+1e20'
*AMM endif
if (pco > 1e+15)
'define ensmp't'=ensmp't'+1e20'
else
'define ensmp't'=ensmp't'+p3s'
endif
'undefine p3s'
'undefine pz'
'undefine pxy'
*
'define t3=temp.'df'(t=1)*(1+0.608*umes.'df'(t=1))'
'define tx=t3-t1'
'define tz=t3-t2'
'define tcor=100*scorr(tx,ty,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine tx'
'define txy=aave(tz*tz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define trms=sqrt(txy)'
'define tbias=aave(tz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
*AMM if (pco != 'Operand')
*AMM 'define ensmt't'=ensmt't'+t3'
*AMM else
*AMM 'define ensmt't'=ensmt't'+1e20'
*AMM endif
if (pco > 1e+15)
'define ensmt't'=ensmt't'+1e20'
else
'define ensmt't'=ensmt't'+t3'
endif
'undefine t3'
'undefine tz'
'undefine txy'
*
'display prms'
prm=subwrd(result,4)
'display pbias'
pbia=subwrd(result,4)
'undefine pcor'
'undefine prms'
'undefine pbias'
*
'display tcor'
tco=subwrd(result,4)
'display trms'
trm=subwrd(result,4)
'display tbias'
tbia=subwrd(result,4)
'undefine tcor'
'undefine trms'
'undefine tbias'
*
*AMM if (pco = 'Operand')
if (pco > 1e+15)
pco='-99.9'
prm='-99.9'
pbia='-99.9'
tco='-99.9'
trm='-99.9'
tbia='-99.9'
endif
pcr=substr(pco,1,5)
prs=substr(prm,1,5)
pbs=substr(pbia,1,5)
tcr=substr(tco,1,5)
trs=substr(trm,1,5)
tbs=substr(tbia,1,5)
*
sk=' 'pcr'  'prs'  'pbs'     'tcr'  'trs'  'tbs'        'tm-t+1'     'tit
say sk
rec=write(filename,sk,append)
*
t=t+1
endwhile
*
A0=A0+tm
n=n+1
endwhile
*
rec=write(filename,' ',append)
rec=write(filename,lgt'   MEMBER: Ensemble Mean',append)
rec=write(filename,lg,append)
*
t=1
while (t <= tm)
ts=t-tm
'set t 'ts
'query dims'
tt=sublin(result,5)
tit=subwrd(tt,6)
'set t 1'
*
'define p3s=(ensmp't')/'members''
'define px=p3s-p1s'
'define pz=p3s-p2s'
'define pcor=100*scorr(px,py,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine px'
'define pxy=aave(pz*pz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define prms=sqrt(pxy)'
'define pbias=aave(pz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine p3s'
'undefine pz'
'undefine pxy'
*
'define t3=(ensmt't')/'members''
'define tx=t3-t1'
'define tz=t3-t2'
'define tcor=100*scorr(tx,ty,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine tx'
'define txy=aave(tz*tz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define trms=sqrt(txy)'
'define tbias=aave(tz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine t3'
'undefine tz'
'undefine txy'
*
'display pcor'
pco=subwrd(result,4)
'display prms'
prm=subwrd(result,4)
'display pbias'
pbia=subwrd(result,4)
'undefine pcor'
'undefine prms'
'undefine pbias'
*
'display tcor'
tco=subwrd(result,4)
'display trms'
trm=subwrd(result,4)
'display tbias'
tbia=subwrd(result,4)
'undefine tcor'
'undefine trms'
'undefine tbias'
*
if (pbia > 1e+15)
pco='-99.9'
prm='-99.9'
pbia='-99.9'
tco='-99.9'
trm='-99.9'
tbia='-99.9'
endif
pcr=substr(pco,1,5)
prs=substr(prm,1,5)
pbs=substr(pbia,1,5)
tcr=substr(tco,1,5)
trs=substr(trm,1,5)
tbs=substr(tbia,1,5)
*
sk=' 'pcr'  'prs'  'pbs'     'tcr'  'trs'  'tbs'        'tm-t+1'     'tit
say sk
rec=write(filename,sk,append)
*
'undefine ensmp't''
'undefine ensmt't''
t=t+1
endwhile
*
'undefine p1s'
'undefine p2s'
'undefine py'
'undefine t1'
'undefine t2'
'undefine ty'
*
