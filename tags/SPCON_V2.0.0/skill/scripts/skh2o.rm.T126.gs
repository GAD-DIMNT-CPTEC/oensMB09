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
filename='skh2o.rmv.'tim'.'resol'.pr'
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
lgt=' Precipit. Water (mm)    Specific Humidity (g/Kg)'
lg=' SKL(%) RMS    BIAS      SKL(%) RMS    BIAS      FCT DAYS INITIAL DATE'
say ' '
say ll
say lgt
say lg
rec=write(filename,' ',append)
rec=write(filename,ll,append)
*
'define h1=lterp(agpl.1(t=1),agpl.2(t=1))'
'define h2=agpl.2(t=1)'
'define hy=h2-h1'
*
'define e1=lterp(umes.1(t=1),umes.2(t=1))'
'define e2=umes.2(t=1)'
'define ey=e2-e1'
*
t=1
while (t <= tm)
'define ensmh't'=0'
'define ensme't'=0'
t=t+1
endwhile
*
rec=read(nmembers.resol.n)
members=sublin(rec,2)
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
'set t '1
df=A0+t
*
*'define h3=agpl.'df'(t=1)'
'define hx=agpl.'df'(t=1)-h1'
'define hz=agpl.'df'(t=1)-h2'
'define hcor=100*scorr(hx,hy,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine hx'
'define hxy=aave(hz*hz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define hrms=sqrt(hxy)'
'define hbias=aave(hz,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'display hcor'
hco=subwrd(result,4)
*AMM if (hco != 'Operand')
*AMM 'define ensmh't'=ensmh't'+agpl.'df'(t=1)'
*AMM else
*AMM 'define ensmh't'=ensmh't'+1e20'
*AMM endif
if (hco > 1e+15)
'define ensmh't'=ensmh't'+1e20'
else
'define ensmh't'=ensmh't'+agpl.'df'(t=1)'
endif
'undefine hz'
'undefine hxy'
*
'define ex=umes.'df'(t=1)-e1'
'define ez=umes.'df'(t=1)-e2'
'define ecor=100*scorr(ex,ey,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'undefine ex'
'define exy=aave(ez*ez,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
'define erms=1000.0*sqrt(exy)'
'define ebias=1000.0*aave(ez,lon='lon1',lon='lon2',lat='lat1',lat='lat2')'
*AMM if (hco != 'Operand')
*AMM 'define ensme't'=ensme't'+umes.'df'(t=1)'
*AMM else
*AMM 'define ensme't'=ensme't'+1e20'
*AMM endif
if (hco > 1e+15)
'define ensme't'=ensme't'+1e20'
else
'define ensme't'=ensme't'+umes.'df'(t=1)'
endif
'undefine ez'
'undefine exy'
*
'display hrms'
hrm=subwrd(result,4)
'display hbias'
hbia=subwrd(result,4)
'undefine hcor'
'undefine hrms'
'undefine hbias'
*
'display ecor'
eco=subwrd(result,4)
'display erms'
erm=subwrd(result,4)
'display ebias'
ebia=subwrd(result,4)
'undefine ecor'
'undefine erms'
'undefine ebias'
*
*AMM if (hco = 'Operand')
if (hco > 1e+15)
hco='-99.9'
hrm='-99.9'
hbia='-99.9'
eco='-99.9'
erm='-99.9'
ebia='-99.9'
endif
hcr=substr(hco,1,5)
hrs=substr(hrm,1,5)
hbs=substr(hbia,1,5)
ecr=substr(eco,1,5)
ers=substr(erm,1,5)
ebs=substr(ebia,1,5)
*
sk=' 'hcr'  'hrs'  'hbs'     'ecr'  'ers'  'ebs'        'tm-t+1'     'tit
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
'undefine h1'
'undefine h2'
'undefine hy'
'undefine e1'
'undefine e2'
'undefine ey'
*
