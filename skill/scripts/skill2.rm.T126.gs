say 'Enter yy mm dd hh'
pull ctime
yy=subwrd(ctime,1)
mm=subwrd(ctime,2)
dd=subwrd(ctime,3)
hh=subwrd(ctime,4)
mb=subwrd(ctime,5)
nd=subwrd(ctime,6)
resol=subwrd(ctime,7)
*say 'mb='mb
*say 'nd='nd
*
rec=read(fileinput.resol)
filei=sublin(rec,2)
rec=read(fileinput.resol)
fnmanl=sublin(rec,2)
call=0
while (call < 1)
call=2
if (filei = unavl)
say ' Analysis: 'filei': 'fnmanl
break
else
say ' Analysis: 'filei': 'fnmanl
endif
*
*   Create ctls
'!doctl.ksh 'yy''mm''dd''hh' AVN 'nd
*
*   Open ctls
'run openctl.'resol'.gs'
*
'set t 1'
'query dims'
time=sublin(result,5)
tim=subwrd(time,6)
*
'!rm -f skillt.'tim'.'resol'.pr'
'!rm -f skills.'tim'.'resol'.pr sksfc.'tim'.'resol'.pr'
'!rm -f skillh.'tim'.'resol'.pr skh2o.'tim'.'resol'.pr'
'!rm -f skillp.'tim'.'resol'.pr skpst.'tim'.'resol'.pr'
'!rm -f spreadg.'tim'.'resol'.pr spreadg.'tim'.'resol'.pr'
'!rm -f spreadw.'tim'.'resol'.pr spreadw.'tim'.'resol'.pr'
*
ttl=' Skill of Ensemble Operational CPTEC/COLA T126L28 Spectral Global Model'
ttm='   using as Control Initial Condition NMC T126L28 Spectral Analysis'
ttn=' perturbation method: EOF based perturbation (Zhang and Krishnamurti) '
*say ' '
*say ttl
*say ttm
filenamet='skillt.'tim'.'resol'.pr'
rec=write(filenamet,ttl,append)
rec=write(filenamet,ttm,append)
rec=close(filenamet)
*
tp=' Sea Level Pressure and Tv Anomalies, Bias and RMSE for Total Field'
tv=' ------------------------------------------------------------------'
tc=' Analysis Date: 'tim
*say ' '
*say tp
*say ' '
*say tc
filenames='skills.'tim'.'resol'.pr'
rec=write(filenames,' ',append)
rec=write(filenames,tp,append)
rec=write(filenames,tv,append)
rec=write(filenames,' ',append)
rec=write(filenames,tc,append)
rec=close(filenames)
*
'set lon 0 360'
'set lat 20 80'
'set lev 1000'
'run sksfc.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 1000'
'run sksfc.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 1000'
'run sksfc.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 1000'
'run sksfc.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 1000'
'run sksfc.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 1000'
'run sksfc.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 1000'
'run sksfc.'resol'.gs'
*
th=' Precip Water and Spec Hum Anomalies, Bias and RMSE for Total Field'
tx=' ------------------------------------------------------------------'
td=' Analysis Date: 'tim
*say ' '
*say th
*say ' '
*say td
filenameh='skillh.'tim'.'resol'.pr'
rec=write(filenameh,' ',append)
rec=write(filenameh,th,append)
rec=write(filenameh,tx,append)
rec=write(filenameh,' ',append)
rec=write(filenameh,td,append)
rec=close(filenameh)
*
'set lon 0 360'
'set lat 20 80'
'set lev 925'
'run skh2o.'resol'.gs'
*
'set lon 0 360'
'set lat -80 -20'
'set lev 925'
'run skh2o.'resol'.gs'
*
'set lon 0 360'
'set lat -20 20'
'set lev 925'
'run skh2o.'resol'.gs'
*
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 925'
'run skh2o.'resol'.gs'
*
'!rm -f skgwu.'tim'.'resol'.pr'
filenameu='skgwu.'tim'.'resol'.pr'
rec=write(filenameu,' ',append)
rec=write(filenameu,' -99.9 means that data are unavailable',append)
rec=close(filenameu)
*
ty='       Geop Height Spread - (In relation to Ensemble Mean)     '
tx=' ------------------------------------------------------------------'
te=' Analysis Date: 'tim
*say ' '
*say th
*say ' '
*say te
filenamey='spreadg.'tim'.'resol'.pr'
rec=write(filenamey,' ',append)
rec=write(filenamey,ty,append)
rec=write(filenamey,tx,append)
rec=write(filenamey,' ',append)
rec=write(filenamey,te,append)
rec=close(filenamey)
*
'set lon 0 360'
'set lat 20 80'
'set lev 500'
'run spreadg.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 500'
'run spreadg.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 500'
'run spreadg.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 500'
'run spreadg.'resol'.gs'
'set lon 0 360'
'set lat 20 80'
'set lev 850'
'run spreadg.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 850'
'run spreadg.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 850'
'run spreadg.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 850'
'run spreadg.'resol'.gs'
*
ty=' Zonal and Meridional Wind Spread (In relation to Ensemble Mean)'
tx=' ------------------------------------------------------------------'
te=' Analysis Date: 'tim
*say ' '
*say th
*say ' '
*say te
filenamey='spreadw.'tim'.'resol'.pr'
rec=write(filenamey,' ',append)
rec=write(filenamey,ty,append)
rec=write(filenamey,tx,append)
rec=write(filenamey,' ',append)
rec=write(filenamey,te,append)
rec=close(filenamey)
*
'set lon 0 360'
'set lat 20 80'
'set lev 850'
'run spreadw.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 850'
'run spreadw.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 850'
'run spreadw.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 850'
'run spreadw.'resol'.gs'
*
dirskl='/gfs/dk19/io_dop/users/alex/oenspro/skill'
'!rm -f skills.'tim'.ens.m02'
'!cat skillt.'tim'.'resol'.pr skills.'tim'.'resol'.pr sksfc.'tim'.'resol'.pr skgwu.'tim'.'resol'.pr > skills.'tim'.ens.m02'
'!mv skills.'tim'.ens.m02 'dirskl''
'!rm -f skillh.'tim'.ens.m02'
'!cat skillt.'tim'.'resol'.pr skillh.'tim'.'resol'.pr skh2o.'tim'.'resol'.pr skgwu.'tim'.'resol'.pr > skillh.'tim'.ens.m02'
'!mv skillh.'tim'.ens.m02 'dirskl''
'!rm -f spreadg.'tim'.ens.m02'
'!cat skillt.'tim'.'resol'.pr spreadg.'tim'.'resol'.pr skgwu.'tim'.'resol'.pr > spreadg.'tim'.ens.m02'
'!mv spreadg.'tim'.ens.m02 'dirskl''
'!rm -f spreadw.'tim'.ens.m02'
'!cat skillt.'tim'.'resol'.pr spreadw.'tim'.'resol'.pr skgwu.'tim'.'resol'.pr > spreadw.'tim'.ens.m02'
'!mv spreadw.'tim'.ens.m02 'dirskl''
'!rm -f skillt.'tim'.'resol'.pr'
'!rm -f skills.'tim'.'resol'.pr sksfc.'tim'.'resol'.pr'
'!rm -f skillh.'tim'.'resol'.pr skh2o.'tim'.'resol'.pr'
'!rm -f spreadg.'tim'.'resol'.pr spreadw.'tim'.'resol'.pr skgwu.'tim'.'resol'.pr'
*
endwhile
*
'!rm -f lterp.in lterp.out'
*
