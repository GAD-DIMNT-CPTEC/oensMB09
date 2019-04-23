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
say 'dayinty.'resol'.ksh 'yy' 'mm' 'dd' 'hh' T126L28 GPOSAVN /bangu/samfs/modoper/tempo/global/oens/nmc/T126L28/GPOS/2010/07/12/ /gfs/home3/modoper/tempo/global/oens/SKILL/CLIMA/'
'!dayinty.'resol'.ksh 'yy' 'mm' 'dd' 'hh' T126L28 GPOSAVN /bangu/samfs/modoper/tempo/global/oens/nmc/T126L28/GPOS/2010/07/12/ /gfs/home3/modoper/tempo/global/oens/SKILL/CLIMA/'
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
'!doctl.ksh 'yy''mm''dd''hh' 'AVN' 'nd
say '!doctl.ksh 'yy''mm''dd''hh' 'AVN' 'nd
*
*   Open ctls
'run openctl.rm.'resol'.gs'
*
*
'q files'
say result
*'quit'
'set t 1'
'query dims'
time=sublin(result,5)
tim=subwrd(time,6)
*
'!rm -f skillt.'tim'.'resol'.pr'
'!rm -f skillg.rmv.'tim'.'resol'.pr skgeo.'tim'.'resol'.pr'
'!rm -f skillw.rmv.'tim'.'resol'.pr skwin.'tim'.'resol'.pr'
'!rm -f skills.rmv.'tim'.'resol'.pr sksfc.'tim'.'resol'.pr'
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
tg=' Geopotential Height and Tv Anomalies, Bias and RMSE for Total Field'
tr=' -------------------------------------------------------------------'
ta=' Analysis Date: 'tim
*say ' '
*say tg
*say ' '
*say ta
filenameg='skillg.rmv.'tim'.'resol'.pr'
rec=write(filenameg,' ',append)
rec=write(filenameg,tg,append)
rec=write(filenameg,tr,append)
rec=write(filenameg,' ',append)
rec=write(filenameg,ta,append)
rec=close(filenameg)
*
'set lon 0 360'
'set lat 20 80'
'set lev 850'
'run skgeo.rm.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 850'
'run skgeo.rm.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 850'
'run skgeo.rm.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 850'
'run skgeo.rm.'resol'.gs'
*
'set lon 0 360'
'set lat 20 80'
'set lev 500'
'run skgeo.rm.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 500'
'run skgeo.rm.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 500'
'run skgeo.rm.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 500'
'run skgeo.rm.'resol'.gs'
*
'set lon 0 360'
'set lat 20 80'
'set lev 250'
'run skgeo.rm.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 250'
'run skgeo.rm.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 250'
'run skgeo.rm.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 250'
'run skgeo.rm.'resol'.gs'
*
*say ' '
say ' -99.9 for all values means that data are unavailable'
*
tw=' Zonal and Meridional Wind Anomalies, Bias and RMSE Total Wind'
ts=' -------------------------------------------------------------'
tb=' Analysis Date: 'tim
*say ' '
*say tw
*say ' '
*say tb
filenamew='skillw.rmv.'tim'.'resol'.pr'
rec=write(filenamew,' ',append)
rec=write(filenamew,tw,append)
rec=write(filenamew,ts,append)
rec=write(filenamew,' ',append)
rec=write(filenamew,tb,append)
rec=close(filenamew)
*
'set lon 0 360'
'set lat 20 80'
'set lev 850'
'run skwin.rm.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 850'
'run skwin.rm.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 850'
'run skwin.rm.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 850'
'run skwin.rm.'resol'.gs'
*
'set lon 0 360'
'set lat 20 80'
'set lev 500'
'run skwin.rm.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 500'
'run skwin.rm.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 500'
'run skwin.rm.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 500'
'run skwin.rm.'resol'.gs'
*
'set lon 0 360'
'set lat 20 80'
'set lev 250'
'run skwin.rm.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 250'
'run skwin.rm.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 250'
'run skwin.rm.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 250'
'run skwin.rm.'resol'.gs'
*
*AMM tp=' Sea Level Pressure and Tv Anomalies, Bias and RMSE for Total Field'
*AMM tv=' ------------------------------------------------------------------'
*AMM tc=' Analysis Date: 'tim
*AMM say ' '
*AMM say tp
*AMM say ' '
*AMM say tc
*AMM filenames='skills.rmv.'tim'.'resol'.pr'
*AMM rec=write(filenames,' ',append)
*AMM rec=write(filenames,tp,append)
*AMM rec=write(filenames,tv,append)
*AMM rec=write(filenames,' ',append)
*AMM rec=write(filenames,tc,append)
*AMM rec=close(filenames)
*
*AMM 'set lon 0 360'
*AMM 'set lat 20 80'
*AMM 'set lev 1000'
*AMM 'run sksfc.'resol'.gs'
*AMM 'set lon 0 360'
*AMM 'set lat -80 -20'
*AMM 'set lev 1000'
*AMM 'run sksfc.'resol'.gs'
*AMM 'set lon 0 360'
*AMM 'set lat -20 20'
*AMM 'set lev 1000'
*AMM 'run sksfc.'resol'.gs'
*AMM 'set lon -101.25 -11.25'
*AMM 'set lat -60 15'
*AMM 'set lev 1000'
*AMM 'run sksfc.'resol'.gs'
*
'!rm -f skgwu.'tim'.'resol'.pr'
filenameu='skgwu.'tim'.'resol'.pr'
rec=write(filenameu,' ',append)
rec=write(filenameu,' -99.9 means that data are unavailable',append)
rec=close(filenameu)
*
dirskl='/gfs/dk22/modoper/tempo/global/oens/skill'
'!rm -f skillg.rmv.'tim'.ens.m02'
'!cat skillt.rmv.'tim'.'resol'.pr skillg.rmv.'tim'.'resol'.pr skgeo.rmv.'tim'.'resol'.pr skgwu.rmv.'tim'.'resol'.pr > skillg.rmv.'tim'.ens.m02'
'!mv skillg.rmv.'tim'.ens.m02 'dirskl''
'!rm -f skillw.rmv.'tim'.ens.m02'
'!cat skillt.rmv.'tim'.'resol'.pr skillw.rmv.'tim'.'resol'.pr skwin.rmv.'tim'.'resol'.pr skgwu.rmv.'tim'.'resol'.pr > skillw.rmv.'tim'.ens.m02'
'!mv skillw.rmv.'tim'.ens.m02 'dirskl''
'!rm -f skills.rmv.'tim'.ens.m02'
*AMM '!cat skillt.rmv.'tim'.'resol'.pr skills.rmv.'tim'.'resol'.pr sksfc.rmv.'tim'.'resol'.pr skgwu.rmv.'tim'.'resol'.pr > skills.rmv.'tim'.ens.m02'
*AMM '!mv skills.rmv.'tim'.ens.m02 'dirskl''
'!rm -f skillt.rmv.'tim'.'resol'.pr'
'!rm -f skillg.rmv.'tim'.'resol'.pr skgeo.rmv.'tim'.'resol'.pr'
'!rm -f skillw.rmv.'tim'.'resol'.pr skwin.rmv.'tim'.'resol'.pr'
*AMM '!rm -f skills.rmv.rmv.'tim'.'resol'.pr sksfc.rmv.'tim'.'resol'.pr'
'!rm -f skgwu.rmv.'tim'.'resol'.pr'
*
endwhile
*
'!rm -f lterp.in lterp.out'
*
