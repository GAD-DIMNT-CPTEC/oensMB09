say 'entre com yy mm dd hh mb nd'
pull ctime
yy=subwrd(ctime,1)
mm=subwrd(ctime,2)
dd=subwrd(ctime,3)
hh=subwrd(ctime,4)
mb=subwrd(ctime,5)
nd=subwrd(ctime,6)
resol=subwrd(ctime,7)
*
*climatologia
'!rm -f clima.gau.'resol'.ctl'
'!rm -f clima.gau.'resol''
say 'dayinty_gau.'resol'.ksh 'yy' 'mm' 'dd' 'hh
'!dayinty_gau.'resol'.ksh 'yy' 'mm' 'dd' 'hh' /gfs/home3/modoper/tempo/global/oenspro/produtos/SKILL/CLIMA/'
*
rec=read (fileinput.resol)
filei=sublin(rec,2)
rec=read (fileinput.resol)
fnmanl=sublin(rec,2)
'!rm -f fileinput.'resol''
if (filei = unavl)
say ' Analysis: 'filei': 'fnmanl
'quit'
else
say ' Analysis: 'filei': 'fnmanl
endif
*
*   Create ctls
'!doctl.ksh 'yy''mm''dd''hh' AVN 'nd
*
*   Open ctls
'open clima.gau.T126.ctl'
'run openctl.'resol'.gs'
*
'q files'
say result
*'quit'
'set t 1'
'query dims'
time=sublin(result,5)
tim=subwrd(time,6)
*
'!rm -f brierscoreg.'tim'.'resol'.pr'
*
ttl='Brier Score of Ensemble Operational CPTEC/COLA T126L28 Spectral Global Model '
ttm='   using as Control Initial Condition NMC T126L28 Spectral Analysis'
tg='       Geopotential Height Anomalies (above +50 m or below -50 m)'
tr=' -------------------------------------------------------------------'
ta=' Analysis Date: 'tim
*
filenameg='brierscoreg.'tim'.'resol'.pr'
rec=write(filenameg,ttl)
rec=write(filenameg,ttm,append)
rec=write(filenameg,' ',append)
rec=write(filenameg,tg,append)
rec=write(filenameg,tr,append)
rec=write(filenameg,' ',append)
rec=write(filenameg,ta,append)
rec=write(filenameg,' ',append)
rec=close(filenameg)
*
'set lon 0 360'
'set lat 20 80'
'set lev 500'
'run brierscore.'resol'.gs'
'set lon 0 360'
'set lat -80 -20'
'set lev 500'
'run brierscore.'resol'.gs'
'set lon 0 360'
'set lat -20 20'
'set lev 500'
'run brierscore.'resol'.gs'
'set lon -101.25 -11.25'
'set lat -60 15'
'set lev 500'
'run brierscore.'resol'.gs'
*
aa=' -99.99 means that data are unavailable'
rec=write(filenameg,' ',append)
rec=write(filenameg,aa,append)
rec=close(filenameg)
*
dirskl='/gfs/dk22/modoper/tempo/global/oens/skill'
'!mv  brierscoreg.'tim'.'resol'.pr 'dirskl'/brierscoreg.'tim'.ens.m02'
*
