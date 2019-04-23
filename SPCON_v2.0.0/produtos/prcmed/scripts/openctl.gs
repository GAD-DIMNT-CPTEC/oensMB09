say 'Enter trc nweek date'
pull ctime
trc=subwrd(ctime,1)
nweek=subwrd(ctime,2)
date=subwrd(ctime,3)

say 'trc='trc
say 'nweek='nweek
say 'date='date

'reinit'
*
*   Climatology
*
t=1
while (t <= nweek)
say fileclm''date'.'trc
lstfct=fileclm''date'.'trc
rec=read (lstfct)
opnfct=sublin(rec,2)
say t' opnfct='opnfct
'open 'opnfct
t=t+1
endwhile

*
*   Forecast 
*
t=1
while (t <= nweek)
lstfct=filefct''date'.'trc
say lstfct
rec=read (lstfct)
opnfct=sublin(rec,2)
say 'opnfct='opnfct
'open 'opnfct
t=t+1
endwhile
