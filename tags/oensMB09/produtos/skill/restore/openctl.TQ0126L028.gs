say 'Enter mb nd'
*pull ctime
mb=15
nd=15
*
say 'mb='mb
say 'nd='nd
*
'reinit'
*
*   Climatology - IEEE
resol='TQ0126L028'
rec=read ("filec."resol".n")
opnclm=sublin(rec,2)
say "rec="rec

say 'opnclm='opnclm
'open 'opnclm
*

*   Initial Condition 
rec=read ("fileanl."resol)
opnicn=sublin(rec,2)
say 'opnicn='opnicn
'open 'opnicn

*
*   Forecast 
memb=1
while (memb <= (mb-1)/2) 
if (memb < 10);memb='0'memb;endif
t=1
while (t <= nd)
rec=read ("filefct"memb"P."resol)
opnfct=sublin(rec,2)
say 'opnfct='opnfct
'open 'opnfct
t=t+1
endwhile
memb=memb+1
endwhile
memb=1
while (memb <= (mb-1)/2) 
if (memb < 10);memb='0'memb;endif
t=1
while (t <= nd)
rec=read ("filefct"memb"N."resol)
opnfct=sublin(rec,2)
say 'opnfct='opnfct
'open 'opnfct
t=t+1
endwhile
memb=memb+1
endwhile
*
t=1
while (t <= nd)
rec=read ("filefctNMC."resol)
opnfct=sublin(rec,2)
say 'opnfct='opnfct
'open 'opnfct
t=t+1
endwhile
*
