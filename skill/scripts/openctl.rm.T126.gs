say 'Enter mb nd'
pull ctime
mb=subwrd(ctime,1)
nd=subwrd(ctime,2)
*
say 'mb='mb
say 'nd='nd
*
'reinit'
*
*   Climatology - IEEE
resol='T126'
rec=read (filec.resol.n)
opnclm=sublin(rec,2)
*say 'opnclm='opnclm
'open 'opnclm
*
*   Initial Condition 
rec=read (fileanl.resol)
opnicn=sublin(rec,2)
*say 'opnicn='opnicn
'open 'opnicn
*
*   Forecast 
*
t=1
while (t <= nd)
rec=read (filefctAVN.resol)
opnfct=sublin(rec,2)
*say 'opnfct='opnfct
'open 'opnfct
t=t+1
endwhile
*
t=1
while (t <= nd)
rec=read (filefctrmv.resol)
opnfct=sublin(rec,2)
*say 'opnfct='opnfct
'open 'opnfct
t=t+1
endwhile
*
 
 
 
 
 
 
 
 
