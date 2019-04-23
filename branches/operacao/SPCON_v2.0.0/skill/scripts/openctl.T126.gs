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
memb=1
while (memb <= (mb-1)/2) 
if (memb < 10);memb='0'memb;endif
t=1
while (t <= nd)
rec=read (filefct''memb''P.resol)
opnfct=sublin(rec,2)
*say 'opnfct='opnfct
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
rec=read (filefct''memb''N.resol)
opnfct=sublin(rec,2)
*say 'opnfct='opnfct
'open 'opnfct
t=t+1
endwhile
memb=memb+1
endwhile
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
*LE RMVt=1
*LE RMVwhile (t <= nd)
*LE RMVrec=read (filefctrmv.resol)
*LE RMVopnfct=sublin(rec,2)
*LE RMV*say 'opnfct='opnfct
*LE RMV'open 'opnfct
*LE RMVt=t+1
*LE RMVendwhile
*
 
 
 
 
 
 
 
 
