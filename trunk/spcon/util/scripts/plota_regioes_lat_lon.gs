'reinit'

'set display color white'
'c' 

vars='prs win hum tem'
regs='hn tr hs nas sas'
mems='01 02 03 04 05 06 07'

vv=1
while(vv<=4)
var=subwrd(vars,vv)

rr=1
while(rr<=5)
reg=subwrd(regs,rr)

mm=1
while(mm<=7)
mem=subwrd(mems,mm)

say var' 'reg' 'mem

'open 'var'pe'reg''mem'12019081700.ctl'
'open 'var'pn'reg''mem'12019081700.ctl'

if(var='prs'); 'color -5 5'  ; endif
#if(var='win'); 'color -10 10'; endif
if(var='hum'); 'color -0.005 0.005'  ; endif
if(var='tem'); 'color -20 20'; endif

if(var='win')
'color -10 10'
'd (uwnp.1-uwnp.2)/2'
'cbarn'
'draw title uwnp z=1 mem='mem' reg='reg''
'printim uwnp_z1_mem'mem'_'reg'.png'
#pull x
'c'
'color -10 10'
'd (vwnp.1-vwnp.2)/2'
'cbarn'
'draw title vwnp z=1 mem='mem' reg='reg''
'printim vwnp_z1_mem'mem'_'reg'.png'
#pull x
'c'
else
'd ('var'.1-'var'.2)/2'
'cbarn'
'draw title 'var' z=1 mem='mem' reg='reg''
'printim 'var'_z1_mem'mem'_'reg'.png'
#pull x
'c'
endif

'close 2'
'close 1'

mm=mm+1
endwhile

rr=rr+1
endwhile

vv=vv+1
endwhile

'quit'
