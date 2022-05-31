'reinit'

'set display color white'
'c' 

'set grid off'

vars='prs win hum tem'
regs='hn tr hs nas sas'

vv=1
while(vv<=4)
var=subwrd(vars,vv)

rr=1
while(rr<=5)
reg=subwrd(regs,rr)

say var' 'reg

'open 'var'pe'reg'ens12019081700.ctl'
'open 'var'pn'reg'ens12019081700.ctl'

'set map 15'

if(var='win')
'color 0 0.5 -kind white->blue'
'd sqrt(ave(pow(ave(uwnp.1,e=1,e=7)-uwnp.1,2),e=1,e=7)) - sqrt(ave(pow(ave(uwnp.2,e=1,e=7)-uwnp.2,2),e=1,e=7))'
'cbarn'
'set gxout contour'
'set ccolor 1'
'set cthick 4'
'd (mean(uwnp.1,e=1,e=7) - mean(uwnp.2,e=1,e=7))/2'
'draw title med. e espal. da pert. de uwnp, z=1 reg='reg''
'printim mean_dp_uwnp_z1_'reg'.png'
#pull x
'c'
'color 0 0.5 -kind white->blue'
'd sqrt(ave(pow(ave(vwnp.1,e=1,e=7)-vwnp.1,2),e=1,e=7)) - sqrt(ave(pow(ave(vwnp.2,e=1,e=7)-vwnp.2,2),e=1,e=7))'
'cbarn'
'set gxout contour'
'set ccolor 1'
'set cthick 4'
'd (mean(vwnp.1,e=1,e=7) - mean(vwnp.2,e=1,e=7))/2'
'draw title med. e espal. da pert. de vwnp, z=1 reg='reg''
'printim mean_dp_vwnp_z1_'reg'.png'
#pull x
'c'
else
if(var='prs'); 'color 0 0.5 -kind white->blue' ; endif
if(var='hum'); 'color 0 5e-4 -kind white->blue'; endif
if(var='tem'); 'color 0 0.5 -kind white->blue' ; endif
'd sqrt(ave(pow(ave('var'.1,e=1,e=7)-'var'.1,2),e=1,e=7)) - sqrt(ave(pow(ave('var'.2,e=1,e=7)-'var'.2,2),e=1,e=7))'
'cbarn'
'set gxout contour'
'set ccolor 1'
'set cthick 4'
'd (mean('var'.1,e=1,e=7) - mean('var'.2,e=1,e=7))/2'
'draw title med. e espal. da pert. de 'var' z=1 reg='reg''
'printim mean_dp_'var'_z1_'reg'.png'
#pull x
'c'
endif

'close 2'
'close 1'

rr=rr+1
endwhile

vv=vv+1
endwhile

'quit'
