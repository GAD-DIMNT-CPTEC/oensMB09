'reinit'

'set display color white'
'c'

'open GFCTENS20190817002019081703R.fct.TQ0126L028.ctl'

#'set t 1 last'
'set t 1 15'

'set x 1'
'set y 1'

vars='uvel vvel temp umes pslc'
vv=1
while(vv<=5)
var=subwrd(vars,vv)

if(var='pslc'); ztop=1; else; ztop=28; endif
zz=1
while(zz<=ztop)
'set z 'zz

say 'var='var', z='zz''

'set gxout stat'
'define ensmean=aave(mean('var',e=1,e=7),global)'
'd ensmean'
linha = sublin(result,8)
min = subwrd(linha,4)
max = subwrd(linha,5)

say min
say max

val = (max - min) 
absval = math_abs(val)

say val
say absval

vmin = min - absval
vmax = max + absval

say vmin
say vmax

'set gxout line'

'set vrange 'vmin' 'vmax''

ee=1
while(ee<=7)
'define ensmem=aave('var'(e='ee'),global)'
'set cmark 0'
'set cthick 8'
'set ccolor 15'
'd ensmem'
ee=ee+1
endwhile

'set cmark 4'
'set cthick 8'
'set digsiz 0.08'
'set ccolor 3'
'd ensmean'

'define ensctr=aave('var'(e=8),global)'
'set cmark 4'
'set cthick 4'
'set digsiz 0.08'
'set ccolor 1'
'd ensctr'

if(vmax>0 & vmin<0)
'set ccolor 1'
'set cmark 0'
'set cthick 4'
'd 'var'(e=8)*0'
endif

'cbar_line -c 3 1 15 -m 4 4 0 -l 8 8 8 -t "Media (7 membros)" "Controle" "Membros" -x 2.40215 -y 7.42438'

#'printim med_ctr_mens-'var'-z'zz'.png'

'c'
zz=zz+1
endwhile
vv=vv+1
endwhile
