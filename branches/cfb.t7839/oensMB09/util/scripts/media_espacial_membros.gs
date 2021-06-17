'reinit'

'open template_ens_all.ctl'

'set t 1 last'

vars='uvel'

vv=1
while(vv<=6)
  var=subwrd(vars,vv)
  
  if(var='pslc'); levs='1000'; len=1; endif
  if(var='prec'); levs='1000'; len=1; endif
  if(var='umes'); levs='1000 925 850 700 500'; len=5; endif
  if(var='temp'); levs='1000 925 850 700 500 300 250 200 50'; len=9; endif
  if(var='uvel'); levs='1000 925 850 700 500 300 250 200 50'; len=9; endif
  if(var='vvel'); levs='1000 925 850 700 500 300 250 200 50'; len=9; endif

  ll=1
  while(ll<=len)
    lev=subwrd(levs,ll)

    say var' 'lev

    if(var='pslc'); lev=1000; range='984.8 986'; endif
    if(var='prec'); lev=1000; range='0 8'; endif

    if(var='umes' & lev=1000); range='0.009 0.012'; endif
    if(var='umes' & lev=925); range='0.007 0.009'; endif
    if(var='umes' & lev=850); range='0.005 0.007'; endif
    if(var='umes' & lev=700); range='0.001 0.005'; endif
    if(var='umes' & lev=500); range='0.0005 0.0025'; endif

    if(var='temp' & lev=1000); range='286 290'; endif
    if(var='temp' & lev=925); range='282 286'; endif
    if(var='temp' & lev=1000); range='278 283'; endif

    if(var='uvel' & lev=1000); range='-1.5 0.5'; endif
    if(var='uvel' & lev=925); range='-0.6 0.8'; endif
    if(var='uvel' & lev=850); range='0.5 2.0'; endif

    if(var='vvel' & lev=1000); range='-0.5 0.4'; endif
    if(var='vvel' & lev=925); range='-0.5 0.4'; endif
    if(var='vvel' & lev=850); range='-0.5 0.4'; endif

    'set lev 'lev

    'set grid off'
    'set grads off'

 
* Membros
    ee=2
    while(ee<=15)
      'define mmean=aave('var'(e='ee'),global)'
      'set x 1'
      'set vrange 'range
      'set cmark 0'
      'set ccolor 1'
      if(ee=1); 'set cmark 1'; endif
      'd mmean'
      ee=ee+1
    endwhile
      
* Controle
    'define cmean=aave('var'(e=1),global)'
    'set cmark 0'
    'set ccolor 3'
    'd cmean'

* Media do conjunto
    'define emean=aave(mean('var',e=2,e=15),global)'
    'set cmark 3'
    'set ccolor 4'
    'd emean'

    pull x
    'c'

    ll=ll+1
  endwhile
  vv=vv+1
endwhile
