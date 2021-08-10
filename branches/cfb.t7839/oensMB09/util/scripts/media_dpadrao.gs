# Executar este script com o grads grads/2.1.a1

'reinit'

'set display color white'
'c'

# uvel, vvel, umes, temp, pslc
var='temp'

'open GANLENS2019081700R.unf.TQ0126L028.ctl'

'set lon 180'
'set z 1 28'

'define media=mean('var',e=1,e=7)'

'define dpad=sqrt(ave(pow(ave('var',e=1,e=7)-'var',2),e=1,e=7))'

if(var='uvel' | var='vvel')
'/stornext/home/carlos.bastarz/bin/grads_scripts/color.gs -20 20'
endif

if(var='umes')
'/stornext/home/carlos.bastarz/bin/grads_scripts/color.gs 0 1e-2 -kind white->blue'
endif

if(var='temp')
'/stornext/home/carlos.bastarz/bin/grads_scripts/color.gs 190 290 -kind blue->red'
endif

'd media'
'/stornext/home/carlos.bastarz/tools/grads_libs/grads/cbarn.gs'

'set gxout contour'
'd dpad'
