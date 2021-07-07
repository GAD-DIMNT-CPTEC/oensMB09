* Uso: grads -blc 'run stdev_fct48_24hrs.gs'
* carlos.bastarz@inpe.br (12/04/2021)

'reinit'

'open template_48hr.ctl'
'open template_24hr.ctl'

var='umes'

outfile = 'stdev_'var'.txt'
'set gxout print'
fmt='%8.3f'
numcols=1
'set prnopts 'fmt' 'numcols' 1'

if(var='pslc')
'set z 1'
else
'set z 1 28'
endif

'define gstdev=sqrt(ave(pow('var'.1-'var'.2,2),t=1,t=53))'

'define mgstdev=aave(gstdev,g)'

'set x 1'
if(var='umes')
'd mgstdev*1000'
else
'd mgstdev'
endif

i=1
while (1)
  line = sublin(result,i)
  if (line = ''); break; endif
  if (i>1)
    rc = write(outfile,line,append)
  endif
  i=i+1
endwhile

'quit'
