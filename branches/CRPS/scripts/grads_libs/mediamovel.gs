function mmovel(args)
dias=subwrd(args,1)
testa=subwrd(args,2)
if(testa='')
    'set t 1 last'
    'q dims'
    l5=sublin(result,5)
    ti=subwrd(l5,11)
    tf=subwrd(l5,13) 
    say ti' 'tf
    ti=ti+((dias-1)/2)
    tf=tf-((dias-1)/2)
    'set t 'ti' 'tf 
    'q dims'
    l5=sublin(result,5)
    ti=subwrd(l5,11)
    tf=subwrd(l5,13)
    say 'ti='ti' tf='tf
    'set t 'ti' 'tf
else
    datai=subwrd(args,2)
    dataf=subwrd(args,3)
    'set time 'datai' 'dataf
    say 'set time 'datai' 'dataf
    'q dims';say result;
    l5=sublin(result,5)
    ti=subwrd(l5,11)
    tf=subwrd(l5,13) 
    say ti' 'tf
    ti=ti+((dias-1)/2)
    tf=tf-((dias-1)/2)
    'set t 'ti' 'tf 
    'q dims';say result;
    l5=sublin(result,5)
    ti=subwrd(l5,11)
    tf=subwrd(l5,13)
    say 'ti='ti' tf='tf
    'set t 'ti' 'tf

endif


* Calculando a media movel e definindo-a como nova variavel 
'q file 1'
l7=sublin(result,7)
var=subwrd(l7,1)
say 'A variavel eh: 'var
halfwidth=((dias-1)/2)
say 'Calculando a media movel ...' 
say 'define mmov=ave('var',t-'halfwidth',t+'halfwidth')'
'define mmov=ave('var',t-'halfwidth',t+'halfwidth')'
say 'A nova variavel eh: mmov'

return
