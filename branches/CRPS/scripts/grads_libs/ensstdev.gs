function enstdv(args)
media=subwrd(args,1)
nens=subwrd(args,2)
var=subwrd(args,3)

'define mesmed=('media')'
i=1
std=0
while (i<=nens)
std='pow((ave('var'.'i',t+0,t+2)-mesmed),2)+'std
i=i+1
endwhile
say 'Defining standard deviation among members of ensemble...'
'define stdev=sqrt((('std')/'nens'))'
return
