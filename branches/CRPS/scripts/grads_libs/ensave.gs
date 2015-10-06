function monta(args)
nens=subwrd(args,1)
say 'args= 'nens
var=subwrd(args,2)
say 'var= 'var
i = 0
a = '('
while (i < nens)
 i = i + 1
 if (i < nens)
   a = a%var'.'i'+'
 else
   a = a%var'.'i')/'i
 endif
endwhile
'define ensave='a
say 'The ensemble average has been defined with the name ensave '
'q define';say result;
return

