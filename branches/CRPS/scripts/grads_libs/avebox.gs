function avebox(args)
var=subwrd(args,1)

'c'
'q dims';say result;
l5=sublin(result,5)
ti=subwrd(l5,11)
tf=subwrd(l5,13)
say ti' 'tf
'set t 'ti
'd 'var
* foi colocado o box pois o line nao esta funcionando bem!
'set rband 2 box 0 0 11.0 8.5'
'q pos'
_x1=subwrd(result,3)
_x2=subwrd(result,8)
_y1=subwrd(result,4)
_y2=subwrd(result,9)
if(_y1>_y2);tmp=_y1;_y1=_y2;_y2=tmp;endif;
say _x1' '_y1' '_x2' '_y2
'draw rec '_x1' '_y1' '_x2' '_y2
write('c:\coords.txt',_x1' '_y1' '_x2' '_y2)
'q xy2w '_x1' '_y1
_lon1=subwrd(result,3)
_lat1=subwrd(result,6)
'q xy2w '_x2' '_y2
_lon2=subwrd(result,3)
_lat2=subwrd(result,6)
say _lon1' '_lat1' '_lon2' '_lat2
say 'set t 'ti' 'tf;'set t 'ti' 'tf;
say 'define newval=aave('var',lon='_lon1',lon='_lon2',lat='_lat1',lat='_lat2')';'define newval=aave('var',lon='_lon1',lon='_lon2',lat='_lat1',lat='_lat2')';
return
