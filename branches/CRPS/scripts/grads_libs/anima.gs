function anima(args)
_var=subwrd(args,1)
ff=subwrd(args,2)

*
* Check if time is varying or not
*
'q dims'
l5=sublin(result,5);say l5;tvariation=subwrd(l5,3);
if(tvariation="varying")
   _ti=subwrd(l5,11)
   _tf=subwrd(l5,13)
else
*
*  If not assumes total range defined in the CTL 
*
   _ti=1
   say 'Looking for the last time defined in the control file...'
   'q file'
   lin = sublin(result,5)
   _tf=subwrd(lin,12)
endif

'set t '_ti;say 'set t '_ti;
'q dims'
xx=sublin(result,5)
data=subwrd(xx,6)
dd=substr(data,4,2)
mmm=substr(data,6,3)
aaaa=substr(data,9,4)
tempoi=subwrd(xx,9)
say dd%mmm%aaaa
'set t '_tf
say 'set t '_tf
'q dims'
xx=sublin(result,5)
data=subwrd(xx,6)
dd=substr(data,4,2)
mmm=substr(data,6,3)
aaaa=substr(data,9,4)
tempof=subwrd(xx,9)
say dd%mmm%aaaa
*'set gxout fwrite'
'set gxout shaded'
'set dbuff on'

'd '_var
'q shades'
nlevs=sublin(result,1)
nlevs=subwrd(nlevs,5)
say nlevs
cclevs=' '
i=1
while(i<=nlevs-1)
   currlin=sublin(result,i+1)
   color=subwrd(currlin,1)
   limite=subwrd(currlin,3)
   cclevs=cclevs%limite%' '
   say cclevs
   i=i+1
endwhile

k=tempoi
while(k<=tempof)
   'swap'
   'set t 'k;say k;    
   'q dims'
   xx=sublin(result,5)
   data=subwrd(xx,6)
   hhz=substr(data,1,3)
   dd=substr(data,4,2)
   mmm=substr(data,6,3)
   aaaa=substr(data,9,4)
   say hhz%dd%mmm%aaaa
*'set fwrite as.bin'
*   'd (olrd+olrn)/2'
*   'set cmax 120'
*   Anomalias normalizadas
*   'set clevs  -3.0 -2.0 -1.0 -0.8 -0.6 -0.4 -0.2 0.2 0.4 0.6 0.8 1.0 2.0 3.0'
*   Altura geop em 500 (dividida por 1000)
*   'set clevs 5.0 5.1 5.2 5.3 5.4 5.5 5.6 5.7 5.8 5.9 6.0'
*   'set ccols 72 2 62 58 54 0 46 42 38 34 27'
*   'set cint .2'
*   'set clevs  20 25 30 35 40 45 50'
*    'set clevs  -6 -5 -4 -3 -2 -1 1 2 3 4 5 6'
*   'set clevs  -3.0 -2.0 -1.0 -0.8 -0.6 -0.4 -0.2 0.2 0.4 0.6 0.8 1.0 2.0 3.0'
   say 'set clevs'cclevs
   'set clevs'cclevs
   'd '_var;'run cbarn';
   'draw title 'hhz%dd%mmm%aaaa
    yy=1
   'q pos'
   button=subwrd(result,5);say button;
   if(button=1);k=k+1;endif;
   if(button=3);k=k-1;endif;
   if(k=_tf+1);k=tempoi;endif;
   if(k=_ti-1);k=tempof;endif;
endwhile
*    while(yy<1e5)
*        yy=yy+1
*    endwhile
