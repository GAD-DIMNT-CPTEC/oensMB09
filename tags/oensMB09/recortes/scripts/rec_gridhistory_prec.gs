say 'resol lev labeli labelf nmemb tmax stt nloc dirinp dirout'
pull ctime
resol =subwrd(ctime,1)
lev   =subwrd(ctime,2)
labeli=subwrd(ctime,3)
labelf=subwrd(ctime,4)
nmemb =subwrd(ctime,5)
tmax  =subwrd(ctime,6)
stt   =subwrd(ctime,7)
nloc  =subwrd(ctime,8)
dirinp=subwrd(ctime,9)
dirout=subwrd(ctime,10)

say '*************INICIO DO RECORTE DO GH************'

dirinp=dirinp%'/'
dirout=dirout%'/'

fileinp=dirinp%'PREC'%labeli%labelf%'.'%resol%lev%'.ctl'
fileout=dirout%'PREC'%stt%labeli%labelf%'.'%resol%lev%'.bin'

say 'fileinp='fileinp
say 'fileout='fileout

floc='loc'%stt%'.'%labeli%'.txt'
rec=read(floc)
loc=sublin(rec,2)

say 'stt ='stt
say 'nloc='nloc
say 'loc ='loc 

'reinit'
say 'open 'fileinp
'open 'fileinp

'set fwrite 'fileout
'set gxout fwrite'

tt=1
while (tt <= tmax)
  say 'tt='tt
 'set t 'tt 

  m=1
  while (m <= nmemb)
   'set y 'm

    n=1
    while (n <= nloc)
      xx=subwrd(loc,n)
     'set x 'xx
     'd prec'
    n=n+1
    endwhile

  m=m+1
  endwhile
tt=tt+1
endwhile

'disable fwrite'

say '*************FIM DO RECORTE DO GH************'

'quit'

