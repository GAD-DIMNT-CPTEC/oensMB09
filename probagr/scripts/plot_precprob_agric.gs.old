say 'Enter with: resol trc labeli nblst dirbct dirfig exp'
pull ctime

resol =subwrd(ctime,1)
trc   =subwrd(ctime,2)
labeli=subwrd(ctime,3)
nblst =subwrd(ctime,4)
dirbct=subwrd(ctime,5)
dirfig=subwrd(ctime,6)
exp   =subwrd(ctime,7)

dirbct=dirbct%'/'
dirfig=dirfig%'/'

*
* Figures for 1st page of agriculture products (Lead times: 5, 10 and 15 days)
*

agric_time='5 10 15'

*
* Set colors for figures
*

'rgbset.gs'

*
* List of ctl files
*

flist=filefct%labeli%'.'%trc
say 'flist='flist

*
* Looping to produce all figures
*

nld=1
nf=1
while (nf <= nblst)

*
* Open Ctl
*

   rec=read(flist)
   ctlfile=sublin(rec,2)
  

*   if (pag_agri.nf='true')

     'reinit'

      ld=subwrd(agric_time,nld)

      say 'open 'dirbct''ctlfile''
     'open 'dirbct''ctlfile''

*
* Obtain the date of the forecasting
*

      labelii=substr(ctlfile,12,10)
      labelff=substr(ctlfile,22,10)

      mesii=substr(labelii,5,2)
      mesff=substr(labelff,5,2)

      if (mesii = 01);mmi='JAN';endif
      if (mesii = 02);mmi='FEV';endif
      if (mesii = 03);mmi='MAR';endif
      if (mesii = 04);mmi='ABR';endif
      if (mesii = 05);mmi='MAI';endif
      if (mesii = 06);mmi='JUN';endif
      if (mesii = 07);mmi='JUL';endif
      if (mesii = 08);mmi='AGO';endif
      if (mesii = 09);mmi='SET';endif
      if (mesii = 10);mmi='OUT';endif
      if (mesii = 11);mmi='NOV';endif
      if (mesii = 12);mmi='DEZ';endif

      if (mesff = 01);mmf='JAN';endif
      if (mesff = 02);mmf='FEV';endif
      if (mesff = 03);mmf='MAR';endif
      if (mesff = 04);mmf='ABR';endif
      if (mesff = 05);mmf='MAI';endif
      if (mesff = 06);mmf='JUN';endif
      if (mesff = 07);mmf='JUL';endif
      if (mesff = 08);mmf='AGO';endif
      if (mesff = 09);mmf='SET';endif
      if (mesff = 10);mmf='OUT';endif
      if (mesff = 11);mmf='NOV';endif
      if (mesff = 12);mmf='DEZ';endif

      labeli=substr(labelii,7,2)%'/'%mmi%'/'%substr(labelii,1,4)%' '%substr(labelii,9,2)
      labelf=substr(labelff,7,2)%'/'%mmf%'/'%substr(labelff,1,4)%' '%substr(labelff,9,2)

*
* Set Grads environment
*

     'set grid off'
     'set display color white'
     'c'
     'set gxout shaded'
     'set mpdset brmap_mres'

     'set lat -60 15'
     'set lon -101.25 -11.25'
  
*
* Plot the figures
*

     'set grads off'
     'set clevs 5 35 65 95'
     'set ccols 0 80 81 82 83'
     'd smth9(prob10*100)'
     'run cbarn.gs'
  
     'set string 1 c 6'
     'set strsiz 0.11 0.12'
     'draw string 5.5 8.3 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'resol''
     'draw string 5.5 8.1 Previsao de Probabilidades de Precipitacao Acumulada em 5 dias > 10.0 mm' 
     'draw string 5.5 7.9 Acumalado entre: 'labeli'Z  e  'labelf'Z'
  
     'printim 'dirfig'prec_agric_large'ld'.gif gif x900 y700'
*     '!convert2 -crop 0x0 'dirfig'prec_agric_large'ld'.gif 'dirfig'prec_agric_large'ld'.gif'
     '!convert2 'dirfig'prec_agric_large'ld'.gif 'dirfig'prec_agric_large'ld'.gif'

     'c'
     'set grads off'
     'set clevs 5 35 65 95'
*     'set ccols 0 32 34 37 39'
     'set ccols 0 80 81 82 83'
     'd smth9(prob10*100)'
     'run cbarn.gs 2.0 1 9.50 4.25'

     'printim 'dirfig'prec_agric_small'ld'.gif gif x900 y700'
     '!convert2 -crop 0x0 -geometry 240x210 'dirfig'prec_agric_small'ld'.gif 'dirfig'prec_agric_small'ld'.gif'
      nld=nld+1
   
   nf=nf+1
endwhile
   
'quit'

   
   
   
   
   
   
   
   
   
