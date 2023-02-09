say 'Enter with the parameters: TRC LABELI NMEMBR NCTLS RESOL PREFX DIRFIG'

pull CTIME

TRC   =subwrd(CTIME,1)
LABELI=subwrd(CTIME,2)
NMEMBR=subwrd(CTIME,3)
NCTLS =subwrd(CTIME,4)
RESOL =subwrd(CTIME,5)
PREFX =subwrd(CTIME,6)
DIRFIG=subwrd(CTIME,7)
_DIRFIG=DIRFIG%'/'

say 'TRC   = 'TRC
say 'LABELI= 'LABELI
say 'NMEMBR= 'NMEMBR
say 'NCTLS = 'NCTLS
say 'RESOL = 'RESOL
say 'PREFX = 'PREFX
say '_DIRFIG= '_DIRFIG

*
* Set environmental of figures
*
'set display color white'
'c'
'rgbset.gs'

*
* Read the ctl files
*

say 'REC=ReadCtl('TRC','LABELI','NMEMBR','NCTLS','PREFX')'
REC=ReadCtl(TRC,LABELI,NMEMBR,NCTLS,PREFX)

*
* Produce the spaguetti figures
*

LONW= -180.0; LONE=  180.0
LATNHS= 10.0; LATNHN= 90.0 
LATSHS=-90.0; LATSHN=-10.0 

* Figures for temp 1000 hPa
VAR='temp'; PLEV=1000; CTNI=10.0; CTNS=20.0
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','LATSHS','LATSHN','PLEV','VAR','CTNI','CTNS')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,LATSHS,LATSHN,PLEV,VAR,CTNI,CTNS)

* Figures for temp 850 hPa
VAR='temp'; PLEV=850; CTNI=0.0; CTNS=15.0
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','LATSHS','LATSHN','PLEV','VAR','CTNI','CTNS')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,LATSHS,LATSHN,PLEV,VAR,CTNI,CTNS)

* Figures for geop 500 hPa
VAR='zgeo'; PLEV=500; CTNI=5600.0; CTNS=5800.0
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','LATSHS','LATSHN','PLEV','VAR','CTNI','CTNS')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,LATSHS,LATSHN,PLEV,VAR,CTNI,CTNS)

'quit'


********************************************************
*                                                      *
*                      FUNCTIONS                       *
*                                                      *
********************************************************


********************************************************
*
* Function to read the ctl's
*
********************************************************

function ReadCtl(TRC,LABELI,NMEMBR,NCTLS,PREFX)
   say ' '
   say 'On the ReadCtl:'
   say 'TRC   = 'TRC
   say 'LABELI= 'LABELI
   say 'NMEMBR= 'NMEMBR
   say 'NCTLS = 'NCTLS
   say 'PREFX = 'PREFX

   MEMBT=0
   MEMBP=1
   while (MEMBP <= (NMEMBR-1)/2) 
      IF (MEMBP < 10);MEMBP='0'MEMBP;endif
      LSTFCT=filefct''MEMBP''P''LABELI'.'TRC
      say 'LSTFCT= 'LSTFCT
      MEMBT=MEMBT+1
      NCT=1
      while (NCT <= NCTLS) 
         REC=read(LSTFCT)
        _CTL.MEMBT.NCT=sublin(REC,2)
         say '_CTL.'MEMBT'.'NCT'= '_CTL.MEMBT.NCT
         NCT=NCT+1
      endwhile
      REC=close(LSTFCT)
      MEMBP=MEMBP+1
   endwhile

   MEMBN=1
   while (MEMBN <= (NMEMBR-1)/2) 
      MEMBT=MEMBT+1
      IF (MEMBN < 10);MEMBN='0'MEMBN;endif
      LSTFCT=filefct''MEMBN''N''LABELI'.'TRC
      say 'LSTFCT= 'LSTFCT
      NCT=1
      while (NCT <= NCTLS) 
         REC=read(LSTFCT)
        _CTL.MEMBT.NCT=sublin(REC,2)
         say '_CTL.'MEMBT'.'NCT'= '_CTL.MEMBT.NCT
         NCT=NCT+1
      endwhile
      REC=close(LSTFCT)
      MEMBN=MEMBN+1
   endwhile

   MEMBT=MEMBT+1
   LSTFCT=filefct''PREFX''LABELI'.'TRC
   say 'LSTFCT= 'LSTFCT
   NCT=1
   while (NCT <= NCTLS) 
      REC=read(LSTFCT)
     _CTL.MEMBT.NCT=sublin(REC,2)
      say '_CTL.'MEMBT'.'NCT'= '_CTL.MEMBT.NCT
      NCT=NCT+1
   endwhile
   REC=close(LSTFCT)

   MEMBT=MEMBT+1
   LSTFCT=filefctENM''LABELI'.'TRC
   say 'LSTFCT= 'LSTFCT
   NCT=1
   while (NCT <= NCTLS) 
      REC=read(LSTFCT)
     _CTL.MEMBT.NCT=sublin(REC,2)
      say '_CTL.'MEMBT'.'NCT'= '_CTL.MEMBT.NCT
      NCT=NCT+1
   endwhile
   REC=close(LSTFCT)
return 


********************************************************
*
* Function to produce figures
*
********************************************************

function FigurePlot(RESOL,DATEI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,LATSHS,LATSHN,PLEV,VAR,CTNI,CTNS)

NCT=1
while (NCT <= NCTLS)

     'c'
     'set mpdset brmap_hires'
     'set map 15 1 3'
     'set grads off'

*
* Open ctl's
*

      say 'REC=OpenCtl('NMEMBR','NCT')'
      REC=OpenCtl(NMEMBR,NCT)

*
* Obtain end date
*

      say 'DATEF=EndDate()'
      DATEF=EndDate()
      say 'DATEF= 'DATEF
      
*
* Produce the figure
*

      if (VAR = 'temp')
*
* Northern Hemisphery
*
        'set parea 1.0 7.0 5.6 10.1'
        'set lev 'PLEV
        'set lon 'LONW' 'LONE
        'set lat 'LATNHS' 'LATNHN
        'set mproj nps'
         MEMB=1
         while (MEMB <= NMEMBR) 
           'set clevs 'CTNS
           'set ccols 2'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB'-273.16)'
           'set clevs 'CTNI
           'set ccols 3'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB'-273.16)'
            MEMB=MEMB+1
         endwhile
        'set clevs 'CTNS
        'set ccols 1'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB'-273.16)'
        'set clevs 'CTNI
        'set ccols 9'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB'-273.16)'
*
* Southern Hemisphery
*
        'set parea 1.0 7.0 0.9 5.4'
        'set lev 'PLEV
        'set lon 'LONW' 'LONE
        'set lat 'LATSHS' 'LATSHN
        'set mproj sps'
        'draw map'
         MEMB=1
         while (MEMB <= NMEMBR) 
           'set clevs 'CTNS
           'set ccols 2'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB'-273.16)'
           'set clevs 'CTNI
           'set ccols 3'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB'-273.16)'
            MEMB=MEMB+1
         endwhile
        'set clevs 'CTNS
        'set ccols 1'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB'-273.16)'
        'set clevs 'CTNI
        'set ccols 9'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB'-273.16)'
      endif

      if (VAR = 'zgeo')
*
* Northern Hemisphery
*
        'set parea 1.0 7.0 5.6 10.1'
        'set lev 'PLEV
        'set lon 'LONW' 'LONE
        'set lat 'LATNHS' 'LATNHN
        'set mproj nps'
         MEMB=1
         while (MEMB <= NMEMBR) 
           'set clevs 'CTNS
           'set ccols 2'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB')'
           'set clevs 'CTNI
           'set ccols 3'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB')'
            MEMB=MEMB+1
         endwhile
        'set clevs 'CTNS
        'set ccols 1'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB')'
        'set clevs 'CTNI
        'set ccols 9'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB')'
*
* Southern Hemisphery
*
        'set parea 1.0 7.0 0.9 5.4'
        'set lev 'PLEV
        'set lon 'LONW' 'LONE
        'set lat 'LATSHS' 'LATSHN
        'set mproj sps'
        'draw map'
         MEMB=1
         while (MEMB <= NMEMBR) 
           'set clevs 'CTNS
           'set ccols 2'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB')'
           'set clevs 'CTNI
           'set ccols 3'
           'set clab off'
           'set cthick 1'
           'd smth9('VAR'.'MEMB')'
            MEMB=MEMB+1
         endwhile
        'set clevs 'CTNS
        'set ccols 1'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB')'
        'set clevs 'CTNI
        'set ccols 9'
        'set clab off'
        'set cthick 6'
        'd smth9('VAR'.'MEMB')'
      endif
   
     'set string 1 c 6'
     'set strsiz 0.12 0.12'
     'draw string 4.25  10.85 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'RESOL''
      if (VAR = 'temp')
       'draw string 4.25  10.60 Diagrama "Spaguetti" - Temperatura (C) ('PLEV' hPa)'
       'set string 1 l 6'
       'set strsiz 0.12 0.12'
       'draw string 0.79 0.70 Membros do Ensemble ('CTNS' graus)'
       'draw string 0.79 0.30 Membros do Ensemble ('CTNI' graus)'
       'draw string 4.94 0.70 Ensemble Medio ('CTNS' graus)'
       'draw string 4.94 0.30 Ensemble Medio ('CTNI' graus)'
      endif
      if (VAR = 'zgeo')
       'draw string 4.25  10.60 Diagrama "Spaguetti" - Altura Geopotencial (m) ('PLEV' hPa)'
       'set string 1 l 6'
       'set strsiz 0.12 0.12'
       'draw string 0.79 0.70 Membros do Ensemble ('CTNS' m)'
       'draw string 0.79 0.30 Membros do Ensemble ('CTNI' m)'
       'draw string 4.94 0.70 Ensemble Medio ('CTNS' m)'
       'draw string 4.94 0.30 Ensemble Medio ('CTNI' m)'
      endif
     'set string 1 c 6'
     'set strsiz 0.12 0.12'
     'draw string 4.25  10.35 Previsao a partir de: 'DATEI'Z  Valido para: 'DATEF'Z'
     'set line 2 1 1'
     'draw line 0.3 0.7 0.75 0.7'
     'set line 3 1 1'
     'draw line 0.3 0.3 0.75 0.3'
     'set line 1 1 6'
     'draw line 4.45 0.7 4.90 0.7'
     'set line 9 1 6'
     'draw line 4.45 0.3 4.90 0.3'
   
      if (VAR = 'temp')
*        'printim '_DIRFIG'sptgl'VAR''PLEV''DATEI''DATEF'.png png x640 y823'
        'printim '_DIRFIG'sptgl'VAR''PLEV''DATEI''DATEF'.png png x768 y1024'
*        '!/usr/bin/convert -trim '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png'
        '!/home/carlos.bastarz/bin/convert -trim '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png'
*        say '!/usr/bin/convert -trim '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png'
        say '!/home/carlos.bastarz/bin/convert -trim '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png '_DIRFIG'/sptgl'VAR''PLEV''DATEI''DATEF'.png'
      endif
      if (VAR = 'zgeo')
*        'printim '_DIRFIG'sptglgeop'PLEV''DATEI''DATEF'.png png x640 y823'
        'printim '_DIRFIG'sptglgeop'PLEV''DATEI''DATEF'.png png x768 y1024'
*        '!/usr/bin/convert -trim '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png'
        '!/home/carlos.bastarz/bin/convert -trim '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png'
*        say '!/usr/bin/convert -trim '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png'
        say '!/home/carlos.bastarz/bin/convert -trim '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png '_DIRFIG'/sptglgeop'PLEV''DATEI''DATEF'.png'
      endif

*
* Close ctl's and reset and undefine Variables
*

      say 'REC=CloseCtl('NMEMBR','NCT')'
      REC=CloseCtl(NMEMBR,NCT)
      CLT=1
     'reset'
  
   NCT=NCT+1
endwhile
return



********************************************************
*
* Function to open the ctl's
*
********************************************************

function OpenCtl(NMEMBR,NCT)
   MEMB=1
   while (MEMB <= NMEMBR+1) 
      say 'open '_CTL.MEMB.NCT
     'open '_CTL.MEMB.NCT
      MEMB=MEMB+1
   endwhile
return 



***********************************************************
*
* Function to obtain end date 
*
***********************************************************

function EndDate()

*
* Get the end date
*

   'set t 1'
   'q time'
    VAR=subwrd(result,5)
    YY=substr(VAR,9,4)
    MC=substr(VAR,6,3)
    DD=substr(VAR,4,2)
    HH=substr(VAR,1,2)
    if (MC = 'JAN');MM='01';endif
    if (MC = 'FEB');MM='02';endif
    if (MC = 'MAR');MM='03';endif
    if (MC = 'APR');MM='04';endif
    if (MC = 'MAY');MM='05';endif
    if (MC = 'JUN');MM='06';endif
    if (MC = 'JUL');MM='07';endif
    if (MC = 'AUG');MM='08';endif
    if (MC = 'SEP');MM='09';endif
    if (MC = 'OCT');MM='10';endif
    if (MC = 'NOV');MM='11';endif
    if (MC = 'DEC');MM='12';endif

    DATEF=YY''MM''DD''HH

return DATEF



********************************************************
*
* Function to close the ctl's
*
********************************************************

function CloseCtl(NMEMBR,NCT)
   MEMB=NMEMBR+1
   while (MEMB >= 1) 
     'close 'MEMB''
      MEMB=MEMB-1
   endwhile
return 



