say 'Enter with the parameters: TRC LABELI NCTLS RESOL'

pull CTIME

TRC   =subwrd(CTIME,1)
LABELI=subwrd(CTIME,2)
NCTLS =subwrd(CTIME,3)
RESOL =subwrd(CTIME,4)
_DIRGIF =subwrd(CTIME,5)

say 'TRC   = 'TRC
say 'LABELI= 'LABELI
say 'NCTLS = 'NCTLS
say 'RESOL = 'RESOL
say 'DIRGIF = '_DIRGIF

*
* Set environmental of figures
*

'set display color white'
'c'
'rgbset.gs'

*
* Read the ctl and spread files
*

say ' '
say 'REC=ReadCtl('TRC','LABELI','NCTLS')'
REC=ReadCtl(TRC,LABELI,NCTLS)

*
* Produce the cluster figures
*

LONW=-130.00; LONE=  0.00
LATS= -80.00; LATN= 20.00 

* Figures for psnm
say ' '
VAR='psnm'
VARS='spsnm'
PLEV=1000
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','LONW','LONE','LATS','LATN','PLEV','VAR','VARS')'
REC=FigurePlot(RESOL,LABELI,NCTLS,LONW,LONE,LATS,LATN,PLEV,VAR,VARS)

* Figures for zgeo500
say ' '
VAR='zgeo'
VARS='szgeo'
PLEV=500
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','LONW','LONE','LATS','LATN','PLEV','VAR','VARS')'
REC=FigurePlot(RESOL,LABELI,NCTLS,LONW,LONE,LATS,LATN,PLEV,VAR,VARS)

* Figures for zgeo250
say ' '
PLEV=250
VAR='zgeo'
VARS='szgeo'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','LONW','LONE','LATS','LATN','PLEV','VAR','VARS')'
REC=FigurePlot(RESOL,LABELI,NCTLS,LONW,LONE,LATS,LATN,PLEV,VAR,VARS)

* Figures for temp850
say ' '
VAR='temp'
VARS='stemp'
PLEV=850
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','LONW','LONE','LATS','LATN','PLEV','VAR','VARS')'
REC=FigurePlot(RESOL,LABELI,NCTLS,LONW,LONE,LATS,LATN,PLEV,VAR,VARS)

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

function ReadCtl(TRC,LABELI,NCTLS)

   say ' '
   say 'On the ReadCtl:'
   say 'TRC   = 'TRC
   say 'LABELI= 'LABELI
   say 'NCTLS = 'NCTLS

   LSTFCT=filefctENM''LABELI'.'TRC
   say 'LSTFCT= 'LSTFCT
   NCT=1
   while (NCT <= NCTLS) 
      REC=read(LSTFCT)
     _CTL.NCT=sublin(REC,2)
      say '_CTL.'NCT'= '_CTL.NCT
      NCT=NCT+1
   endwhile
   REC=close(LSTFCT)

   LSTFCT=filespr''LABELI'.'TRC
   say 'LSTFCT= 'LSTFCT
   NCT=1
   while (NCT <= NCTLS) 
      REC=read(LSTFCT)
     _CLTRS.NCT=sublin(REC,2)
      say '_CLTRS.'NCT'= '_CLTRS.NCT
      NCT=NCT+1
   endwhile
   REC=close(LSTFCT)
return 


********************************************************
*
* Function to produce figures
*
********************************************************

function FigurePlot(RESOL,DATEI,NCTLS,LONW,LONE,LATS,LATN,PLEV,VAR,VARS)

NCT=1
while (NCT <= NCTLS)

*
* Set environmental of figures
*

     'set grid off'
     'set map 0 1 6'
     'set mpdset brmap_hires'
     'c'

*
* Open ctl's
*

      say 'open '_CTL.NCT
      say 'open '_CLTRS.NCT
     'open '_CTL.NCT
     'open '_CLTRS.NCT

*
* Obtain dates and identify the clusters
*

      say 'DATEF=EndDate()'
      DATEF=EndDate()
      say 'DATEF= 'DATEF
      
*
* Plot the figure
*

     'set lon 'LONW' 'LONE
     'set lat 'LATS' 'LATN
     'set lev 'PLEV

      if (VAR = 'psnm')
         INT='1 2 3 4 5 6 7 8 9 10'
         COR='44 46 48 50  39 38 37  23 24 25 26 27 28 29'
      endif
      if (VAR = 'temp')
         if (PLEV = 850)
            INT='0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0'
            COR='44 46 48 50 39 38 37 23 24 25 26 27 28 29'
         endif
      endif
      if (VAR = 'zgeo')
         if (PLEV = 500)
            INT='10 20 30 40 50 60 70 80 90 100'
            COR='44 46 48 50  39 38 37  23 24 25 26 27 28 29'
         endif
         if (PLEV = 250)
            INT='20 40 60 80 100 120 140 160 180 200'
            COR='44 46 48 50 39 38 37 23 24 25 26 27 28 29'
         endif
      endif

      say 'clevs='INT
      say 'ccols='COR

     'set grads off'
      if (VAR = 'psnm')
        'set gxout shaded'
        'set clevs 'INT
        'set ccols 'COR
        'd smth9('VARS'.2)'
        'cbarn.gs'
        'set gxout contour'
        'set ccolor 1'
        'set cthick 6'
        'set cmin 960'
        'set cmax 1044'
        'set cint 4'
        'set clskip 2'
        'set clopts 1 -1 0.095'
        'd smth9('VAR')'
      endif
      
      if (VAR = 'temp')
        'set gxout shaded'
        'set clevs 'INT
        'set ccols 'COR
        'd smth9('VARS'.2)'
        'cbarn.gs'
        'set gxout contour'
        'set ccolor 1'
        'set cthick 6'
        'set cmin -30'
        'set cmax  30'
        'set cint 3'
        'set clskip 2'
        'set clopts 1 -1 0.095'
        'd smth9('VAR'-273.16)'
      endif  
        
      if (VAR = 'zgeo')
        'set gxout shaded'
        'set clevs 'INT
        'set ccols 'COR
        'd smth9('VARS'.2)'
        'cbarn.gs'
        'set gxout contour'
        'set ccolor 1'
        'set cthick 6'
         if (PLEV = 500)
          'set cmin 4500'
          'set cmax 6500'
          'set cint   50'
         endif
         if (PLEV = 250)
          'set cmin  9000'
          'set cmax 11600'
          'set cint   100'
         endif        
        'set clskip 2'
        'set clopts 1 -1 0.095'
        'd smth9('VAR')'
      endif

     'set string 1 c 6'
     'set strsiz 0.11 0.12'
     'draw string 5.5  8.3 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'RESOL''
      if (VAR = 'psnm')
       'draw string 5.5 8.1 Pressao ao Nivel Medio do Mar * (hPa) [contorno] - Espalhamento do Ensemble (hPa) [cores]'
      endif
      if (VAR = 'temp')
       'draw string 5.5 8.1 Temperatura do Ar * (C) - 'PLEV' hPa [contorno] - Espalhamento do Ensemble (C) [cores]'
      endif
      if (VAR = 'zgeo')
       'draw string 5.5 8.1 Altura Geopotencial * (m) - 'PLEV' hPa [contorno] - Espalhamento do Ensemble (m) [cores]'
      endif
     'draw string 5.5 7.9 Previsao a partir de: 'DATEI'Z  Valido para: 'DATEF'Z' 
     'set string 1 l 6'
     'set strsiz 0.12 0.12'
     'draw string 1.5 0.3 * Media do Conjunto de Previsoes'
   
      if (VAR = 'psnm')
        'printim '_DIRGIF'/'VAR''DATEI''DATEF'.png png x773 y602'
        '!/usr/bin/convert -trim '_DIRGIF'/'VAR''DATEI''DATEF'.png ' _DIRGIF'/'VAR''DATEI''DATEF'.png '
      endif
      if (VAR = 'temp')
        'printim '_DIRGIF'/'VAR''PLEV''DATEI''DATEF'.png png x773 y602'
        '!/usr/bin/convert -trim '_DIRGIF'/'VAR''PLEV''DATEI''DATEF'.png ' _DIRGIF'/'VAR''PLEV''DATEI''DATEF'.png '
      endif
     if (VAR = 'zgeo')
        'printim '_DIRGIF'/geop'PLEV''DATEI''DATEF'.png png x773 y602'
        '!/usr/bin/convert -trim '_DIRGIF'/geop'PLEV''DATEI''DATEF'.png ' _DIRGIF'/geop'PLEV''DATEI''DATEF'.png '
      endif
   
*
* Close ctl's and reset Variables
*

     'close 2'
     'close 1'
     'reset'
  
   NCT=NCT+1
endwhile
return


***********************************************************
*
* Function to obtain dates 
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
