say 'Enter with the parameters: TRC LABELI NMEMBR NCTLS RESOL PREFX EXP'

pull CTIME

TRC   =subwrd(CTIME,1)
LABELI=subwrd(CTIME,2)
NMEMBR=subwrd(CTIME,3)
NCTLS =subwrd(CTIME,4)
RESOL =subwrd(CTIME,5)
PREFX =subwrd(CTIME,6)
EXP   =subwrd(CTIME,7)

say 'TRC   = 'TRC
say 'LABELI= 'LABELI
say 'NMEMBR= 'NMEMBR
say 'NCTLS = 'NCTLS
say 'RESOL = 'RESOL
say 'PREFX = 'PREFX

*
* Set environmental of figures
*
'set display color white'
'c'
'rgbset.gs'

'set gxout shaded'
'set grid off'

*
* Read the ctl files
*

say 'REC=ReadCtl('TRC','LABELI','NMEMBR','NCTLS','PREFX')'
REC=ReadCtl(TRC,LABELI,NMEMBR,NCTLS,PREFX)

*
* Open ctl's
*

NCT=1
say 'REC=OpenCtl('NMEMBR','NCT')'
REC=OpenCtl(NMEMBR,NCT)

*
* Produce the spaguetti figures
*

LONW= -180.0; LONE=  180.0
LATNHS=-45.0; LATNHN= 30.0 

* Figures for temp 850 hPa
VAR='temp'; PLEV=850; CTNI='-5 -4 -3 -2 -1 1 2 3 4 5'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for temp 500 hPa
VAR='temp'; PLEV=500; CTNI='-5 -4 -3 -2 -1 1 2 3 4 5'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for temp 250 hPa
VAR='temp'; PLEV=250; CTNI='-5 -4 -3 -2 -1 1 2 3 4 5'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for uvel 850 hPa
VAR='uvel'; PLEV=850; CTNI='-10 -8 -6 -4 -2 2 4 6 8 10'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for uvel 500 hPa
VAR='uvel'; PLEV=500; CTNI='-10 -8 -6 -4 -2 2 4 6 8 10'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for uvel 250 hPa
VAR='uvel'; PLEV=250; CTNI='-10 -8 -6 -4 -2 2 4 6 8 10'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for vvel 850 hPa
VAR='vvel'; PLEV=850; CTNI='-10 -8 -6 -4 -2 2 4 6 8 10'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for vvel 500 hPa
VAR='vvel'; PLEV=500; CTNI='-10 -8 -6 -4 -2 2 4 6 8 10'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

* Figures for vvel 250 hPa
VAR='vvel'; PLEV=250; CTNI='-10 -8 -6 -4 -2 2 4 6 8 10'
say 'REC=FigurePlot('RESOL','LABELI','NCTLS','NMEMBR','LONW','LONE','LATNHS','LATNHN','PLEV','VAR','CTNI')'
REC=FigurePlot(RESOL,LABELI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

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

function FigurePlot(RESOL,DATEI,NCTLS,NMEMBR,LONW,LONE,LATNHS,LATNHN,PLEV,VAR,CTNI,EXP)

NCT=1
while (NCT <= NCTLS)

     'c'
     'set mpdset mres'
     'set map 15 1 3'
     'set grads off'

     'set lev 'PLEV
     'set lon 'LONW' 'LONE
     'set lat 'LATNHS' 'LATNHN

*
* Produce the figure
*

      nl=1
      nc=1
      MEMB=1
      while (MEMB <= (NMEMBR-1)/2) 
         say 'vpage 'nl' 'nc' 4 2'
        'vpage 'nl' 'nc' 4 2'

        'set xlopts 1 4 0.15'
        'set ylopts 1 4 0.15'
        'set ylint 10'

        'set clevs 'CTNI
        'set ccols 49 47 45 43 42 0 22 23 25 27 29'
         say 'd smth9('VAR'.'MEMB'-'VAR'.'NMEMBR')'
        'd smth9('VAR'.'MEMB'-'VAR'.'NMEMBR')'
        'cbarn.gs'
        'draw title Perturbacao: 'MEMB''

         MEMB=MEMB+1
         if (nc = 2);nl=nl+1;endif
         if (nc = 1)
            nc=2
         else
            nc=1
         endif
      endwhile


     'set vpage off'

   
     'set string 1 c 6'
     'set strsiz 0.12 0.12'
     'draw string 5.5  8.4 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'RESOL''
      if (VAR = 'temp')
       'draw string 5.5  8.2 Perturbacoes Iniciais da Temperatura (C) ('PLEV' hPa)'
      endif
      if (VAR = 'uvel')
       'draw string 5.5  8.2 Perturbacoes Iniciais da Componente Zonal do Vento (m/s) ('PLEV' hPa)'
      endif
      if (VAR = 'vvel')
       'draw string 5.5  8.2 Perturbacoes Iniciais da Componente Meridional do Vento (m/s) ('PLEV' hPa)'
      endif
     'set string 1 c 6'
     'set strsiz 0.12 0.12'
     'draw string 5.5  8.0 Valido para: 'DATEI'Z'


*     'printim 'EXP'/perturbations'VAR''PLEV'_'DATEI''DATEI'.png png x900 y700'
     say '==========================> printim 'EXP'/perturbations'VAR''PLEV'_'DATEI''DATEI'.png'
     
*     'printim 'EXP'/perturbations'VAR''PLEV'_'DATEI''DATEI'.png png x720 y560'
     'printim 'EXP'/perturbations'VAR''PLEV'_'DATEI''DATEI'.png png x1024 y768'
     '!/usr/bin/convert -trim 'EXP'/perturbations'VAR''PLEV'_'DATEI''DATEI'.png 'EXP'/perturbations'VAR''PLEV'_'DATEI''DATEI'.png'

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



