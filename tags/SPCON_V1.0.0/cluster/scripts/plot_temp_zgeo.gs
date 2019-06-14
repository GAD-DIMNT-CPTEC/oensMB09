say 'Enter with the parameters: TRC LABELI NMEMBR NCTLS RESOL PREFX'

pull CTIME

TRC   =subwrd(CTIME,1)
LABELI=subwrd(CTIME,2)
NMEMBR=subwrd(CTIME,3)
NCTLS =subwrd(CTIME,4)
RESOL =subwrd(CTIME,5)
PREFX =subwrd(CTIME,6)
_DIRGIF =subwrd(CTIME,7)

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

*
* Read the ctl and cluster files
*

say 'REC=ReadCtl('TRC','LABELI','NMEMBR','NCTLS','PREFX')'
REC=ReadCtl(TRC,LABELI,NMEMBR,NCTLS,PREFX)

*
* Obtain dates and identify the clusters
*

say 'REC=DateCluster('NCTLS')'
REC=DateCluster(NCTLS)

*
* Produce the cluster figures
*

LONW=-101.25; LONE=-11.25
LATS= -60.00; LATN= 15.00 

* Figures for temp 1000 hPa
VAR='temp'
PLEV=1000
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR)

* Figures for temp 925 hPa
VAR='temp'
PLEV=925
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR)

* Figures for temp 850 hPa
VAR='temp'
PLEV=850
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR)

* Figures for temp 700 hPa
VAR='temp'
PLEV=700
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR)

* Figures for zgeo 500 hPa
VAR='zgeo'
PLEV=500
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR)

* Figures for zgeo 200 hPa
VAR='zgeo'
PLEV=200
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR)

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

   LSTFCT=fileclt''LABELI'.'TRC
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
* Function to obtain dates and identify the clusters
*
********************************************************

function DateCluster(NCTLS)

NCT=1      
while (NCT <= NCTLS)

*
* Get the initial and final dates
*

      say '_CLTRS.'NCT'= '_CLTRS.NCT
      REC=read(_CLTRS.NCT)
      REC=read(_CLTRS.NCT)
      LINE=sublin(REC,2)
     _DATEI.NCT=subwrd(LINE,3)
     _DATEF.NCT=subwrd(LINE,6)
      say ' '
      say '_DATEI.'NCT': '_DATEI.NCT'  _DATEF.'NCT': '_DATEF.NCT
      say ' '

*
* Get the number of clusters, the number of members that each cluster contain
* and identify the members of each cluster
*

      REC=read(_CLTRS.NCT)
      LINE=sublin(REC,2)
     _NCLT.NCT=subwrd(LINE,4)
      say 'Number of clusters: '_NCLT.NCT
      CLT=1
 
      while (CLT <= _NCLT.NCT)
         REC=read(_CLTRS.NCT)
         LINE=sublin(REC,2)
        _NMEMB.NCT.CLT=subwrd(LINE,5)
         say 'Number of members of the cluster.'NCT'.'CLT': '_NMEMB.NCT.CLT
      
         say 'The members of the cluster 'CLT' are: '
         NMB=1
         while (NMB <= _NMEMB.NCT.CLT)
            REC=read(_CLTRS.NCT)
            LINE=sublin(REC,2)
           _MB.NCT.CLT.NMB=subwrd(LINE,2)
            say _MB.NCT.CLT.NMB
            NMB=NMB+1
         endwhile
 
         CLT=CLT+1
      endwhile
      REC=close(_CLTRS.NCT)
   NCT=NCT+1
endwhile
return


********************************************************
*
* Function to produce figures
*
********************************************************

function FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR)

NCT=1
while (NCT <= NCTLS)

     'c'
     'set mpdset brmap_mres'
     'set grid off'
     'set map 15'
     'set annot 1 5'

*
* Open ctl's
*
      say 'REC=OpenCtl('NMEMBR','NCT')'
      REC=OpenCtl(NMEMBR,NCT)

     'set lon 'LONW' 'LONE
     'set lat 'LATS' 'LATN
     'set lev 'PLEV

*
* Evaluate the cluster mean and produce the figure
*

      CLT=1
      while (CLT <= _NCLT.NCT)
        'VARMED'CLT'=0.0'
         NMB=1
         while (NMB <= _NMEMB.NCT.CLT)
            nm=_MB.NCT.CLT.NMB
           'VARMED'CLT'=VARMED'CLT'+'VAR'.'nm''
            NMB=NMB+1
         endwhile
        'VARMED'CLT'=VARMED'CLT'/'_NMEMB.NCT.CLT''
         CLT=CLT+1
      endwhile
   
      if (VAR = 'temp')
         if (PLEV = 1000)
            INT='-15 -12 -9 -6 -3 0 3 6 9 12 15 18 21 24 27 30 33 36 39'
            COR='51 52 53 54 55 56    50 49 48 46 44	22 23 24 25 26 27 67 28 68'
            CONT='0 15 24'
         endif
         if (PLEV = 925)
            INT='-18 -15 -12 -9 -6 -3 0 3 6 9 12 15 18 21 24 27 30 33 36'
            COR='51 52 53 54 55 56    50 49 48 46 44	22 23 24 25 26 27 67 28 68'
            CONT='0 9 18'
         endif
         if (PLEV = 850)
            INT='-21 -18 -15 -12 -9 -6 -3 0 3 6 9 12 15 18 21 24 27 30 33'
            COR='51 52 53 54 55 56    50 49 48 46 44   22 23 24 25 26 27 67 28 68'
            CONT='-12 0 12'
         endif
         if (PLEV = 700)
            INT='-30 -27 -24 -21 -18 -15 -12 -9 -6 -3 0 3 6 9 12 15 18 21'
            COR='51 52 53 54 55 56    50 49 48 46 44   22 23 24 25 26 27 67 28'
            CONT='-9 0 9'
         endif
      endif
      if (VAR = 'zgeo')
         if (PLEV = 500)
            INT='4600 4700 4800 4900 5000 5100 5200 5300 5400 5500 5600 5700 5800 5840 5880 5900 5910 5920 5930 5940 5950'
            COR='51 52 53 54 55 56    50 49 48 46 44	38 37 35   24 26 27 67 28 68 29 69'
         endif
         if (PLEV = 200)
            INT='10000 10200 10400 10600 10800 11000 11200 11400 11600 11800 12000 12200 12400 12600 12800 13000 13200'
            COR='53 54 55 56	50 49 48 46 44   38 37 35   24 26 27 67 28 68 29'
         endif
      endif

      say 'clevs='INT
      say 'ccols='COR

      X=1
      CLT=1
      while (CLT <= _NCLT.NCT) 
        'set vpage 'Position(X)
        'set grads off'
         if (VAR = 'temp')
           'set gxout shaded'
           'set clevs 'INT
           'set ccols 'COR
           'd smth9(VARMED'CLT'-273.15)'
            if (CLT = 1)
              'cbarn.gs'
            endif
           'set gxout contour'
           'set ccolor 1'
           'set cthick 1'
           'set clopts 1 -1 0.100'
           'set clevs 'CONT
           'd smth9(VARMED'CLT'-273.15)'
           'draw title cluster: 'CLT'  n.o de membros: '_NMEMB.NCT.CLT''
         endif
         if (VAR = 'zgeo')
           'set gxout contour'
           'set clevs 'INT
           'set ccols 'COR
           'set cthick 6'
           'd smth9(VARMED'CLT')'
           'draw title cluster: 'CLT'  n.o de membros: '_NMEMB.NCT.CLT''
         endif
         X=X+1
         CLT=CLT+1
      endwhile

     'set vpage off'
     'set string 1 c 6'
     'set strsiz 0.11 0.12'
     'draw string 4.0 10.8 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'RESOL''
      if (VAR = 'temp')
       'draw string 4.0 10.6 Cluster Medio para a Temperatura (C) em 'PLEV' hPa'
      endif
      if (VAR = 'zgeo')
       'draw string 4.0 10.6 Cluster Medio para a Altura Geopotencial (m) em 'PLEV' hPa'
      endif
     'draw string 4.0 10.4 Previsao de: '_DATEI.NCT'Z   Valido para: '_DATEF.NCT'Z'
   
     'printim '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.gif gif x800 y1000'
*     'printim '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.gif gif x640 y800'
     '!/usr/X11R6/bin/convert -crop 0x0 '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.gif  '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.png'
     '!rm -f '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.gif  '
   
*
* Close ctl's and reset and undefine Variables
*
      say 'REC=CloseCtl('NMEMBR','NCT')'
      REC=CloseCtl(NMEMBR,NCT)
      CLT=1
      while (CLT <= _NCLT.NCT) 
        'undefine VARMED'CLT''
         CLT=CLT+1
      endwhile
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
   while (MEMB <= NMEMBR) 
      say 'open '_CTL.MEMB.NCT
     'open '_CTL.MEMB.NCT
      MEMB=MEMB+1
   endwhile
return 


********************************************************
*
* Function to close the ctl's
*
********************************************************

function CloseCtl(NMEMBR,NCT)
   MEMB=NMEMBR
   while (MEMB >= 1) 
     'close 'MEMB''
      MEMB=MEMB-1
   endwhile
return 


********************************************************
*
* Function to set up the position of picture in the virtual page
*
********************************************************

function Position(N)
FIG.1=' 0.10 4.50 6.50 10.00'
FIG.2=' 4.00 8.40 6.50 10.00'
FIG.3=' 0.10 4.50 3.25  6.75'
FIG.4=' 4.00 8.40 3.25  6.75'
FIG.5=' 0.10 4.50 0.00  3.50'
FIG.6=' 4.00 8.40 0.00  3.50'
return FIG.N

