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

* Figures for tems
VAR='tems'
PLEV=1000
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
     'set mpdset brmap_hires'
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
* Evaluate tems1 e tems2
*

      if (NCT <= 4)
         I=1
         while (I <= NMEMBR)
           'M1'VAR''I'=0.0'
           'M2'VAR''I'='VAR'.'I'(t=1)'
            I=I+1
         endwhile
      else
         I=1
         while (I <= NMEMBR)
            N=2*I
            M=N-1
           'M1'VAR''I'='VAR'.'M'(t=1)'
           'M2'VAR''I'='VAR'.'N'(t=1)'
            I=I+1
         endwhile
      endif

*
* Evaluate the cluster mean and produce the figure
*

      CLT=1
      while (CLT <= _NCLT.NCT)
        'VARMED1'CLT'=0.0'
        'VARMED2'CLT'=0.0'
         NMB=1
         while (NMB <= _NMEMB.NCT.CLT)
            NM=_MB.NCT.CLT.NMB
           'VARMED1'CLT'=VARMED1'CLT'+M1'VAR''NM''
           'VARMED2'CLT'=VARMED2'CLT'+M2'VAR''NM''
            NMB=NMB+1
         endwhile
        'VARMED1'CLT' =VARMED1'CLT'/'_NMEMB.NCT.CLT''
        'VARMED2'CLT' =VARMED2'CLT'/'_NMEMB.NCT.CLT''
         CLT=CLT+1
      endwhile

      INT ='-15 -12 -9 -6 -3 0 3 6 9 12 15 18 21 24 27 30 33 36 39'
      INTD='-10 -8 -6 -4 -2  2 4 6 8 10'
      CONT='0 15 30'
      COR =' 51 52 53 54 55 56    50 49 48 46 44   22 23 24 25 26 27 67 28 68'

      say 'clevs='INT
      say 'ccols='COR

      X=1
      CLT=1
      while (CLT <= _NCLT.NCT) 
        'set vpage 'Position(X)
        'set grads off'
        'set gxout shaded'
        'set clevs 'INT
        'set ccols 'COR
        'd VARMED2'CLT'-273.16'
         if (CLT = 1)
           'cbarn.gs'
         endif
        'set gxout contour'
        'set ccolor 39'
        'set clopts -1 -1 0.11'
        'set clevs 'CONT
        'd VARMED2'CLT'-273.16'

        'set clevs 'INTD
        'set ccolor 0'
        'set clopts -1 -1 0.11'
        'set cthick 1'
        'set cstyle 2'
        'd VARMED2'CLT'-VARMED1'CLT''
        'draw title cluster: 'CLT'  n.o de membros: '_NMEMB.NCT.CLT''

         X=X+1
         CLT=CLT+1
      endwhile

     'set vpage off'
     'set string 1 c 6'
     'set strsiz 0.11 0.12'
     'draw string 4.0 10.8 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'RESOL''
      if (NCT <= 4)
         'draw string 4.0 10.5 Cluster Medio para Temperatura a Superficie (C) (Cores)'
      else
         'draw string 4.0 10.5 Cluster Medio para Temperatura a Superficie (C) (Cores) e'
         'draw string 4.0 10.3 Variacao da Temperatura nas Ultimas 24 horas (Contornos Tracejados)'
      endif
     'draw string 4.0 10.0 Previsao de: '_DATEI.NCT'Z   Valido para: '_DATEF.NCT'Z'
   
     'printim '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.png png x800 y1000'
     '!/usr/bin/convert -trim '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.png  '_DIRGIF'/cluster'VAR''PLEV''_DATEI.NCT''_DATEF.NCT'.png'

*
* Close ctl's and reset and undefine Variables
*

      say 'REC=CloseCtl('NMEMBR','NCT')'
      REC=CloseCtl(NMEMBR,NCT)
      I=1
      while (I <= NMEMBR)
        'undefine M1'VAR''I''
        'undefine M2'VAR''I''
         I=I+1
      endwhile
      CLT=1
      while (CLT <= _NCLT.NCT) 
        'undefine VARMED1'CLT''
        'undefine VARMED2'CLT''
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
   NCT2=NCT-4
   MEMB=1
   while (MEMB <= NMEMBR) 

      if (NCT <= 4)
         say 'open '_CTL.MEMB.NCT
        'open '_CTL.MEMB.NCT
      else
         say 'open '_CTL.MEMB.NCT2
         say 'open '_CTL.MEMB.NCT
        'open '_CTL.MEMB.NCT2
        'open '_CTL.MEMB.NCT
      endif

      MEMB=MEMB+1
   endwhile
return 


********************************************************
*
* Function to close the ctl's
*
********************************************************

function CloseCtl(NMEMBR,NCT)
   MEMB=2*NMEMBR
   while (MEMB >= 1) 
     'close 'MEMB''
      MEMB=MEMB-1
   endwhile
return 


********************************************************
*
* Function to set up the position of picture on the virtual page
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

