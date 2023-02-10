say 'Enter with the parameters: TRC LABELI NMEMBR NCTLS RESOL PREFX'

pull CTIME

TRC   =subwrd(CTIME,1)
LABELI=subwrd(CTIME,2)
NMEMBR=subwrd(CTIME,3)
NCTLS =subwrd(CTIME,4)
RESOL =subwrd(CTIME,5)
PREFX =subwrd(CTIME,6)
_DIRGIF =subwrd(CTIME,7)
convert=subwrd(CTIME,8)

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

* Figures for prec+psnm
VAR='prec'
VAR2='psnm'
PLEV=1000
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR','VAR2')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR,VAR2)

* Figures for prec+wind925
VAR='prec'
VAR2='vento'
PLEV=925
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR','VAR2')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR,VAR2)

* Figures for prec+wind250
VAR='prec'
VAR2='vento'
PLEV=250
say 'REC=FigurePlot('RESOL','NCTLS','NMEMBR','LONW','LONE','LATS','LATN','PLEV','VAR','VAR2')'
REC=FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR,VAR2)

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

function FigurePlot(RESOL,NCTLS,NMEMBR,LONW,LONE,LATS,LATN,PLEV,VAR,VAR2)

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
* Evaluate the accumulated precipitation
*

      if (NCT <= 4)
         I=1
         while (I <= NMEMBR)
           'M'VAR''I'=0.0'
            if (VAR2 = 'psnm')
              'M'VAR2''I'='VAR2'.'I'(t=1)'
            endif
            if (VAR2 = 'vento')
              'MUVEL'I'=UVEL.'I'(t=1)'
              'MVVEL'I'=VVEL.'I'(t=1)'
            endif
            I=I+1
         endwhile
      else
         I=1
         while (I <= NMEMBR)
            N=4*I
           'M'VAR''I'=('VAR'.'N-3'(t=1)+'VAR'.'N-2'(t=1)+'VAR'.'N-1'(t=1)+'VAR'.'N'(t=1))/4'
            if (VAR2 = 'psnm')
              'M'VAR2''I'='VAR2'.'N'(t=1)'
            endif
            if (VAR2 = 'vento')
              'MUVEL'I'=UVEL.'N'(t=1)'
              'MVVEL'I'=VVEL.'N'(t=1)'
            endif
            I=I+1
         endwhile
      endif

*
* Evaluate the cluster mean and produce the figure
*

      CLT=1
      while (CLT <= _NCLT.NCT)
        'VARMED'CLT'=0.0'
         if (VAR2 = 'psnm')
           'VARMED2'CLT'=0.0'
         endif
         if (VAR2 = 'vento')
           'VARMEDU2'CLT'=0.0'
           'VARMEDV2'CLT'=0.0'
         endif
         NMB=1
         while (NMB <= _NMEMB.NCT.CLT)
            NM=_MB.NCT.CLT.NMB
           'VARMED'CLT'=VARMED'CLT'+M'VAR''NM''
            if (VAR2 = 'psnm')
              'VARMED2'CLT'=VARMED2'CLT'+M'VAR2''NM''
            endif
            if (VAR2 = 'vento')
              'VARMEDU2'CLT'=VARMEDU2'CLT'+MUVEL'NM''
              'VARMEDV2'CLT'=VARMEDV2'CLT'+MVVEL'NM''
            endif
            NMB=NMB+1
         endwhile
        'VARMED'CLT' =VARMED'CLT'/'_NMEMB.NCT.CLT''
         if (VAR2 = 'psnm')
           'VARMED2'CLT'=VARMED2'CLT'/'_NMEMB.NCT.CLT''
         endif
         if (VAR2 = 'vento')
           'VARMEDU2'CLT'=VARMEDU2'CLT'/'_NMEMB.NCT.CLT''
           'VARMEDV2'CLT'=VARMEDV2'CLT'/'_NMEMB.NCT.CLT''
         endif
         CLT=CLT+1
      endwhile

      if (VAR = 'prec')
         INTR='3 5 10 15 20 30 40 50 60 70 80'
         CORR='0 50 49 48 47 46 45 44 43 42 51 52 53 54'
         CONT='5 20 50'
      endif
      if (VAR2 = 'psnm')
         INT='964 968 972 976 980 984 988 992 996 1000 1004 1008 1012 1016 1020 1024 1028 1032 1036 1040 1044'
         COR='39 39 38 38 37 37 36 36 10    23 24 8    25 26 26 27 27 28 28 29 29'
      endif
      if (VAR2 = 'vento')
         if (PLEV = 925)
            INT='3 6 9 12 15 18 21 24 27 30 33'
            COR='39 38 37 10       23 24 8	25 26 27 28 29'
            SKP=6
            VMIN=15
         endif
         if (PLEV = 250)
            INT='5 10 15 20 25 30 35 40 45 50 55 60'
            COR='53 55   50 47 45   38 36   22 24 26 27  66 67'
            SKP=6
            VMIN=35
         endif
      endif

      say 'clevs='INT
      say 'ccols='COR

      X=1
      CLT=1
      while (CLT <= _NCLT.NCT) 
        'set vpage 'Position(X)
        'set grads off'
         if (NCT > 4)
            if (VAR = 'prec')
              'set gxout shaded'
              'set clevs 'INTR
              'set ccols 'CORR
              'd smth9(VARMED'CLT')'
               if (CLT = 1)
                 'cbarn.gs'
               endif
              'set gxout contour'
              'set ccolor 39'
              'set cthick 1'
              'set clopts -1 -1 0.11'
              'set clevs 'CONT
              'd smth9(VARMED'CLT')'
            endif
         endif
         if (VAR2 = 'psnm')
           'set gxout contour'
           'set clevs 'INT
           'set ccols 'COR
           'set clopts -1 -1 0.11 '
           'set cthick 1'
           'd VARMED2'CLT''
         endif
         if (VAR2 = 'vento')
           'set gxout barb'
           'set ccolor 25'
           'd skip(VARMEDU2'CLT',6,6);VARMEDV2'CLT''
         endif
        'draw title cluster: 'CLT'  membros: '_NMEMB.NCT.CLT''
         X=X+1
         CLT=CLT+1
      endwhile

     'set vpage off'
     'set string 1 c 6'
     'set strsiz 0.11 0.12'
     'draw string 4.0 10.8 CPTEC/INPE/MCT - PREVISAO DE TEMPO GLOBAL POR ENSEMBLE - 'RESOL''
      if (VAR2 = 'psnm')
       'draw string 4.0 10.5 Cluster Medio para Pressao ao Nivel Medio do Mar (hPa) (Contornos)'
      endif
      if (VAR2 = 'vento')
       'draw string 4.0 10.5 Cluster Medio para: Vento em 'PLEV' hPa (Barbelas)'
      endif
      if (NCT > 4)
       'draw string 4.0 10.3 e Precipitacao Acumulada em 24 horas (mm) (Cores)'
      endif
     'draw string 4.0 10.0 Previsao de: '_DATEI.NCT'Z   Valido para: '_DATEF.NCT'Z'
   
     'printim '_DIRGIF'/cluster'VAR''VAR2''PLEV''_DATEI.NCT''_DATEF.NCT'.png png x1000 y1200'
     '!'convert' -trim '_DIRGIF'/cluster'VAR''VAR2''PLEV''_DATEI.NCT''_DATEF.NCT'.png ' _DIRGIF'/cluster'VAR''VAR2''PLEV''_DATEI.NCT''_DATEF.NCT'.png'

*
* Close ctl's and reset and undefine Variables
*

      say 'REC=CloseCtl('NMEMBR','NCT')'
      REC=CloseCtl(NMEMBR,NCT)
      I=1
      while (I <= NMEMBR)
        'undefine M'VAR''I''
         if (VAR2 = 'psnm')
           'undefine M'VAR2''I''
         endif
         if (VAR2 = 'vento')
           'undefine MUVEL'I''
           'undefine MVVEL'I''
         endif
         I=I+1
      endwhile
      CLT=1
      while (CLT <= _NCLT.NCT) 
        'undefine VARMED'CLT''
         if (VAR2 = 'psnm')
           'undefine VARMED2'CLT''
         endif
         if (VAR2 = 'vento')
           'undefine VARMEDU2'CLT''
           'undefine VARMEDV2'CLT''
         endif
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
   NCT3=NCT-3
   NCT2=NCT-2
   NCT1=NCT-1
   MEMB=1
   while (MEMB <= NMEMBR) 

      if (NCT <= 4)
         say 'open '_CTL.MEMB.NCT
        'open '_CTL.MEMB.NCT
      else
         say 'open '_CTL.MEMB.NCT3
         say 'open '_CTL.MEMB.NCT2
         say 'open '_CTL.MEMB.NCT1
         say 'open '_CTL.MEMB.NCT
        'open '_CTL.MEMB.NCT3
        'open '_CTL.MEMB.NCT2
        'open '_CTL.MEMB.NCT1
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
   MEMB=4*NMEMBR
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

