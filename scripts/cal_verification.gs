'reinit'

tools=''
pathm=''
pathr=''
out=''
n=80
i=1
var='prec'
***************PARAMETROS QUE DEPENDEM DO PRODUTO***************
prol='WEEK0 FORT0 3WKS0 MNTH0'
incrproi=1
incrprof=4
****************************************************************
***************PARAMETROS QUE DEPENDEM DO MES DE AVALIACAO***************
monthl='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
*monthi ALTERAR LINHA 44
*monthf ALTERAR LINHA 45
*************************************************************************

while(incrproi<=incrprof)

   plt=subwrd(prol,incrproi)
   
   if(plt=WEEK0)
      ltf=4
      pro='week'
      
   endif
   if(plt=FORT0)
      ltf=2
      pro='fort'
      
   endif
      if(plt=3WKS0)
      ltf=1
      pro='3wks'
      
   endif
      if(plt=MNTH0)
      ltf=1
      pro='mnth'
   endif   
   monthi=5
   monthf=6
   while(monthi<=monthf)
   
      month=subwrd(monthl,monthi)
   
      lt=1
      while(lt<=ltf)

         'open 'pathm''var'-'pro'/anomalies/data/BAM12_'month'_'plt''lt'.ctl'
         'open 'pathr''var'-'pro'/anomalias/dados/GPCP_'month'_'plt''lt'.ctl'
         'set display color white'
         'c'

         'set gxout shaded'
         'set font 1'
         'set map 1 1 3'
         'set display color white'
         'set mproj latlon'
         'set grads off'
         'set lat -90 90'
         'set lon -260 190'

***************ENSEMBLE MEAN***************

         'set t 1 'n
         'define var3=ave(varx.1,e=1,e=11)'

********************MEANS******************

         'set t 1'
         'define fm=ave(var3,t=1,t='n')'
         'define rm=ave(varx.2,t=1,t='n')'

**************STANDARD DEVIATION***********

         'define fstd=sqrt(sum(pow(var3-fm,2),t=1,t='n')/'n')'
         'define rstd=sqrt(sum(pow(varx.2-rm,2),t=1,t='n')/'n')'

***************CORRELATION***************

         'corr=sum(((var3-fm)/fstd)*((varx.2-rm)/rstd),t=1,t='n')/'n''

***************RMSE***************

         'define rmse=sqrt((sum(pow(var3-varx.2, 2), t=1, t='n'))/'n')'
*         'define rmsec=sqrt((sum(pow(0-varx.2, 2), t=1, t='n'))/'n')'
         
***************MSE***************

         'define mse=(sum(pow(var3-varx.2, 2), t=1, t='n'))/'n''
         'define msec=(sum(pow(0-varx.2, 2), t=1, t='n'))/'n''

***************PHASE ERROR***************

         'define pher=2*(fstd/rstd)*corr'

***************AMPLITUDE ERROR***************

         'define aper=pow(fstd/rstd,2)'
  
***********CONDITIONAL BIAS***********

          'define cnbs=pow(corr-(fstd/rstd),2)'

***********UNCONDITIONAL BIAS***********

          'define unbs=pow((fm-rm)/rstd,2)'

***************MSSS***************

           'define msss=1-(mse/msec)'

***************FIGURRES***************
        ''tools'cmrgb.gs'
         metl='corr rmse pher aper cnbs unbs msss'
         meti=1
         metf=7

         while(meti<=metf)
         
            met=subwrd(metl,meti)
            
            if(met=corr)
               metn='CORRELATION'
               vale='-0.8 -0.6 -0.4 -0.2 0.2 0.4 0.6 0.8'
               valc='20 21 22 23 24 25 26 27 28'
            endif
            if(met=rmse)
               metn='RMSE'
               vale='1 2 3 4 5 6 7 8 9 10'
               valc='0 29 30 31 32 33 34 35 36 37 38'
            endif
            if(met=cnbs)
               metn='CONDITIONAL BIAS'
               vale='0.1 0.2 0.4 0.6 0.9 1.1 2'
               valc='39 40 41 42 43 44 45 46'
            endif
            if(met=msss)
               metn=MSSS
               vale='-0.1 0 0.1 0.2 0.4 0.6 0.8'
               valc='47 48 49 50 51 52 53 54'
            endif
            if(met=unbs)
               metn='UNCONDITIONAL BIAS'
               vale='-0.5 0.5'
               valc='55 56 57'
            endif
            if(met=aper)
               metn='AMPLITUDE ERROR'
               vale='0.1 0.2 0.4 0.6 0.9 1.1 2'
               valc='39 40 41 42 43 44 45 46'
            endif
            if(met=pher)
               metn='PHASE ERROR'
               vale='-0.8 -0.6 -0.4 -0.2 0.2 0.4 0.6 0.8'
               valc='20 21 22 23 24 25 26 27 28'
            endif

            say month' - 'plt''lt '   'metn

****AF****

            reg=af
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 9.0 9.3 1.5 7.0 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.13'
            'draw string 5.5 8.4 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 8.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 7.9 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****AA****

            reg=aa
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 9.3 9.7 1.5 7.0 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.13'
            'draw string 5.5 8.4 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 8.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 7.9 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****AU****

            reg=au
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 8.8 9.1 1.5 7.0 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.13'
            'draw string 5.5 8.4 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 8.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 7.9 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****EU****

            reg=eu
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 1.5 9.5 0.2 0.5 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 8.25 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 8.0 PRECIPITATION (1999-2018)'
            'draw string 5.5 7.75 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****AN****

            reg=an
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 10 10.3 1.5 7.0 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 8.4 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 8.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 7.9 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****PA****

            reg=pa
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 1.5 9.5 1.4 1.8 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 7.1 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 6.75 PRECIPITATION (1999-2018)'
            'draw string 5.5 6.4 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****NE****

            reg=ne
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 1.5 9.5 2.6 2.9 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 6.2 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 5.85 PRECIPITATION (1999-2018)'
            'draw string 5.5 5.5 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****SE****

            reg=se
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 1.5 9.5 2.6 2.9 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 6.2 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 5.85 PRECIPITATION (1999-2018)'
            'draw string 5.5 5.5 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****TR****

            reg=tr
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 1.5 9.5 2.9 3.2 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 5.8 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 5.45 PRECIPITATION (1999-2018)'
            'draw string 5.5 5.1 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****GL****

            reg=gl
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 1.5 9.5 1.1 1.4 -edge triangle -line on'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 7.5 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 7.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 6.8 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'
             'reset'
****AM****

            reg=am
            'set gxout shaded'
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 9.2 9.5 1.5 7.0 -edge triangle -line on'
    
            'set string 1 c 14 0'
            'set strsiz 0.13'
            'draw string 5.5 8.4 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 8.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 7.9 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
 
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

****AS****

            reg=as
            'set gxout shaded'
            ''tools''reg
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            ''tools'xcbar 8.0 8.3 1.5 7.0 -edge triangle -line on'
    
            'set string 1 c 14 0'
            'set strsiz 0.13'
            'draw string 5.5 8.4 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 8.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 7.9 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'

             'c'
 
             '!convert -density 300 -trim 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png 'out'/bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'
       
             meti=meti+1
         endwhile
         
         'close 2'
         'close 1'
         lt=lt+1
      endwhile
      
      monthi=monthi+1
   endwhile
      
   incrproi=incrproi+1
endwhile
   
