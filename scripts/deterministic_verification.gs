'reinit'

n=80
i=1
var='prec'
***************PARAMETROS QUE DEPENDEM DO PRODUTO***************
prol='WEEK0 FORT0 3WKS0 MNTH0'
incrproi=4
incrprof=4
****************************************************************
***************PARAMETROS QUE DEPENDEM DO MES DE AVALIACAO***************
monthl='JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
*monthi ALTERAR LINHA 40
*monthf ALTERAR LINHA 41
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
   monthi=6
   monthf=6
   while(monthi<=monthf)
   
      month=subwrd(monthl,monthi)
   
      lt=1
      while(lt<=ltf)

         'open BAM12_'month'_'plt''lt'.ctl'
         'open GPCP_'month'_'plt''lt'.ctl'
         'set display color white'
         'c'

         'set gxout shaded'
         'set font 1'
         'set map 1 1 3'
         'set display color white'
         'set mproj latlon'
         'set grads off'
         'set lat -90 90'
         'set lon -180 180'

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
*        ''tools'cmrgb.gs'
         metl='corr rmse pher aper cnbs unbs msss'
         meti=1
         metf=7
         ret=palette()
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

            'set map 1 1 5'
            'set lat -70 70'
            'set lon -180 180'
            'set xlint 60'
            'set ylint 30'
            'set mpdset mres'
            'set mpt 2 off'
            'set xlopts 1 0.5 0.15'
            'set ylopts 1 0.5 0.15'
            'set grads off'
            'set clevs 'vale
            'set ccols 'valc   
            'd smth9('met')'
            'cbarn'

            'set string 1 c 14 0'
            'set strsiz 0.15'
            'draw string 5.5 7.5 'metn' BETWEEN FORECAST AND OBS. ANOMALIES'
            'draw string 5.5 7.15 PRECIPITATION (1999-2018)'
            'draw string 5.5 6.8 ISSUED: 'month'    VALID FOR 'plt''lt''
            'printim bam12_'met'_anomaly_'var'_'pro'0'lt'_'month'_'reg'.png'
            'c'

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

function palette()
*****corr-20-28*****
'set rgb 20 000 000 085 255'
'set rgb 21 000 000 255 255'
'set rgb 22 085 085 255 255'
'set rgb 23 170 170 255 255'
'set rgb 24 255 255 255 255'
'set rgb 25 255 204 068 255'
'set rgb 26 255 128 017 255'
'set rgb 27 255 000 000 255'
'set rgb 28 085 000 000 255'
****************************
*****rmse-29-38*****
'set rgb 29 225 255 255 255'
'set rgb 30 150 210 250 255'
'set rgb 31 080 165 245 255'
'set rgb 32 040 130 240 255'
'set rgb 33 020 100 210 255'
'set rgb 34 255 255 170 255'
'set rgb 35 255 192 060 255'
'set rgb 36 255 096 000 255'
'set rgb 37 225 020 000 255'
'set rgb 38 165 000 000 255'
****************************
*****cnbs-39-46*****
'set rgb 39 052 100 179 255'
'set rgb 40 078 139 201 255'
'set rgb 41 112 176 227 255'
'set rgb 42 138 203 236 255'
'set rgb 43 171 221 232 255'
'set rgb 44 255 255 255 255'
'set rgb 45 253 215 123 255'
'set rgb 46 241 156 040 255'
****************************
*****-msss-47-54*****
'set rgb 47 138 207 242 255'
'set rgb 48 255 255 255 255'
'set rgb 49 248 245 168 255'
'set rgb 50 255 215 123 255'
'set rgb 51 248 156 045 255'
'set rgb 52 242 104 041 255'
'set rgb 53 234 038 039 255'
'set rgb 54 161 032 036 255'
****************************
*****-unbc-55-57*****
'set rgb 55 241 156 040 255'
'set rgb 56 255 255 255 255'
'set rgb 57 161 032 036 255'
****************************

