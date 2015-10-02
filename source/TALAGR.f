      subroutine talagr(n,m,en,ve,ital,irel,ave,xmed,sprd,rmsa)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                            C
C     USAGE: TO CALCULATE TALAGRAND HISTOGRAM FOR ENSEMBLE FORECASTS         C
C            ON ONE GRID POINT                                               C
C     CODE : F77 on IBMSP --- Yuejian Zhu (07/26/2004)                       C
C                                                                            C
C     INPUT: n    number of ensember forecasts                               C
C            m    number of ensember forecasts to be verify                  C
C            en   vector of n containning forecasts at gridpoint             C
C            ve   value of verification at gridpoint ( analysis )            C
C                                                                            C
C     OUTPUT: ital vector of n+1 containing zeroes except 1 for              C
C                  bin containing truth                                      C
C             irel vector of n containning the relative position             C
C                  between analysis and forecasts                            C
C             ave  average of ensemble fcsts                                 C
C             xmed median of ensemble fcsts                                  C
C             sprd spread of ensemble fcsts                                  C
C             rmsa root mean square error for analysis and mean (ave)        C
C                                                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c--------+----------+----------+----------+----------+----------+----------+--
      dimension en(n),em(m),enb(m,2),ena(m,3)
      dimension ital(m+1),irel(m)
      irel = 0
      ital = 0
      ave  = 0.0      
      xmed = 0.0             
      sprd = 0.0        
      rmsa = 0.0        
      do i = 1, m
       if (en(i).eq.-9999.99) goto 999
       em(i) = en(i)
      enddo
      
      do i=1,m
       enb(i,1)=i
       enb(i,2)=em(i)
       ena(i,1)=i
       ena(i,2)=em(i)
       ital(i)=0
c ----
c calculate the average
c ----
       ave=ave+em(i)/float(m)
      enddo
c ----
c to calculate the spread
c ----
      do i=1,m
       sprd=sprd+(em(i)-ave)*(em(i)-ave)
      enddo
c ----
c to calculate the root mean square error for analysis and ensemble ave
c ----
      rmsa=(ve-ave)*(ve-ave)
c ----
c to order data
c ----
      call sortmm(ena,m,3,2)
c ----
c get relative position for analysis
c ----
      do i = 1, m
       if (ve.le.ena(i,2)) then
        if (i.eq.1) then
         iii=ena(i,3)
         irel(iii) = 2
        else
         iii=ena(i,3)
         jjj=ena(i-1,3)
         irel(iii) = 1
         irel(jjj) = 1
        endif
        goto 100
       endif
      enddo
      iii=ena(m,3)
      irel(iii) = 2
 100  continue
c ----
c to calculate the talagrand histogram
c ----
      do i=1,m
       if(ve.le.ena(i,2)) then
        ital(i)=1
        goto 200
       endif
      enddo
      ital(m+1)=1
 200  continue
c ----
c to calculate the median
c ----
      im=m/2
      if(mod(m,2).eq.1) then
       xmed=ena(im+1,2)
      else
       xmed=(ena(im,2)+ena(im+1,2))/2.
      endif
 999  continue

      return
      end

