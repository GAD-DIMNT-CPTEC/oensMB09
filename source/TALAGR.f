      subroutine talagr(n,m,en,ve,ital,irel,ave,xmed,sprd,
     *rmsa,anldate,fctlag)
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
      character anldate*10,fctlag*4

      integer jj, sumital !CFB: jj e um contador; 
!                               sumital e o valor da soma de todos os "1s" para cada coluna de ital
      real rank

      irel = 0
      ital = 0
      ave  = 0.0      
      xmed = 0.0             
      sprd = 0.0        
      rmsa = 0.0        
!CFB
      sumital = 0
!CFB
      do i = 1, m
       if (en(i).eq.-9999.99) goto 999
       em(i) = en(i)
      enddo
      
      do i=1,m
       enb(i,1)=i
       enb(i,2)=em(i)
       ena(i,1)=i
       ena(i,2)=em(i)
       ital(i)=0 !CFB: o array ital inteiro e inicializado com zeros
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
! CFB
! Calcular o histograma = quantificar quantas vezes a referencia esta dentro
! de cada bin;
! No final, a matrix ital contera 16 colunas (bins) (para o caso do SCON global do CPTEC
! com 15 membros);
! Para cada bin, havera 11520 valores que podem ser somados e a partir
! dos quais, a frequencia relativa podera ser calculada;
! O que pode ser plotado no Talagrand histogram,
! sao as frequencias relativas em cada bin.
! CFB
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

!CFB
      open(118,
     &file='../rankhist/RANKHIST4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'
     &//ANLDATE//'.txt',
     &FORM='FORMATTED',STATUS='UNKNOWN')
      write(118,'(16I3)') ital
!CFB

 999  continue

      return
      end
