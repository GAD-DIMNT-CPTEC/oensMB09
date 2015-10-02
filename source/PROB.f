c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      PROB(fcst,fanl,clim,wght,len,ib,im,cscrf,anldate,fctlag)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2004-08-01
c
c This is main ensemble based probabilistic verification sub-program
c         for NAEFS project on IBM-SP
c
c  For:   1. Reliability for m+1 probabilities ( m=members )
c         2. Ranked probability score ( and skill scores)
c         3. Brier score ( and Brier skill score )
c         4. Hit rate and false alarm rate
c         5. Resolution and reliability (option)
c         6. Economic values (option)
c         7. Information content (option)
c
c   subroutines                                                    
c              EVALUE ---> to calculate the economic value
c   
c   parameters:
c      len   -- total grid points/observations
c      im    -- ensemble members
c      ib    -- ib>=2: climatologically equally-a-likely-bin 
c               ib=1: climatological based, specified range consideration
c               ib=-1: no climatology, specified value consideration
c      scrf  -- sample climatological relative frequence
c
c   Fortran 77 on IBMSP
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine prob(fcst,fanl,clim,wght,len,ib,im,cscrf,anldate,
     *fctlag)
      INTEGER LEN
CACC  parameter (mxlen=len)
      INTEGER IB
CACC  dimension fcst(mxlen,im),fanl(mxlen),wght(mxlen)
      dimension fcst(LEN,im),fanl(LEN),wght(LEN)
CACC  REAL CRPSFARR(MXLEN),CRPSCARR(MXLEN)
      REAL CRPSFARR(LEN),CRPSCARR(LEN)
      REAL CRPSFVAL,CRPSCVAL
CACC  REAL clim(mxlen,ib+1)
      REAL clim(LEN,ib+1)
      REAL cl(ib+1)
      dimension fcp(ib),anp(ib)
      dimension sfcp(im+1),sanp(im+1),fp(im+1)
      dimension bsf(ib+1),bsc(ib+1),bss(ib+1)
      dimension fyy(im+1),fyn(im+1)
      dimension hra(im+1),fal(im+1)
      dimension fhk(ib)
      dimension fv(18)             ! 18 ratios has been set by subroutine
      dimension rp(im+1,2)
      dimension sp(ib,2),yy(im+1),yn(im+1)
      dimension fs(im),clrps(ib-1)
      character anldate*10,fctlag*4
      INTEGER IDL
      
      common /vscore/ infow(500),probs(500),dists(500)             

      write(*,444) "PROB: Total number of gridpoints: ", len
      write(*,445) fanl(720)
      write(*,446) (fcst(720,n),n=1,im)
      write(*,447) (clim(720,n),n=1,ib+1)
 444  format (A,I10)
 445  format (F7.1)
 446  format (15(F7.1))
 447  format (23(F7.1))

      imp1=im+1
c ----
c     to creat climate based equally-likely-bin or specified values
c ----

      sanp = 0.0
      sfcp = 0.0
      bsf  = 0.0
      bsc  = 0.0
      rpfs = 0.0
      rpfc = 0.0
      fyy  = 0.0
      fyn  = 0.0
      crpsf= 0.0
CACC  CRPSC IS THE CONTINUOUS RANKED PROBABILITY SCORE FOR CLIMATOLOGY ?!
      crpsc= 0.0
      crpsn= 0.0
      crpss= 0.0
      bspec = cscrf 
c     print *, 'bspec=',bspec
      fhk  = 0.0
      ftl  = 0.0
c ------------------------------------------------------------------
c
c BEGIN DO NXY
c
      do nxy = 1, len  

       do i = 1, im
          fs(i) = fcst(nxy,i)
       enddo
       fa=fanl(nxy)
       do i = 1, ib+1
          cl(i) = clim(nxy,i)
CACC      print*,'PROB.f: cl(',i,')=',cl(i)
       enddo
       wfac = wght(nxy)

c      print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '        RELIABILITY DIAGRAM '
c      print *, '++++++++++++++++++++++++++++++++++++'

c       call reli(fs,im,fa,cl,ib,rp,irt)

       do np = 1, imp1
        sanp(np) = sanp(np) + rp(np,2)*wfac
        sfcp(np) = sfcp(np) + rp(np,1)*wfac
       enddo

c      print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '        BRIER SCORE        '
c      print *, '++++++++++++++++++++++++++++++++++++'

c       call brsc(fs,im,fa,cl,ib,bspec,sp,irt)

       do nb = 1, ib
        bsf(nb) = bsf(nb) + sp(nb,1)*wfac
        bsc(nb) = bsc(nb) + sp(nb,2)*wfac
       enddo

c      print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '       RANKED PROBABILITY SCORE    '
c      print *, '++++++++++++++++++++++++++++++++++++'

c       call rrps(fs,im,fa,cl,ib,bspec,rpsf,rpsc,irt)

       rpfs = rpfs + rpsf*wfac
       rpfc = rpfc + rpsc*wfac

c       print *, '++++++++++++++++++++++++++++++++++++'
c       print *, ' Continuous Ranked Probability Score'
c       print *, '++++++++++++++++++++++++++++++++++++'

c      fs=fs(NumberOfMembers)
c      im=NumberOfMembers
c      fa=Analysis/Observation gridpoint value

CACC   STANDARDIZED CRPS
CACC   call crps(fs,im,fa,crpsff,1,irt)

CACC   NON-STANDARDIZED CRPS
       call crps(fs,im,fa,crpsff,0,irt)
       crpsf = crpsf + crpsff*wfac
       CRPSFVAL=crpsff
       CRPSFARR(NXY)=CRPSFVAL

       call crps(fs,im,fa,crpsfn,1,irt)
       crpsn = crpsn + crpsfn*wfac

       if (ib.ne.1) then
CACC      do nb = 1, ib-1
CACC         clrps(nb)=cl(nb+1)
CACC      enddo
CACC      call crps(clrps,ib-1,fa,crpscc,0,irt)
          call crps(cl,ib+1,fa,crpscc,0,irt)
          CRPSCVAL=crpscc
       else
CACC      Number of bins (ib) = 1
CACC      the crps is approximately in this calculation
          clrps(1)=cl(1)
          clrps(2)=cl(2)
          call crps(clrps,2,fa,crpscc,0,irt)
       endif
       crpsc = crpsc + crpscc*wfac
       CRPSCARR(NXY)=CRPSCVAL

       crpss = crpss + wfac*(crpscc-crpsff)/crpscc

       PRINT *,nxy,'crpsc=',crpsc,' crpscc=',crpscc,'crpsff=',crpsff,
     *         'crpsf=',crpsf,' wfac=',wfac,' crpss=',crpss 

c      print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '  HIT RATE AND FALSE ALARM RATE     '
c      print *, '++++++++++++++++++++++++++++++++++++'

c       call hrfa(fs,im,fa,cl,ib,yy,yn,irt)

       do np = 1, imp1
        fyy(np) = fyy(np) + yy(np)*wfac
        fyn(np) = fyn(np) + yn(np)*wfac
       enddo

c     print *, '++++++++++++++++++++++++++++++++++++'
c     print *, '     HEIDKE SKILL SCORE if ib=3     '
c     print *, '++++++++++++++++++++++++++++++++++++'
 
       if (ib.eq.3) then
c        call fapb(fst,m,obs,clm,ib,fcp,anp,irt)
        ftl = ftl + im*wfac
        do nb = 1, ib
         do np = 1, imp1
          if (fcp(nb).eq.float(np-1)) then
           if (anp(nb).eq.1.) then
            fhk(nb) = fhk(nb)+(np-1)*wfac
           endif
          endif
         enddo
        enddo
       endif

      enddo
c
c END DO NXY
c
c ------------------------------------------------------------------

C     print *, '++++++++++++++++++++++++++++++++++++'
C     print *, '     RELIABILITY DIAGRAM OUTPUT     '
C     print *, '++++++++++++++++++++++++++++++++++++'

C
C     Brier scores could be calculated from reliability diagram
C
      do np = 1, imp1
       if (sfcp(np).gt.0.1E-10) then
        fp(np) = (sanp(np)/sfcp(np))*100.0
       else
        fp(np) = 0.0
       endif
      enddo

      do ii = 1, imp1
       probs(ii)      = sanp(ii)
       probs(ii+imp1) = sfcp(ii)
      enddo

c     write (*,801) (sanp(ii),ii=1,im+1)         
c     write (*,801) (sfcp(ii),ii=1,im+1)         
c     write (*,801) (fp(ii),ii=1,im+1)         

C
C     Option: use reliability diagram to calculate information content
C      only for ib > 1 ?
      csfcp = 0.0
      cinfm = 0.0
      do np = 2, imp1
       csfcp = csfcp + sfcp(np)
       if (fp(np).gt.0.0.and.fp(np).lt.100..and.ib.gt.1) then
        xrate1 = fp(np)/100.0
        xrate2 = (1.0-xrate1)/float(ib-1)
        xinf = xrate1*alog10(xrate1)/alog10(float(ib)) 
     .       + (ib-1.0)*xrate2*alog10(xrate2)/alog10(float(ib))
        cinfm = cinfm + xinf*sfcp(np)
c      else
c      how about fp=0.0?
c       xrate  = fp(np)/100.0
c       xinf = xrate*alog10(xrate)/alog10(float(ib))
c       cinfm = cinfm + xinf*sfcp(np)
       endif
      enddo
      cinfm = 1.0 + cinfm/csfcp

      probs(14+4*imp1)= cinfm

c     print *, "Information Content: ", cinfm 

C
C     Option: use reliability diagram to calculate rel, res, and unc
c
c     Calculate the BS = reliability - resolution + uncertainty
c     --- Reference: Dianiel S. Wilks book, chapter 7 of
c        <<Statistical Methods in the Atmospheric Science>>
c        p262, equation (7.28)
c        p246, table 7.3
c
      tfst  = 0.0
      do np = 1, imp1
       tfst = tfst + sfcp(np)
      enddo

      creli = 0.0
      creso = 0.0
      cunce = 0.0

      do np = 1, imp1
       if (sfcp(np).ne.0.0) then
        cdis  = (np-1.0)/(imp1-1.0)-sanp(np)/sfcp(np)   ! distance for reli
        creli = creli + sfcp(np)/tfst * cdis*cdis       ! weight=sfc/tfst
        cdis  = sanp(np)/sfcp(np) - cscrf               ! distance for reso
        creso = creso + sfcp(np)/tfst * cdis*cdis       ! weight=sfc/tfst
       endif
      enddo

      cunce = cscrf*(1.0 - cscrf)
      cbss  = (creso - creli)/cunce
      cbs   = creli - creso + cunce

      probs(10+4*imp1) = creli
      probs(11+4*imp1) = creso
      probs(12+4*imp1) = cunce
      probs(13+4*imp1) = cscrf

c     print *, 'Reliability =',creli
c     print *, 'Resolution  =',creso
c     print *, 'Uncetainty  =',cunce
c     print *, 'S. Cli. Rel.=',cscrf
c     print *, 'BS          =',cbs 
c     print *, 'BSS         =',cbss  

c     Notes for ib=1:
c     when using bspec for specified case
c     usually, cbss (from decomposition) is not equal to bss (from Brier scores)
c     the different comes from bspec (pre-defined, not true in fact)
c     if we use cscrf instead of bspec when calculate Brier score
c     Then, the result will be the same.
c     therefore, BSS (from decomposition) is the right one.
c        --- Yuejian Zhu (08/31/2004)

c     print *, '++++++++++++++++++++++++++++++++++++'
c     print *, '        BRIER SCORES OUTPUT         '
c     print *, '++++++++++++++++++++++++++++++++++++'

      bsfa = 0.0
      bsca = 0.0
      do nb = 1, ib
       bsf(nb) = bsf(nb)/float(len)
       bsfa    = bsfa+bsf(nb)
       bsc(nb) = bsc(nb)/float(len)
       bsca    = bsca+bsc(nb)
       if (bsc(nb).ne.0.0) then
        bss(nb) = (bsc(nb)-bsf(nb))/bsc(nb)
        if (bss(nb).le.-10.0) bss(nb)=-9.99
       endif
      enddo
      bsfa = bsfa/float(ib)
      bsca = bsca/float(ib)
      if (bsca.ne.0.0) then
       bssa = (bsca-bsfa)/bsca
       if (bssa.le.-10.0) bssa=-9.99
      endif
      bsf(ib+1) = bsfa
      bsc(ib+1) = bsca
      bss(ib+1) = bssa

      probs(7+4*imp1) = bsfa 
      probs(8+4*imp1) = bsca 
      probs(9+4*imp1) = bssa 
ccc   adding bss for 10% and 90% (extreme forecast)
      probs(16+4*imp1) = bss(1)
      probs(17+4*imp1) = bss(ib)

c     write (*,802) bsfa,bsca,bssa               

c     print *, 'BSf         =',bsfa  
c     print *, 'BSc         =',bsca  
c     print *, 'BSS         =',bssa  

c     print *, '++++++++++++++++++++++++++++++++++++'
c     print *, ' RANKED PROBABILITY SCORES OUTPUT   '
c     print *, '++++++++++++++++++++++++++++++++++++'

      rpfs = rpfs/float(len)
      rpfc = rpfc/float(len)
      rpss = (rpfs-rpfc)/(1.0-rpfc)

      probs(1+4*imp1) = rpfs
      probs(2+4*imp1) = rpfc
      probs(3+4*imp1) = rpss 

c     write (*,802) rpfs,rpfc,rpss             

c     print *, '++++++++++++++++++++++++++++++++++++'
c     print *, ' Continuous Ranked Probability Score'
c     print *, '++++++++++++++++++++++++++++++++++++'

      crpsf = crpsf/float(len)
      crpsc = crpsc/float(len)
      crpss = crpss/float(len)
      crpsn = crpsn/float(len)
      crpss0= (crpsf-crpsc)/(0.0-crpsc)


      OPEN(105,
     &FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'
     &//ANLDATE//'.txt',
     &FORM='FORMATTED',STATUS='UNKNOWN')
         WRITE(105,'(2F12.4)') crpsf,crpsc
      CLOSE(105)

      IDL=INDEX(FCTLAG,'h')-1
      OPEN(106,
     &FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'
     &//ANLDATE//'.grads',
     &FORM='UNFORMATTED',STATUS='UNKNOWN')
         WRITE(106) (CRPSFARR(I),I=1,LEN)
      CLOSE(106)

      OPEN(107,
     &FILE='../dataout/CRPSC4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'
     &//ANLDATE//'.grads',
     &FORM='UNFORMATTED',STATUS='UNKNOWN')
         WRITE(107) (CRPSCARR(I),I=1,LEN)
      CLOSE(107)

      OPEN(108,
     &FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'
     &//ANLDATE//'.aave.grads',
     &FORM='UNFORMATTED',STATUS='UNKNOWN')
         WRITE(108) crpsf
         WRITE(108) crpsc
      CLOSE(108)

      OPEN(109,
     &FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'
     &//ANLDATE//'.aave.ctl', FORM='FORMATTED', STATUS='UNKNOWN')

      WRITE(109,100) '^CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'
     &//ANLDATE//'.aave.grads'
      WRITE(109,105)
      WRITE(109,110)
      WRITE(109,115) 
      WRITE(109,120) 
      WRITE(109,125)
      WRITE(109,130)
      WRITE(109,140)
      WRITE(109,145)
      WRITE(109,150)
      WRITE(109,155)
      CLOSE(109)
 100  FORMAT ('DSET ', A)
 105  FORMAT ('UNDEF -9999.')
 110  FORMAT ('OPTIONS SEQUENTIAL')
 115  FORMAT ('XDEF 1 LINEAR 1.0 1.0')
 120  FORMAT ('YDEF 1 LINEAR 1.0 1.0')
 125  FORMAT ('ZDEF 1 LINEAR 1.0 1.0')
 130  FORMAT ('TDEF 1 LINEAR 1JAN1950 1YR')
 140  FORMAT ('VARS 2')
 145  FORMAT ('CRPSF 0 99 CRPS for FORECAST [THE LOWER THE BETTER]')
 150  FORMAT ('CRPSC 0 99 CRPS for CLIMATOLOGY [THE LOWER THE BETTER]')
 155  FORMAT ('ENDVARS')

      probs(4+4*imp1) = crpsf
      probs(5+4*imp1) = crpsc
c     probs(6+4*imp1) = crpss 
c     probs(4+4*imp1) = crpsn
c     probs(5+4*imp1) = crpss
      probs(6+4*imp1) = crpss0 

      write (*,'(A,F12.4)') "CRPSF: ",crpsf 
      write (*,'(A,F12.4)') "CRPSS: ",crpss 
      write (*,'(A,F12.4)') "CRPSC: ",crpsc 
      write (*,'(A,F12.4)') "CRPSN: ",crpsn 

      end
