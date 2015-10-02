c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      RRPS(fst,m,obs,clm,ib,bspec,rpsf,rpsc,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Ranked Probability Score          
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      ib    -- climatological equally-a-likely-bin
c      bspec -- sample climatological frequence
c      rpsf  -- ranked probability scores for forecast
c      rpsc  -- ranked probability scores for climatology
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: fapb  
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine rrps(fst,m,obs,clm,ib,bspec,rpsf,rpsc,irt)
      dimension fst(m),clm(ib+1) 
      dimension fcp(ib),anp(ib)
      
      call fapb(fst,m,obs,clm,ib,fcp,anp,irt)

      irt=0
      rpsf   = .0
      rpsc   = .0
      do nb = 1, ib
       sfcbs = .0
       sfcbc = .0
       sanbs = .0
       do ii = 1, nb
        fcbs  = fcp(ii)
        fcbc  = bspec
        anbs  = anp(ii)
        sfcbs = sfcbs+(fcbs/float(m))
        sfcbc = sfcbc+fcbc
        sanbs = sanbs+anbs
       enddo
       rpsf = rpsf+(sfcbs-sanbs)**2
       rpsc = rpsc+(sfcbc-sanbs)**2
      enddo
      if (ib.ge.2) then
       rpsf  = 1.0-(1.0/float(ib-1))*rpsf
       rpsc  = 1.0-(1.0/float(ib-1))*rpsc
      elseif (ib.eq.1) then
c --- try this way for one specification??? (no ranked)
       rpsf  = 1.0 - rpsf
       rpsc  = 1.0 - rpsc
      else
c --- try this way, (no ranked)
       rpsf  = 1.0 - rpsf
       rpsc  = 1.0 - rpsc
      endif

      return   
      end
