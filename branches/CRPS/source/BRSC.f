c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      BRSC(fst,m,obs,clm,ib,bspec,sp,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Brier Score                       
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      clm   -- climatological data
c      ib    -- climatological data dimension (equally-a-likely-bin)
c      bspec -- sample climatological frequence
c      sp    -- Brier Score
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: fapb   
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine brsc(fst,m,obs,clm,ib,bspec,sp,irt)
      dimension fst(m),clm(ib+1) 
      dimension sp(ib,2),fcp(ib),anp(ib)
      
      call fapb(fst,m,obs,clm,ib,fcp,anp,irt)
c     print *, 'bspec=',bspec
c     write (*,'(20f3.0)') fcp,anp

      irt=0
      sp=0.0
      do nb = 1, ib

       if (anp(nb).ne.0.0) then
        ei = 1.0
       else
        ei = 0.0
       endif
      
       sf = (ei-fcp(nb)/float(m))**2
       if (ib.ge.2) then
        sc = (ei-1.0/float(ib))**2
       elseif (ib.eq.1) then
        sc = (ei-bspec)**2
       else
        sc = (ei-bspec)**2
       endif
       sp(nb,1) = sp(nb,1) + sf
       sp(nb,2) = sp(nb,2) + sc
      enddo     ! nb

      return   
      end
