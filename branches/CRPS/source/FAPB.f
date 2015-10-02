c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      FAPB(fst,m,obs,clm,ib,fcp,anp,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete forecast and analysis probability 
c  of special valus, or equally-a-likely-bin for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      clm   -- special values, or equally-a-likely-bin (ib+1)
c      rp    -- probabilities of each special value, or equally-a-likely-bin
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine fapb(fst,m,obs,clm,ib,fcp,anp,irt)
      dimension fst(m),clm(ib+1),fcp(ib),anp(ib)
      
c     write (*,'(26f6.0)') (fst(i),i=1,m),obs,(clm(i),i=1,ib+1)

      fcp=0.0
      anp=0.0
      irt=0
      icnt=0
      if (ib.eq.1) then
       do n = 1, m
        if (fst(n).gt.clm(1)) then
         icnt=icnt+1          
        endif
       enddo
       fcp(1)=float(icnt)
       if (obs.gt.clm(1)) then
        anp(1)=1.0
       endif
c     elseif (ib.eq.-1) then
c      do n = 1, m
c       if (fst(n).le.clm(1)) then
c        icnt=icnt+1
c       endif
c      enddo
c      rp(1,1)=icnt
c      if (obs.gt.clm(1)) then
c       rp(1,2)=1.0
c      endif
c     elseif (ib.eq.0) then
ccc if ib=0, clm(1) is the mean of climatology
c      do n = 1, m
c       if (fst(n).gt.clm(1)) then
c        icnt=icnt+1
c       endif
c      enddo
c      rp(1,1)=icnt
c      if (obs.gt.clm(1)) then
c       rp(1,2)=1.0
c      endif
      elseif (ib.ge.2) then
ccc for example: ib=2 2 equally-a-likely-bin
       icnt=0
       do n = 1, m
        if (fst(n).le.clm(2)) then
         icnt=icnt+1
        endif
       enddo
       fcp(1)=float(icnt)      
       if (obs.le.clm(2)) then
        anp(1)=1.0
       endif

       icnt=0
       do n = 1, m
        if (fst(n).gt.clm(ib)) then
         icnt=icnt+1
        endif
       enddo
       fcp(ib)=float(icnt)       
       if (obs.gt.clm(ib)) then
        anp(ib)=1.0
       endif

       if (ib.gt.2) then
       do i = 2, ib-1
        icnt=0
        do n = 1, m
         if (fst(n).gt.clm(i).and.fst(n).le.clm(i+1)) then
          icnt=icnt+1
         endif
        enddo
        fcp(i)=float(icnt)      
        if (obs.gt.clm(i).and.obs.le.clm(i+1)) then
         anp(i)=1.0
        endif
       enddo
       endif
      else
       irt=99
      endif

      return   
      end
