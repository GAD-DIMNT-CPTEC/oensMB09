c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      RELI(fst,m,obs,clm,ib,rp,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Contengent Rank Probability Scores
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      clm   -- climatological data
c      ib    -- climatological data dimension (equally-a-likely-bin)
c      rp    -- reliability (self-category: m+1) 
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine reli(fst,m,obs,clm,ib,rp,irt)
      dimension fst(m),clm(ib+1),rp(m+1,2) 
      
c     print *, 'm=',m,' ib=',ib
c     write (*,'(26f6.0)') (fst(i),i=1,m),obs,(clm(i),i=1,ib+1)
      irt=0
      rp=0.0
      icnt=0
      if (ib.eq.1) then
       do n = 1, m
        if (fst(n).gt.clm(1)) then
         icnt=icnt+1          
        endif
       enddo
       rp(icnt+1,1)=1.0
       if (obs.gt.clm(1)) then
        rp(icnt+1,2)=1.0
       endif
      elseif (ib.eq.-1) then
       do n = 1, m
        if (fst(n).le.clm(1)) then
         icnt=icnt+1
        endif
       enddo
       rp(icnt+1,1)=1.0
       if (obs.gt.clm(1)) then
        rp(icnt+1,2)=1.0
       endif
      elseif (ib.eq.0) then
ccc if ib=0, clm(1) is the mean of climatology
       do n = 1, m
        if (fst(n).gt.clm(1)) then
         icnt=icnt+1
        endif
       enddo
       rp(icnt+1,1)=1.0
       if (obs.gt.clm(1)) then
        rp(icnt+1,2)=1.0
       endif
      elseif (ib.ge.2) then
ccc for example: ib=2 2 equally-a-likely-bin
       icnt=0
       do n = 1, m
        if (fst(n).le.clm(2)) then
         icnt=icnt+1
        endif
       enddo
c      rp(icnt+1,1)=rp(icnt+1,1)+1.0/float(ib+1)
       rp(icnt+1,1)=rp(icnt+1,1)+1.0
       if (obs.le.clm(2)) then
c       rp(icnt+1,2)=rp(icnt+1,2)+1.0/float(ib+1)
        rp(icnt+1,2)=rp(icnt+1,2)+1.0
       endif

       icnt=0
       do n = 1, m
        if (fst(n).gt.clm(ib)) then
         icnt=icnt+1
        endif
       enddo
c      rp(icnt+1,1)=rp(icnt+1,1)+1.0/float(ib+1)
       rp(icnt+1,1)=rp(icnt+1,1)+1.0
       if (obs.gt.clm(ib)) then
c       rp(icnt+1,2)=rp(icnt+1,2)+1.0/float(ib+1)
        rp(icnt+1,2)=rp(icnt+1,2)+1.0
       endif

       if (ib.gt.2) then
       do i = 2, ib-1
        icnt=0
        do n = 1, m
         if (fst(n).gt.clm(i).and.fst(n).le.clm(i+1)) then
          icnt=icnt+1
         endif
        enddo
c       rp(icnt+1,1)=rp(icnt+1,1)+1.0/float(ib+1)
        rp(icnt+1,1)=rp(icnt+1,1)+1.0
        if (obs.gt.clm(i).and.obs.le.clm(i+1)) then
c        rp(icnt+1,2)=rp(icnt+1,2)+1.0/float(ib+1)
         rp(icnt+1,2)=rp(icnt+1,2)+1.0
        endif
       enddo
       endif
      else
       irt=99
      endif

      return   
      end
