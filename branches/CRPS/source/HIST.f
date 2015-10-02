c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      HIST(fst,m,obs,chist,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete histgram distribution              
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      chist -- hist position ( range: 1 - m+1 )
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: sortmm
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine hist(fst,m,obs,chist,irt)
      dimension fst(m),ena(m,3)
      
      irt=0
      chist=float(m+1)
      do n = 1, m
       ena(n,1)=fst(n)
      enddo
      call sortmm(ena,m,3,2)
      do n = 1, m
       if (obs.le.ena(n,3)) then
        chist=float(n)
        return
       endif
      enddo
      return   
      end
