c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      HRFA(fst,m,obs,clm,ib,fyy,fyn,irt)
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
c      fyy   -- hit rate
c      fyn   -- false alarm rate
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: fapb  
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine hrfa(fst,m,obs,clm,ib,yy,yn,irt)
      dimension fst(m),clm(ib+1) 
      dimension yy(m+1),yn(m+1)
      dimension fcp(ib),anp(ib)
      
      call fapb(fst,m,obs,clm,ib,fcp,anp,irt)

      irt=0
      yy=0.0
      yn=0.0
Ccccc question???
ccccc No question: fyy(1) is representing zero probability
ccccc if fyy(1) = 1  means miss
ccccc if fyn(1) = 1  means correct rejection
ccccc if fyy(2) = 1  means hit for 10% probability ( 1 of 10 members )
ccccc if fyn(2) = 1  means false alarm for 10% probability
ccccc if fyy(3) = 1  means hit for 20% probability ( 2 of 10 members )
ccccc if fyn(3) = 1  means false alarm for 20% probability
ccccc ......

      do nb = 1, ib
       do np = 1, m+1  
        if (fcp(nb).eq.float(np-1)) then
         if (anp(nb).eq.1.) then
          yy(np) = yy(np)+1.0
         else
          yn(np) = yn(np)+1.0
         endif
        endif
       enddo
      enddo          ! nb

      return   
      end
