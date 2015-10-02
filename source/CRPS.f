c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      CRPS(fst,m,obs,ccrps,ictl,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Continuous Ranked Probability Score
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble size   
c      obs   -- analysis or observation, or climatology (truth)                
c      ccrps -- crp scores (local normalize score: 0 - 1.0)
c                          (standard score: 0 - infinite  )
c      ictl  -- =1 (make local normalize score)              
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: sortmm
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine crps(fst,m,obs,ccrps,ictl,irt)
      dimension fst(m),ena(m,3)
      
      irt=0
      k=0
      ta=0.0
      ccrps=0.0
      if (m.le.0) goto 999
C----------------------------------------------
C
CACC  IF ENSEMBLE SIZE IS 1 (ONLY ONE MEMBER!)
C
      if (m.eq.1) then
         ta=abs(fst(1)-obs)
         tds=ta
         goto 899
      endif
C----------------------------------------------
C
CACC  ena(NMembers,3) CONTAINS THE SORTED VALUES PREDICTED
C     BY THE ENSEMBLE (FROM LOW TO HIGH)
C
      do n = 1, m
         ena(n,1)=fst(n)
      enddo
c     re-arrange data from low to high
      call sortmm(ena,m,3,1)


CACC  SORTING WAS CHECKED, IT IS OKAY
CACC  ena(i,1) is the original input data
CACC  ena(i,2) is the sorted data, from lowest to highest
CACC  ena(i,3) is the rank/position of the input data
    
CACC  write(6,"(A,23F9.4)")"ena 1", (ena(i,1),i=1,m)
CACC  write(6,"(A,23F9.4)")"ena 2", (ena(i,2),i=1,m)
CACC  write(6,"(A,23F9.4)")"ena 3", (ena(i,3),i=1,m)

C----------------------------------------------
C
C    IDENTIFIES THE MEMBER FOR WHICH THE OBSERVED 
C    VALUE IS IMMEDIATELY LOWER THAN THE PREDICTED
C    VALUES. THIS MEMBER IS STORED IN k
C
      do n = 1, m
         if (obs.lt.ena(n,2)) goto 100
         k=n
      enddo
 100  continue
      if (m.gt.1) then
c        left end point (approximately)
         elhf=(ena(2,2)-ena(1,2))/2.0
         elend=ena(1,2)-elhf                    
c        right end point (approximately)
         erhf=(ena(m,2)-ena(m-1,2))/2.0
         erend=ena(m,2)+erhf                          
      else
         elend=ena(1,2)
         erend=ena(1,2)
      endif

      if (obs.gt.elend.and.obs.lt.erend) then
         tds=erend-elend                   
      else
         if (obs.lt.elend) then
            tds=erend-obs
         else
            tds=obs-elend
         endif
      endif

      IF (k.le.0) THEN
C     K LOWER OR EQUAL TO ZERO MEANS THAT THE OBSERVED VALUE IS LOWER
C     THAN THE LOWEST VALUE PREDICTED BY THE MODEL, i.e., IS AN OUTLIER 
C     THIS ALGORITM IS IN ACCORDANCE TO HERBACH(2000)
         do n = 1, m
            fac=1.0-float(n-1)/float(m)
            if (n.eq.1) then
               ds=ena(1,2)-obs              
               ta=ta+fac*fac*ds
            else
               ds=ena(n,2)-ena(n-1,2)
               ta=ta+fac*fac*ds
            endif
C       print *, 'k=',k,' ds=',ds,' ta=',ta,' obs=',obs
         enddo
      ELSEIF (k.ge.m) then
C     "K GREATER OR EQUAL TO NUMBER OF MEMBERS" MEANS THAT THE OBSERVED
C     VALUE IS HIGHER THAN THE HIGHEST VALUE PREDICTED BY THE MODEL,
C     i.e., IS AN OUTLIER. THIS ALGORITM IS IN ACCORDANCE TO HERBACH(2000)
         do n = 1, m
            fac=float(n)/float(m)
            if (n.eq.m) then
               ds=obs-ena(m,2)
               ta=ta+fac*fac*ds
            else
               ds=ena(n+1,2)-ena(n,2)
               ta=ta+fac*fac*ds
            endif
C            print *, 'k=',k,' ds=',ds,' ta=',ta,' obs=',obs
         enddo
      ELSE
         do n = 1, k
            fac=float(n)/float(m)
            if (n.ne.k) then
               ds=ena(n+1,2)-ena(n,2)
               ta=ta+fac*fac*ds
            else
               ds=obs-ena(n,2)
               ta=ta+fac*fac*ds
            endif
C            print *,'n=',n,'k=',k,' ds=',ds,' ta=',ta,
C     &       ' obs=',obs,'ena=',ena(n,k)
         enddo
         do n = k+1, m
            fac=1.0-float(n-1)/float(m)
            if (n.ne.k+1) then
               ds=ena(n,2)-ena(n-1,2)
               ta=ta+fac*fac*ds
            else
               ds=ena(n,2)-obs
               ta=ta+fac*fac*ds
            endif
C            print *,'n=',n,'k=',k,' ds=',ds,' ta=',ta,
C     &       ' obs=',obs,'ena=',ena(n,k)
         enddo
      ENDIF
 899  continue
      IF (tds.ne.0.0.and.ictl.eq.1) THEN
         ta=ta/tds
      ENDIF
      ccrps=ta
C      print *, ccrps,obs,(ena(i,2),i=1,m)
CACC      print *, ccrps
      return   
 999  print *, "ensemble size is less than one, crps=0, quit"
      irt=10
      return
      end
