!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Subroutine      CRPS(fst,m,obs,ccrps,ictl,irt)
!  Programmed by: Yuejian Zhu           Org: np23          Date: 2006-12-26
!
!
!  This is the subroutine to calculete Continuous Ranked Probability Score
!         for NAEFS project on IBM-SP
!
!   parameters:
!      fst   -- forecasts for m ensemble members
!      m     -- ensemble size   
!      obs   -- analysis or observation, or climatology (truth)                
!      ccrps -- crp scores (local normalize score: 0 - 1.0)
!                          (standard score: 0 - infinite  )
!      ictl  -- =1 (make local normalize score)              
!      irt   -- 0-successful
!               10-need to review ensemble forecast data
!
!   Called subroutine: sortmm
!
!--------+---------+---------+---------+---------+----------+---------+--
subroutine crps(fst,m,obs,ccrps,ictl,irt) 
	
	REAL :: fst(m),ena(m,3)

	irt=0
      	k=0
      	ta=0.0
      	ccrps=0.0

      	if (m.le.0) then
           go to 999
	end if
!------------------------------------------------------
!
!CACC IF ENSEMBLE SIZE IS 1 (ONLY ONE MEMBER!)
!
      if (m.eq.1) then
         ta=abs(fst(1)-obs)
         tds=ta
         goto 899
      end if
!------------------------------------------------------
!------------------------------------------------------
!
!CACC  ena(NMembers,3) CONTAINS THE VALUES PREDICTED
!CACC  BY THE ENSEMBLE SORTED FROM LOWER TO HIGHER
!
      do n = 1, m
         ena(n,1)=fst(n)
      end do
      call sortmm(ena,m,3,1)

!CACC SORTING WAS CHECKED, IT IS OKAY!
!CACC ena(:,1) is the original input data
!CACC ena(:,2) is the sorted data, from lowest to highest
!CACC ena(:,3) is the rank/position of the input data
    
!CACC write(6,"(A,23F9.4)")"ena 1", (ena(i,1),i=1,m)
!CACC write(6,"(A,23F9.4)")"ena 2", (ena(i,2),i=1,m)
!CACC write(6,"(A,23F9.4)")"ena 3", (ena(i,3),i=1,m)

      ! Compares the observed value with each forecasted value
      !
      do n = 1, m
         if (obs.lt.ena(n,2)) then 
	    goto 100
         end if
      k=n
      end do
  100 continue
	
      if (m.gt.1) then
         elhf=(ena(2,2)-ena(1,2))/2.0
         elend=ena(1,2)-elhf
         erhf=(ena(m,2)-ena(m-1,2))/2.0
         erend=ena(m,2)+erhf
      else
         elend=ena(1,2)
         erend=ena(1,2)
      end if

      if (obs.gt.elend.and.obs.lt.erend) then
         	tds=erend-elend
      else
        	if (obs.lt.elend) then
            		tds=erend-obs
        	else
            		tds=obs-elend
         	end if
      end if
	
	IF (k.le.0) THEN
        	do n = 1, m
            		fac=1.0-float(n-1)/float(m)
            		if (n.eq.1) then
               			ds=ena(1,2)-obs
               			ta=ta+fac*fac*ds
            		else
               			ds=ena(n,2)-ena(n-1,2)
               			ta=ta+fac*fac*ds
            		end if
         	end do
	ELSE IF (k.ge.m) then
		do n = 1, m
            		fac=float(n)/float(m)
            		if (n.eq.m) then
               			ds=obs-ena(m,2)
               			ta=ta+fac*fac*ds
            		else
               			ds=ena(n+1,2)-ena(n,2)
               			ta=ta+fac*fac*ds
            		end if
         	end do
	ELSE
         	do n = 1, k
            		fac=float(n)/float(m)
            		if (n.ne.k) then
               			ds=ena(n+1,2)-ena(n,2)
               			ta=ta+fac*fac*ds
            		else
               			ds=obs-ena(n,2)
               			ta=ta+fac*fac*ds
            		end if
         	end do
         	do n = k+1, m
            		fac=1.0-float(n-1)/float(m)
            		if (n.ne.k+1) then
               			ds=ena(n,2)-ena(n-1,2)
               			ta=ta+fac*fac*ds
            		else
               			ds=ena(n,2)-obs
               			ta=ta+fac*fac*ds
            		end if
         	end do
	END IF

	899  continue
	
      	IF (tds.ne.0.0.and.ictl.eq.1) THEN
		ta=ta/tds
      	END IF

      	ccrps=ta
      	return

 	999  print *, "ensemble size is less than one, crps=0, quit"

      	irt=10
      	return

end subroutine crps
