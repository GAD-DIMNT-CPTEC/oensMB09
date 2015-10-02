subroutine prob(fcst,fanl,clim,wght,len,ib,im,cscrf,anldate,fctlag)

	
	INTEGER :: len,ib,im
	REAL :: CRPSFARR(len),CRPSCARR(len),CRPSFVAL,CRPSCVAL,clim(len,ib+1),cl(ib+1)
	CHARACTER(len=4) :: fctlag		
	CHARACTER(len=10) :: anldate		
	REAL :: fcst(len,im),fanl(LEN),wght(LEN)
      	REAL :: fcp(ib),anp(ib)
      	REAL :: sfcp(im+1),sanp(im+1),fp(im+1)
      	REAL :: bsf(ib+1),bsc(ib+1),bss(ib+1)
      	REAL :: fyy(im+1),fyn(im+1)
      	REAL :: hra(im+1),fal(im+1)
      	REAL :: fhk(ib)
      	REAL :: fv(18)             ! 18 ratios has been set by subroutine
      	REAL :: rp(im+1,2)
      	REAL :: sp(ib,2),yy(im+1),yn(im+1)
      	REAL :: fs(im),clrps(ib-1)

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
	sanp = 0.0
      	sfcp = 0.0
      	bsf  = 0.0
      	bsc  = 0.0
      	rpfs = 0.0
      	rpfc = 0.0
      	fyy  = 0.0
      	fyn  = 0.0
      	crpsf= 0.0
	crpsc= 0.0
      	crpsn= 0.0
      	crpss= 0.0
      	bspec = cscrf
	fhk  = 0.0
      	ftl  = 0.0

	! Begin do NXY
	do nxy = 1, len
       		do i = 1, im
          		fs(i) = fcst(nxy,i)
       		end do
       		fa=fanl(nxy)
       		do i = 1, ib+1
         	 cl(i) = clim(nxy,i)
       		end do
       		wfac = wght(nxy)

		!RELIABILITY DIAGRAM 
      		do np = 1, imp1
        		sanp(np) = sanp(np) + rp(np,2)*wfac
        		sfcp(np) = sfcp(np) + rp(np,1)*wfac
       		end do

    		!BRIER SCORE       
       		do nb = 1, ib
        		bsf(nb) = bsf(nb) + sp(nb,1)*wfac
        		bsc(nb) = bsc(nb) + sp(nb,2)*wfac
       		end do

		!RANKED PROBABILITY SCORE    
       		rpfs = rpfs + rpsf*wfac
       		rpfc = rpfc + rpsc*wfac

		!Continuous Ranked Probability Score

     		!fs=fs(NumberOfMembers)
     		!im=NumberOfMembers
     		!fa=Analysis/Observation gridpoint value

		!NON-STANDARDIZED CRPS

	        call crps(fs,im,fa,crpsff,0,irt)
       		crpsf = crpsf + crpsff*wfac
       		CRPSFVAL=crpsff
       		CRPSFARR(NXY)=CRPSFVAL
       		call crps(fs,im,fa,crpsfn,1,irt)
       		crpsn = crpsn + crpsfn*wfac

       		if (ib.ne.1) then
          		call crps(cl,ib+1,fa,crpscc,0,irt)
          		CRPSCVAL=crpscc
       		else
          		clrps(1)=cl(1)
          		clrps(2)=cl(2)
          		call crps(clrps,2,fa,crpscc,0,irt)
       		end if
	
       		crpsc = crpsc + crpscc*wfac
       		CRPSCARR(NXY)=CRPSCVAL
	      	crpss = crpss + wfac*(crpscc-crpsff)/crpscc
       		PRINT*,nxy,'crpsc=',crpsc,' crpscc=',crpscc,'crpsff=',crpsff, &
			    'crpsf=',crpsf,' wfac=',wfac,' crpss=',crpss

		!Hit RATE AND FALSE ALARM RATE     

		do np = 1, imp1
        		fyy(np) = fyy(np) + yy(np)*wfac
        		fyn(np) = fyn(np) + yn(np)*wfac
       		end do

		!HEIDKE SKILL SCORE if ib=3   

       		if (ib.eq.3) then
        		ftl = ftl + im*wfac
        		do nb = 1, ib
         			do np = 1, imp1
          				if (fcp(nb).eq.float(np-1)) then
           					if (anp(nb).eq.1.) then
            						fhk(nb) = fhk(nb)+(np-1)*wfac
           					end if
          				end if
         			end do
        		end do
       		end if

	end do

	!RELIABILITY DIAGRAM OUTPUT     

	!Brier scores could be calculated from reliability diagram

      	do np = 1, imp1
       		if (sfcp(np).gt.0.1E-10) then
        		fp(np) = (sanp(np)/sfcp(np))*100.0
       		else
        		fp(np) = 0.0
       		end if
      	end do

      	do ii = 1, imp1
       		probs(ii)      = sanp(ii)
       		probs(ii+imp1) = sfcp(ii)
      	end do

	csfcp = 0.0
      	cinfm = 0.0
      	do np = 2, imp1
       		csfcp = csfcp + sfcp(np)
       		if (fp(np).gt.0.0.and.fp(np).lt.100..and.ib.gt.1) then
        		xrate1 = fp(np)/100.0
        		xrate2 = (1.0-xrate1)/float(ib-1)
        		xinf = xrate1*alog10(xrate1)/alog10(float(ib))+ (ib-1.0)*xrate2*alog10(xrate2)/alog10(float(ib))
        		cinfm = cinfm + xinf*sfcp(np)
       		end if
      	end do

      	cinfm = 1.0 + cinfm/csfcp
      	probs(14+4*imp1)= cinfm
	tfst  = 0.0

      	do np = 1, imp1
       		tfst = tfst + sfcp(np)
      	end do

      	creli = 0.0
      	creso = 0.0
      	cunce = 0.0

      	do np = 1, imp1
       		if (sfcp(np).ne.0.0) then
        		cdis  = (np-1.0)/(imp1-1.0)-sanp(np)/sfcp(np)   ! distance for reli
        		creli = creli + sfcp(np)/tfst * cdis*cdis       ! weight=sfc/tfst
        		cdis  = sanp(np)/sfcp(np) - cscrf               ! distance for reso
        		creso = creso + sfcp(np)/tfst * cdis*cdis       ! weight=sfc/tfst
       		end if
      	end do

      	cunce = cscrf*(1.0 - cscrf)
      	cbss  = (creso - creli)/cunce
      	cbs   = creli - creso + cunce
      	probs(10+4*imp1) = creli
      	probs(11+4*imp1) = creso
      	probs(12+4*imp1) = cunce
      	probs(13+4*imp1) = cscrf

	!BRIER SCORES OUTPUT         

      	bsfa = 0.0
      	bsca = 0.0
      	do nb = 1, ib
      		bsf(nb) = bsf(nb)/float(len)
       		bsfa    = bsfa+bsf(nb)
       		bsc(nb) = bsc(nb)/float(len)
       		bsca    = bsca+bsc(nb)

       		if (bsc(nb).ne.0.0) then
        		bss(nb) = (bsc(nb)-bsf(nb))/bsc(nb)

        		if (bss(nb).le.-10.0) then
				bss(nb)=-9.99
       			end if
       		end if
      	end do

     	bsfa = bsfa/float(ib)
      	bsca = bsca/float(ib)

      	if (bsca.ne.0.0) then
       		bssa = (bsca-bsfa)/bsca

       		if (bssa.le.-10.0) then
			bssa=-9.99
      		end if
      	end if

      	bsf(ib+1) = bsfa
      	bsc(ib+1) = bsca
     	bss(ib+1) = bssa
      	probs(7+4*imp1) = bsfa
      	probs(8+4*imp1) = bsca
      	probs(9+4*imp1) = bssa
      	probs(16+4*imp1) = bss(1)
      	probs(17+4*imp1) = bss(ib)

	!RANKED PROBABILITY SCORES OUTPUT   

      	rpfs = rpfs/float(len)
      	rpfc = rpfc/float(len)
      	rpss = (rpfs-rpfc)/(1.0-rpfc)
      	probs(1+4*imp1) = rpfs
      	probs(2+4*imp1) = rpfc
      	probs(3+4*imp1) = rpss

	!Continuous Ranked Probability Score

      	crpsf = crpsf/float(len)
      	crpsc = crpsc/float(len)
      	crpss = crpss/float(len)
      	crpsn = crpsn/float(len)
      	crpss0= (crpsf-crpsc)/(0.0-crpsc)

	OPEN(105,FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'//ANLDATE//'.txt',FORM='FORMATTED',STATUS='UNKNOWN')
        	WRITE(105,'(2F12.4)') crpsf,crpsc
      	CLOSE(105)

      	IDL=INDEX(FCTLAG,'h')-1

      	OPEN(106,FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'//ANLDATE//'.grads',&
                 FORM='UNFORMATTED',STATUS='UNKNOWN')
        	WRITE(106) (CRPSFARR(I),I=1,LEN)
      	CLOSE(106)

      	OPEN(107,FILE='../dataout/CRPSC4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'//ANLDATE//'.grads',&
                 FORM='UNFORMATTED',STATUS='UNKNOWN')
        	WRITE(107) (CRPSCARR(I),I=1,LEN)
      	CLOSE(107)

      	OPEN(108,FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'//ANLDATE//'.aave.grads',&
                 FORM='UNFORMATTED',STATUS='UNKNOWN')
        	 WRITE(108) crpsf
         	 WRITE(108) crpsc
      	CLOSE(108)

      	OPEN(109,FILE='../dataout/CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'//ANLDATE//'.aave.ctl',&
                 FORM='FORMATTED', STATUS='UNKNOWN')
		WRITE(109,100) '^CRPS4CPTECEPS.'//TRIM(FCTLAG)//'ForecastFor'//ANLDATE//'.aave.grads'
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
      	probs(6+4*imp1) = crpss0

     	write (*,'(A,F12.4)') "CRPSF: ",crpsf
      	write (*,'(A,F12.4)') "CRPSS: ",crpss
      	write (*,'(A,F12.4)') "CRPSC: ",crpsc
      	write (*,'(A,F12.4)') "CRPSN: ",crpsn

end subroutine prob
