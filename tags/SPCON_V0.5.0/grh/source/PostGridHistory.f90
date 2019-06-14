PROGRAM PostGridHistory

   USE Units, ONLY : AUnits, SetUnits

   USE InputParameters, Only : r4, i4, DelT, TMean, &
                               Imax, Jmax, nPTx, nFmx, MxGHsl, nFmxO, mUTC, &
                               cVar, lVar, uVar, cLoc, iLoc, jLoc, Prx, &
                               iVar, bVar, dVar, uVarc, lVarc, &
                               GrdBox, Title, DoPt, LMonth, AMonth, &
                               TopogInp, GrHDatIn, GrHDatOut, GrHPrfOut, &
                               GrHLocOut, GrHIdfOut, GrHCtlOut, &
                               DirTopog, DirModel, DirOut, &
                               InitParameters

   IMPLICIT NONE
 
   INTEGER (KIND=i4) :: n, i, j, l, m, mt, ix, np, nv, nt, ios, &
                        nPmp, nTmp, MTmStp, MaxTim, MedTim, MinTim, &
                        iMinute, jMinute, iHour, jHour, iDay, iMonth, iYear

   REAL (KIND=r4) :: FMdTim, tp, es, ee, tvm, cte

   CHARACTER (LEN=2_i4) :: cDay, cMonth, cHour
   CHARACTER (LEN=4_i4) :: cYear

   LOGICAL (KIND=i4) :: ExitICn

   REAL (KIND=r4) :: LabTim(6_i4)

   INTEGER (KIND=i4), DIMENSION (:), ALLOCATABLE :: lVari

   REAL (KIND=r4), DIMENSION (:,:), ALLOCATABLE :: GrdHis, Topo

   REAL (KIND=r4), DIMENSION (:), ALLOCATABLE :: Psr, Pcr, Usr, Vsr, Tsr, Hsr, &
                                                 Psm, Pcm, Usm, Vsm, Tsm, Hsm, &
                                                 Psp, Pcp, Usp, Vsp, Tsp, Hsp, &
                                                 Ccr, Ccm, Ccp, Sdr, Sdm, Sdp, &
                                                 Sfr, Sfm, Sfp, Tar, Tam, Tap, &
                                                 Plr, Plm, Plp, Top, Tpp, Tp2m,Tp2

   CALL InitParameters ()

   CALL SetUnits ()

   ALLOCATE (lVari(nFmx))
   ALLOCATE (GrdHis(nPTx,MxGHsl), Topo(Imax,Jmax))
   ALLOCATE (Psr(nPTx), Pcr(nPTx), Usr(nPTx), Vsr(nPTx), Tsr(nPTx), Hsr(nPTx))
   ALLOCATE (Psm(nPTx), Pcm(nPTx), Usm(nPTx), Vsm(nPTx), Tsm(nPTx), Hsm(nPTx))
   ALLOCATE (Psp(nPTx), Pcp(nPTx), Usp(nPTx), Vsp(nPTx), Tsp(nPTx), Hsp(nPTx))
   ALLOCATE (Ccr(nPTx), Ccm(nPTx), Ccp(nPTx), Sdr(nPTx), Sdm(nPTx), Sdp(nPTx))
   ALLOCATE (Sfr(nPTx), Sfm(nPTx), Sfp(nPTx), Tar(nPTx), Tam(nPTx), Tap(nPTx))
   ALLOCATE (Plr(nPTx), Plm(nPTx), Plp(nPTx), Top(nPTx), Tpp(nPTx), Tp2m(nPTx), Tp2(nPTx))

   Top=0.0_r4
   Tpp=0.0_r4
   INQUIRE (FILE=TRIM(DirTopog)//TopogInp, EXIST=ExitICn)
   IF (ExitICn) THEN
      OPEN (UNIT=20, STATUS='OLD', FORM='UNFORMATTED', &
            FILE=TRIM(DirTopog)//TopogInp)
      READ (UNIT=20) Top
   ELSE
      WRITE (UNIT=*, FMT='(A,/,A)') ' Inital Condition File Does Not Exist:', &
                                    ' Set Topography Null'
      Top=0.0_r4
   END IF
   CLOSE(UNIT=20)

!  nt=0_i4
!  DO n=1_i4,nPTx
!     ix=0_i4
!     DO i=1_i4,Imax
!        DO j=1_i4,Jmax
!           IF (iLoc(n) == i .AND. jLoc(n) == j) THEN
!              ix=1_i4
!              nt=nt+1_i4
!              Top(nt)=Top(nt)
!           END IF
!        END DO
!     END DO
!     IF (ix == 0_i4) WRITE (UNIT=*, FMT='(3(A,I5))') ' n = ', n, &
!                                  ' iLoc = ', iLoc(n), ' jLoc = ', jLoc(n)
!  END DO
!  nTmp=nt

   nTmp=nPTx
   np=0_i4
   DO n=1_i4,nPTx
      IF (cLoc(n)(1_i4:4_i4) /= GrdBox) THEN
         np=np+1_i4
         DoPt(n)=.TRUE.
      END IF
   END DO
   nPmp=np
   WRITE (UNIT=*, FMT='(/,2(A,I5),/)') ' nTmp = ',nTmp,' nPmp = ',nPmp
   WRITE (UNIT=*, FMT='(10F8.2)') (Top(l),l=1_i4,nTmp)
   WRITE (UNIT=*, FMT='(/)')
   
   OPEN  (UNIT=30, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//GrHPrfOut)
   OPEN  (UNIT=40, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//GrHLocOut)
   OPEN  (UNIT=50, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//GrHIdfOut)
   WRITE (UNIT=30, FMT='(I5.5)') nPmp
   WRITE (UNIT=40, FMT='(I5.5)') nPmp
   WRITE (UNIT=50, FMT='(I5.5)') nPmp
   DO n=1_i4,nPTx
      IF (DoPt(n)) WRITE (UNIT=30, FMT='(A)') Prx(n)
      IF (DoPt(n)) WRITE (UNIT=40, FMT='(A)') Prx(n)
      IF (DoPt(n)) WRITE (UNIT=50, FMT='(A)') cLoc(n)
   END DO
   CLOSE (UNIT=30)
   CLOSE (UNIT=40)
   CLOSE (UNIT=50)

   MedTim=NINT(TMean/DelT,i4)
   FMdTim=1.0_r4/REAL(MedTim,r4)
   DO n=1_i4,nPTx
     Plm(n)=0.0_r4
     Psm(n)=0.0_r4
     Pcm(n)=0.0_r4
     Ccm(n)=0.0_r4
     Usm(n)=0.0_r4
     Vsm(n)=0.0_r4
     Tsm(n)=0.0_r4
     Hsm(n)=0.0_r4
     Sdm(n)=0.0_r4
     Sfm(n)=0.0_r4
     Tam(n)=0.0_r4
   END DO

   nv=0_i4
   DO n=1_i4,nFmx
     nv=nv+lVar(n)
     lVari(n)=nv-lVar(n)+1_i4
   END DO
   WRITE (UNIT=*, FMT='(A)')' lVar:'
   WRITE (UNIT=*, FMT='(20I4)')lVar
   WRITE (UNIT=*, FMT='(A)')' lVari:'
   WRITE (UNIT=*, FMT='(20I4)')lVari

   OPEN (UNIT=60, STATUS='OLD', FORM='UNFORMATTED', &
         FILE=TRIM(DirModel)//GrHDatIn)
   OPEN (UNIT=70, STATUS='REPLACE', FORM='UNFORMATTED', &
         FILE=TRIM(DirOut)//GrHDatOut)

   mt=0_i4
   DO

      READ (UNIT=60, IOSTAT=ios) LabTim
      
      PRINT* ,ios
      IF (ios /= 0_i4) EXIT
      READ (UNIT=60) GrdHis
      mt=mt+1_i4

      DO n=1_i4,nFmx
         l=lVari(n)
         DO m=1_i4,nPTx            
	    IF (INDEX(cvar(n),iVar(1_i4)) == 1_i4) THEN
               Plr(m)=GrdHis(m,l)
               Psr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(2_i4)) == 1_i4) THEN
               Pcr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(3_i4)) == 1_i4) THEN
               Ccr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(4_i4)) == 1_i4) THEN
               Usr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(5_i4)) == 1_i4) THEN
               Vsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(6_i4)) == 1_i4) THEN
               Tsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(7_i4)) == 1_i4) THEN
               Hsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(8_i4)) == 1_i4) THEN
               Sdr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(9_i4)) == 1_i4) THEN
               Sfr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(10_i4)) == 1_i4) THEN
               Tar(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(11_i4)) == 1_i4) THEN
               WRITE(*,*)cvar(n),iVar(11_i4),GrdHis(m,l)
	       Tp2(m)=GrdHis(m,l)
            END IF
         END DO
      END DO

      DO n=1_i4,nPTx
         Pcr(n)=Pcr(n)*REAL(MedTim,r4)*DelT
         Sfr(n)=Sfr(n)*REAL(MedTim,r4)*DelT
         Ccr(n)=Ccr(n)*100.0_r4
         IF (Hsr(n) < 0.0_r4) Hsr(n)=1.0E-6_r4
         tp=Tsr(n)/(1.0_r4+0.608_r4*Hsr(n))
         es=6.1078_r4*EXP(17.2693882_r4*(tp-273.16_r4)/(tp-35.86_r4))
         ee=Psr(n)*Hsr(n)/(0.622_r4+0.378_r4*Hsr(n))
         Hsr(n)=100.0_r4*ee/es
         IF (Hsr(n) > 100.0_r4) Hsr(n)=100.0_r4
         tvm=Tsr(n)+0.5_r4*0.0065_r4*Top(n)
         cte=(9.80665_r4*Top(n))/(287.05_r4*tvm)
         Psr(n)=Psr(n)*EXP(cte)
         Tsr(n)=tp-273.16_r4
         IF (ABS(Tar(n)) > 100.0_r4) THEN
            Tar(n)=Tar(n)-273.16_r4
         ELSE
            Tar(n)=Tsr(n)
         END IF
      END DO

      DO n=1_i4,nPTx
         Plm(n)=Plm(n)+Plr(n)
         Psm(n)=Psm(n)+Psr(n)
         Pcm(n)=Pcm(n)+Pcr(n)
         Ccm(n)=Ccm(n)+Ccr(n)
         Usm(n)=Usm(n)+Usr(n)
         Vsm(n)=Vsm(n)+Vsr(n)
         Tsm(n)=Tsm(n)+Tsr(n)
         Hsm(n)=Hsm(n)+Hsr(n)
         Sdm(n)=Sdm(n)+Sdr(n)
         Sfm(n)=Sfm(n)+Sfr(n)
         Tam(n)=Tam(n)+Tar(n)
	 Tp2m(n)=Tp2m(n)+Tp2(n)
      END DO

      IF (MOD(mt,MedTim) == 0_i4) THEN
         np=0_i4
         DO n=1_i4,nPTx
            IF (DoPt(n)) THEN
               np=np+1_i4
               Plp(np)=FMdTim*Plm(n)
               Psp(np)=FMdTim*Psm(n)
               Pcp(np)=FMdTim*Pcm(n)
               Ccp(np)=FMdTim*Ccm(n)
               Usp(np)=FMdTim*Usm(n)
               Vsp(np)=FMdTim*Vsm(n)
               Tsp(np)=FMdTim*Tsm(n)
               Hsp(np)=FMdTim*Hsm(n)
               Sdp(np)=FMdTim*Sdm(n)
               Sfp(np)=FMdTim*Sfm(n)
               Tap(np)=FMdTim*Tam(n)
	       Tp2(np)=FMdTim*Tp2m(n)
               Tpp(np)=Top(n)
            END IF
         END DO
         WRITE (UNIT=70) (Tpp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Plp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Psp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Pcp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Ccp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Usp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Vsp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Tsp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Hsp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Sdp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Sfp(n),n=1_i4,nPmp)
         WRITE (UNIT=70) (Tap(n),n=1_i4,nPmp)
	 WRITE (UNIT=70) (Tp2(n),n=1_i4,nPmp)
         DO n=1_i4,nPTx
            Plm(n)=0.0_r4
            Psm(n)=0.0_r4
            Pcm(n)=0.0_r4
            Ccm(n)=0.0_r4
            Usm(n)=0.0_r4
            Vsm(n)=0.0_r4
            Tsm(n)=0.0_r4
            Hsm(n)=0.0_r4
            Sdm(n)=0.0_r4
            Sfm(n)=0.0_r4
            Tam(n)=0.0_r4
	    Tp2m(n)=0.0_r4
         END DO
      END IF

   END DO
   MaxTim=mt

   CLOSE (UNIT=60)
   CLOSE (UNIT=70)

   cYear=GrHDatOut(08_i4:11_i4)
   cMonth=GrHDatOut(12_i4:13_i4)
   cDay=GrHDatOut(14_i4:15_i4)
   cHour=GrHDatOut(16_i4:17_i4)
   MTmStp=MaxTim/MedTim
   READ (cDay, FMT='(I2)') iDay
   READ (cMonth, FMT='(I2)') iMonth
   READ (cYear, FMT='(I4)') iYear
   READ (cHour, FMT='(I2)') jHour
   jMinute=NINT(TMean/60.0_r4,i4)
   iMinute=jMinute/2_i4
   MinTim=iMinute/60_i4
   iMinute=iMinute-MinTim*60_i4
   iHour=jHour+MinTim-mUTC
   IF (iHour < 0_i4) THEN
      iHour=iHour+24_i4
      iDay=iDay-1_i4
      IF (iDay == 0_i4) THEN
         iMonth=iMonth-1_i4
         IF (iMonth == 0_i4) THEN
            iMonth=12_i4
            iYear=iYear-1_i4
         END IF
      END IF
   END IF
   IF (MOD(iYear,4_i4) == 0_i4) THEN
      LMonth(2_i4)=29_i4
   END IF
   IF (iDay == 0_i4) iDay=LMonth(iMonth)
   WRITE (cDay, FMT='(I2.2)') iDay
   WRITE (cYear, FMT='(I4.4)') iYear

   OPEN  (UNIT=80, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//GrHCtlOut)
   WRITE (UNIT=80, FMT='(A)') 'dset ^'//GrHDatOut
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A)') 'options sequential big_endian'
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A)') 'undef -2.56E33'
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A)') 'title '//title
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A,I4,A)') 'xdef ',nPmp,' linear 1 1'
   WRITE (UNIT=80, FMT='(A,I4,A)') 'ydef ',1,' linear 1 1'
   WRITE (UNIT=80, FMT='(A,I4,A)') 'zdef ',1,' linear 1000 -10'
   WRITE (UNIT=80, FMT='(A,I4,A,I2.2,A,I2.2,5A,I4,A)') &
                        'tdef ', MTmStp, ' linear ', iHour, ':', &
                         iMinute, 'z', cDay, AMonth(iMonth), cYear,' ', &
                         jMinute, 'mn'
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A,I5)') 'vars ', nFmxO
   DO n=1_i4,nFmxO
      WRITE (UNIT=80, FMT='(A,I3,5A)') bVar(n), lVarc(n), ' 99 ', dVar(n),  &
                          '(',AUnits(uVarc(n)),')'
   END DO
   WRITE (UNIT=80, FMT='(A)') 'endvars'
   CLOSE (UNIT=80)

   STOP' End of Grid History Post-Processing'

END PROGRAM PostGridHistory
