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

   CHARACTER (LEN=2   ) :: cDay, cMonth, cHour
   CHARACTER (LEN=4   ) :: cYear

   LOGICAL  :: ExitICn

   REAL (KIND=r4) :: LabTim(6   )

   INTEGER (KIND=i4), DIMENSION (:), ALLOCATABLE :: lVari

   REAL (KIND=r4), DIMENSION (:,:), ALLOCATABLE :: GrdHis, Topo

   REAL (KIND=r4), DIMENSION (:), ALLOCATABLE :: Psr, Pcr, Usr, Vsr, Tsr, Hsr, &
                                                 Psm, Pcm, Usm, Vsm, Tsm, Hsm, &
                                                 Psp, Pcp, Usp, Vsp, Tsp, Hsp, &
                                        
                                                 Ccr, Ccm, Ccp, Sdr, Sdm, Sdp, &
                                                 Sfr, Sfm, Sfp, Tar, Tam, Tap, &
                                                 Plr, Plm, Plp, Top, Tpp, &

                                   tgdpr, usslr, uzrsr, uzdsr, cssfr, clsfr, &
                                   prcvr, rnofr, focir, olisr, olesr, civbr, &
                                   civdr, cinbr, cindr, alvbr, alvdr, tp2mr, &
                                   qq2mr, us2mr, vs2mr, tivgr, ocisr, ocesr, &
                                   tgrdr, trdlr, pidlr, trgcr, pigcr, evbsr, &
                                   csdlr, csgrr, tspsr, cllwr, clmdr, clhir, &
                                     
                                   tgdpm, usslm, uzrsm, uzdsm, cssfm, clsfm, &
                                   prcvm, rnofm, focim, olism, olesm, civbm, &
                                   civdm, cinbm, cindm, alvbm, alvdm, tp2mm, &
                                   qq2mm, us2mm, vs2mm, tivgm, ocism, ocesm, &
                                   tgrdm, trdlm, pidlm, trgcm, pigcm, evbsm, &
                                   csdlm, csgrm, tspsm, cllwm, clmdm, clhim, &

                                   tgdpp, usslp, uzrsp, uzdsp, cssfp, clsfp, &
                                   prcvp, rnofp, focip, olisp, olesp, civbp, &
                                   civdp, cinbp, cindp, alvbp, alvdp, tp2mp, &
                                   qq2mp, us2mp, vs2mp, tivgp, ocisp, ocesp, &
                                   tgrdp, trdlp, pidlp, trgcp, pigcp, evbsp, &
                                   csdlp, csgrp, tspsp, cllwp, clmdp, clhip


   CALL InitParameters ()

   CALL SetUnits ()

   ALLOCATE (lVari(nFmx))
   ALLOCATE (GrdHis(nPTx,MxGHsl), Topo(Imax,Jmax))
   ALLOCATE (Psr(nPTx), Pcr(nPTx), Usr(nPTx), Vsr(nPTx), Tsr(nPTx), Hsr(nPTx))
   ALLOCATE (Psm(nPTx), Pcm(nPTx), Usm(nPTx), Vsm(nPTx), Tsm(nPTx), Hsm(nPTx))
   ALLOCATE (Psp(nPTx), Pcp(nPTx), Usp(nPTx), Vsp(nPTx), Tsp(nPTx), Hsp(nPTx))
   ALLOCATE (Ccr(nPTx), Ccm(nPTx), Ccp(nPTx), Sdr(nPTx), Sdm(nPTx), Sdp(nPTx))
   ALLOCATE (Sfr(nPTx), Sfm(nPTx), Sfp(nPTx), Tar(nPTx), Tam(nPTx), Tap(nPTx))
   ALLOCATE (Plr(nPTx), Plm(nPTx), Plp(nPTx), Top(nPTx), Tpp(nPTx))

   ALLOCATE (tgdpr(nPTx), usslr(nPTx), uzrsr(nPTx), uzdsr(nPTx), cssfr(nPTx), clsfr(nPTx))
   ALLOCATE (prcvr(nPTx), rnofr(nPTx), focir(nPTx), olisr(nPTx), olesr(nPTx), civbr(nPTx))
   ALLOCATE (civdr(nPTx), cinbr(nPTx), cindr(nPTx), alvbr(nPTx), alvdr(nPTx), tp2mr(nPTx))
   ALLOCATE (qq2mr(nPTx), us2mr(nPTx), vs2mr(nPTx), tivgr(nPTx), ocisr(nPTx), ocesr(nPTx))
   ALLOCATE (tgrdr(nPTx), trdlr(nPTx), pidlr(nPTx), trgcr(nPTx), pigcr(nPTx), evbsr(nPTx))
   ALLOCATE (csdlr(nPTx), csgrr(nPTx), tspsr(nPTx), cllwr(nPTx), clmdr(nPTx), clhir(nPTx))

   ALLOCATE (tgdpm(nPTx), usslm(nPTx), uzrsm(nPTx), uzdsm(nPTx), cssfm(nPTx), clsfm(nPTx))
   ALLOCATE (prcvm(nPTx), rnofm(nPTx), focim(nPTx), olism(nPTx), olesm(nPTx), civbm(nPTx))
   ALLOCATE (civdm(nPTx), cinbm(nPTx), cindm(nPTx), alvbm(nPTx), alvdm(nPTx), tp2mm(nPTx))
   ALLOCATE (qq2mm(nPTx), us2mm(nPTx), vs2mm(nPTx), tivgm(nPTx), ocism(nPTx), ocesm(nPTx))
   ALLOCATE (tgrdm(nPTx), trdlm(nPTx), pidlm(nPTx), trgcm(nPTx), pigcm(nPTx), evbsm(nPTx))
   ALLOCATE (csdlm(nPTx), csgrm(nPTx), tspsm(nPTx), cllwm(nPTx), clmdm(nPTx), clhim(nPTx))

   ALLOCATE (tgdpp(nPTx), usslp(nPTx), uzrsp(nPTx), uzdsp(nPTx), cssfp(nPTx), clsfp(nPTx))
   ALLOCATE (prcvp(nPTx), rnofp(nPTx), focip(nPTx), olisp(nPTx), olesp(nPTx), civbp(nPTx))
   ALLOCATE (civdp(nPTx), cinbp(nPTx), cindp(nPTx), alvbp(nPTx), alvdp(nPTx), tp2mp(nPTx))
   ALLOCATE (qq2mp(nPTx), us2mp(nPTx), vs2mp(nPTx), tivgp(nPTx), ocisp(nPTx), ocesp(nPTx))
   ALLOCATE (tgrdp(nPTx), trdlp(nPTx), pidlp(nPTx), trgcp(nPTx), pigcp(nPTx), evbsp(nPTx))
   ALLOCATE (csdlp(nPTx), csgrp(nPTx), tspsp(nPTx), cllwp(nPTx), clmdp(nPTx), clhip(nPTx) )

   Top=0.0_r4
   Tpp=0.0_r4
   INQUIRE (FILE=TRIM(DirTopog)//TRIM(TopogInp), EXIST=ExitICn)
   IF (ExitICn) THEN
      OPEN (UNIT=20, STATUS='OLD', FORM='UNFORMATTED', &
            FILE=TRIM(DirTopog)//TRIM(TopogInp))
      READ (UNIT=20) Top
   ELSE
      WRITE (UNIT=*, FMT='(A,/,A)') ' Inital Condition File Does Not Exist:', &
                                    ' Set Topography Null'
      Top=0.0_r4
   END IF
   CLOSE(UNIT=20)

!  nt=0   
!  DO n=1   ,nPTx
!     ix=0   
!     DO i=1   ,Imax
!        DO j=1   ,Jmax
!           IF (iLoc(n) == i .AND. jLoc(n) == j) THEN
!              ix=1   
!              nt=nt+1   
!              Top(nt)=Top(nt)
!           END IF
!        END DO
!     END DO
!     IF (ix == 0   ) WRITE (UNIT=*, FMT='(3(A,I5))') ' n = ', n, &
!                                  ' iLoc = ', iLoc(n), ' jLoc = ', jLoc(n)
!  END DO
!  nTmp=nt

   nTmp=nPTx
   np=0   
   DO n=1   ,nPTx
      IF (cLoc(n)(1   :4   ) /= GrdBox) THEN
         np=np+1   
         DoPt(n)=.TRUE.
      END IF
   END DO
   nPmp=np
   WRITE (UNIT=*, FMT='(/,2(A,I5),/)') ' nTmp = ',nTmp,' nPmp = ',nPmp
   WRITE (UNIT=*, FMT='(10F8.2)') (Top(l),l=1   ,nTmp)
   WRITE (UNIT=*, FMT='(/)')
   
   OPEN  (UNIT=30, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//TRIM(GrHPrfOut))
   OPEN  (UNIT=40, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//TRIM(GrHLocOut))
   OPEN  (UNIT=50, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//TRIM(GrHIdfOut))
   WRITE (UNIT=30, FMT='(I5.5)') nPmp
   WRITE (UNIT=40, FMT='(I5.5)') nPmp
   WRITE (UNIT=50, FMT='(I5.5)') nPmp
   DO n=1   ,nPTx
      IF (DoPt(n)) WRITE (UNIT=30, FMT='(A)') Prx(n)
      IF (DoPt(n)) WRITE (UNIT=40, FMT='(A)') Prx(n)
      IF (DoPt(n)) WRITE (UNIT=50, FMT='(A)') cLoc(n)
   END DO
   CLOSE (UNIT=30)
   CLOSE (UNIT=40)
   CLOSE (UNIT=50)

   MedTim=NINT(TMean/DelT,i4)
   FMdTim=1.0_r4/REAL(MedTim,r4)
   DO n=1   ,nPTx
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

   nv=0   
   DO n=1   ,nFmx
     nv=nv+lVar(n)
     lVari(n)=nv-lVar(n)+1   
   END DO
   WRITE (UNIT=*, FMT='(A)')' lVar:'
   WRITE (UNIT=*, FMT='(20I4)')lVar
   WRITE (UNIT=*, FMT='(A)')' lVari:'
   WRITE (UNIT=*, FMT='(20I4)')lVari

   OPEN (UNIT=60, STATUS='OLD', FORM='UNFORMATTED', &
         FILE=TRIM(DirModel)//TRIM(GrHDatIn))
   OPEN (UNIT=70, STATUS='REPLACE', FORM='UNFORMATTED', &
         FILE=TRIM(DirOut)//TRIM(GrHDatOut))

   mt=0   
   DO

      READ (UNIT=60, IOSTAT=ios) LabTim
      IF (ios /= 0   ) EXIT
      READ (UNIT=60) GrdHis
      mt=mt+1   

      DO n=1   ,nFmx
         l=lVari(n)
         DO m=1   ,nPTx
            IF (INDEX(cvar(n),iVar(1   )) == 1   ) THEN
               Plr(m)=GrdHis(m,l)
               Psr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(2   )) == 1   ) THEN
               Pcr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(3   )) == 1   ) THEN
               Ccr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(4   )) == 1   ) THEN
               Usr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(5   )) == 1   ) THEN
               Vsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(6   )) == 1   ) THEN
               Tsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(7   )) == 1   ) THEN
               Hsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(8   )) == 1   ) THEN
               Sdr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(9   )) == 1   ) THEN
               Sfr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(10   )) == 1   ) THEN
               Tar(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(11   )) == 1   ) THEN
               tgdpr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(12   )) == 1   ) THEN
               usslr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(13   )) == 1   ) THEN
                uzrsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(14   )) == 1   ) THEN
                uzdsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(15   )) == 1   ) THEN
                cssfr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(16   )) == 1   ) THEN
                clsfr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(17   )) == 1   ) THEN
                prcvr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(18   )) == 1   ) THEN
                rnofr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(19   )) == 1   ) THEN
                focir(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(20   )) == 1   ) THEN
                olisr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(21   )) == 1   ) THEN
                olesr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(22   )) == 1   ) THEN
                civbr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(23   )) == 1   ) THEN
                civdr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(24   )) == 1   ) THEN
                cinbr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(25   )) == 1   ) THEN
                cindr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(26   )) == 1   ) THEN
                alvbr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(27   )) == 1   ) THEN
                alvdr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(28   )) == 1   ) THEN
                tp2mr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(29   )) == 1   ) THEN
                qq2mr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(30   )) == 1   ) THEN
                us2mr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(31   )) == 1   ) THEN
                vs2mr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(32   )) == 1   ) THEN
                ocisr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(33   )) == 1   ) THEN
                ocesr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(34   )) == 1   ) THEN
                tgrdr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(35   )) == 1   ) THEN
                trdlr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(36   )) == 1   ) THEN
                pidlr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(37   )) == 1   ) THEN
                trgcr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(38   )) == 1   ) THEN
                pigcr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(39   )) == 1   ) THEN
                evbsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(40   )) == 1   ) THEN
                csdlr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(41   )) == 1   ) THEN
                csgrr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(42   )) == 1   ) THEN
                tspsr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(43   )) == 1   ) THEN
                cllwr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(44   )) == 1   ) THEN
                clmdr(m)=GrdHis(m,l)
            ELSE IF (INDEX(cvar(n),iVar(45   )) == 1   ) THEN
                clhir(m)=GrdHis(m,l)
            END IF

         END DO
      END DO






















      DO n=1   ,nPTx
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

      DO n=1   ,nPTx
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
         tgdpm(n)=tgdpm(n)+tgdpr(n)
         usslm(n)=usslm(n)+usslr(n)
         uzrsm(n)=uzrsm(n)+uzrsr(n)
         uzdsm(n)=uzdsm(n)+uzdsr(n)
         cssfm(n)=cssfm(n)+cssfr(n)
         clsfm(n)=clsfm(n)+clsfr(n)
         prcvm(n)=prcvm(n)+prcvr(n)
         rnofm(n)=rnofm(n)+rnofr(n)
         focim(n)=focim(n)+focir(n)
         olism(n)=olism(n)+olisr(n)
         olesm(n)=olesm(n)+olesr(n)
         civbm(n)=civbm(n)+civbr(n)
         civdm(n)=civdm(n)+civdr(n)
         cinbm(n)=cinbm(n)+cinbr(n)
         cindm(n)=cindm(n)+cindr(n)
         alvbm(n)=alvbm(n)+alvbr(n)
         alvdm(n)=alvdm(n)+alvdr(n)
         tp2mm(n)=tp2mm(n)+tp2mr(n)
         qq2mm(n)=qq2mm(n)+qq2mr(n)
         us2mm(n)=us2mm(n)+us2mr(n)
         vs2mm(n)=vs2mm(n)+vs2mr(n)
         ocism(n)=ocism(n)+ocisr(n)
         ocesm(n)=ocesm(n)+ocesr(n)
         tgrdm(n)=tgrdm(n)+tgrdr(n)         
         trdlm(n)=trdlm(n)+trdlr(n)
         pidlm(n)=pidlm(n)+pidlr(n)
         trgcm(n)=trgcm(n)+trgcr(n)
         pigcm(n)=pigcm(n)+pigcr(n)
         evbsm(n)=evbsm(n)+evbsr(n)
         csdlm(n)=csdlm(n)+csdlr(n)
         csgrm(n)=csgrm(n)+csgrr(n)
         tspsm(n)=tspsm(n)+tspsr(n)
         cllwm(n)=cllwm(n)+cllwr(n)
         clmdm(n)=clmdm(n)+clmdr(n)
         clhim(n)=clhim(n)+clhir(n) 
      END DO

      IF (MOD(mt,MedTim) == 0   ) THEN
         np=0   
         DO n=1   ,nPTx
            IF (DoPt(n)) THEN
               np=np+1   
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
               tgdpp(np)=FMdTim*tgdpm(n)
               usslp(np)=FMdTim*usslm(n)
               uzrsp(np)=FMdTim*uzrsm(n)
               uzdsp(np)=FMdTim*uzdsm(n)
               cssfp(np)=FMdTim*cssfm(n)
               clsfp(np)=FMdTim*clsfm(n)
               prcvp(np)=FMdTim*prcvm(n)
               rnofp(np)=FMdTim*rnofm(n)
               focip(np)=FMdTim*focim(n)
               olisp(np)=FMdTim*olism(n)
               olesp(np)=FMdTim*olesm(n)
               civbp(np)=FMdTim*civbm(n)
               civdp(np)=FMdTim*civdm(n)
               cinbp(np)=FMdTim*cinbm(n)
               cindp(np)=FMdTim*cindm(n)
               alvbp(np)=FMdTim*alvbm(n)
               alvdp(np)=FMdTim*alvdm(n)
               tp2mp(np)=FMdTim*tp2mm(n)
               qq2mp(np)=FMdTim*qq2mm(n)
               us2mp(np)=FMdTim*us2mm(n)
               vs2mp(np)=FMdTim*vs2mm(n)
               ocisp(np)=FMdTim*ocism(n)
               ocesp(np)=FMdTim*ocesm(n)
               tgrdp(np)=FMdTim*tgrdm(n)
               trdlp(np)=FMdTim*trdlm(n)
               pidlp(np)=FMdTim*pidlm(n)
               trgcp(np)=FMdTim*trgcm(n)
               pigcp(np)=FMdTim*pigcm(n)
               evbsp(np)=FMdTim*evbsm(n)
               csdlp(np)=FMdTim*csdlm(n)
               csgrp(np)=FMdTim*csgrm(n)
               tspsp(np)=FMdTim*tspsm(n)
               cllwp(np)=FMdTim*cllwm(n)
               clmdp(np)=FMdTim*clmdm(n)
               clhip(np)=FMdTim*clhim(n)


               Tpp(np)=Top(n)
            END IF
         END DO
         WRITE (UNIT=70) (Tpp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Plp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Psp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Pcp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Ccp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Usp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Vsp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Tsp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Hsp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Sdp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Sfp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (Tap(n),n=1   ,nPmp)
         WRITE (UNIT=70) (tgdpp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (usslp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (uzrsp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (uzdsp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (cssfp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (clsfp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (prcvp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (rnofp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (focip(n),n=1   ,nPmp)
         WRITE (UNIT=70) (olisp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (olesp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (civbp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (civdp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (cinbp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (cindp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (alvbp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (alvdp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (tp2mp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (qq2mp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (us2mp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (vs2mp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (ocisp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (ocesp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (tgrdp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (trdlp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (pidlp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (trgcp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (pigcp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (evbsp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (csdlp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (csgrp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (tspsp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (cllwp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (clmdp(n),n=1   ,nPmp)
         WRITE (UNIT=70) (clhip(n),n=1   ,nPmp)

         DO n=1   ,nPTx
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
            tgdpm(n)=0.0_r4
            usslm(n)=0.0_r4
            uzrsm(n)=0.0_r4
            uzdsm(n)=0.0_r4
            cssfm(n)=0.0_r4
            clsfm(n)=0.0_r4
            prcvm(n)=0.0_r4
            rnofm(n)=0.0_r4
            focim(n)=0.0_r4
            olism(n)=0.0_r4
            olesm(n)=0.0_r4
            civbm(n)=0.0_r4
            civdm(n)=0.0_r4
            cinbm(n)=0.0_r4
            cindm(n)=0.0_r4
            alvbm(n)=0.0_r4
            alvdm(n)=0.0_r4
            tp2mm(n)=0.0_r4
            qq2mm(n)=0.0_r4
            us2mm(n)=0.0_r4
            vs2mm(n)=0.0_r4
            ocism(n)=0.0_r4
            ocesm(n)=0.0_r4
            tgrdm(n)=0.0_r4
            trdlm(n)=0.0_r4
            pidlm(n)=0.0_r4
            trgcm(n)=0.0_r4
            pigcm(n)=0.0_r4
            evbsm(n)=0.0_r4
            csdlm(n)=0.0_r4
            csgrm(n)=0.0_r4
            tspsm(n)=0.0_r4
            cllwm(n)=0.0_r4
            clmdm(n)=0.0_r4
            clhim(n)=0.0_r4
         END DO
      END IF

   END DO
   MaxTim=mt

   CLOSE (UNIT=60)
   CLOSE (UNIT=70)

   cYear=GrHDatOut(08   :11   )
   cMonth=GrHDatOut(12   :13   )
   cDay=GrHDatOut(14   :15   )
   cHour=GrHDatOut(16   :17   )
   MTmStp=MaxTim/MedTim
   READ (cDay, FMT='(I2)') iDay
   READ (cMonth, FMT='(I2)') iMonth
   READ (cYear, FMT='(I4)') iYear
   READ (cHour, FMT='(I2)') jHour
   jMinute=NINT(TMean/60.0_r4,i4)
   iMinute=jMinute/2   
   MinTim=iMinute/60   
   iMinute=iMinute-MinTim*60   
   iHour=jHour+MinTim-mUTC
   IF (iHour < 0   ) THEN
      iHour=iHour+24   
      iDay=iDay-1   
      IF (iDay == 0   ) THEN
         iMonth=iMonth-1   
         IF (iMonth == 0   ) THEN
            iMonth=12   
            iYear=iYear-1   
         END IF
      END IF
   END IF
   IF (MOD(iYear,4   ) == 0   ) THEN
      LMonth(2   )=29   
   END IF
   IF (iDay == 0   ) iDay=LMonth(iMonth)
   WRITE (cDay, FMT='(I2.2)') iDay
   WRITE (cYear, FMT='(I4.4)') iYear

   OPEN  (UNIT=80, STATUS='REPLACE', FORM='FORMATTED', &
          FILE=TRIM(DirOut)//TRIM(GrHCtlOut))
   WRITE (UNIT=80, FMT='(A)') 'dset ^'//TRIM(GrHDatOut)
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A)') 'options sequential big_endian'
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A)') 'undef -2.56E33'
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A)') 'title '//title
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A,I4,A)') 'xdef ',nPmp,' linear 1 1'
   WRITE (UNIT=80, FMT='(A,I4,A)') 'ydef ',1,' linear 1 1'
   WRITE (UNIT=80, FMT='(A,I4,A)') 'zdef ',1,' linear 1000 10'
   WRITE (UNIT=80, FMT='(A,I4,A,I2.2,A,I2.2,5A,I4,A)') &
                        'tdef ', MTmStp, ' linear ', iHour, ':', &
                         iMinute, 'z', cDay, AMonth(iMonth), cYear,' ', &
                         jMinute, 'mn'
   WRITE (UNIT=80, FMT='(A)') '*'
   WRITE (UNIT=80, FMT='(A,I5)') 'vars ', nFmxO
   DO n=1   ,nFmxO
      WRITE (UNIT=80, FMT='(A,I3,5A)') bVar(n), lVarc(n), ' 99 ', dVar(n),  &
                          '(',AUnits(uVarc(n)),')'
   END DO
   WRITE (UNIT=80, FMT='(A)') 'endvars'
   CLOSE (UNIT=80)

   STOP' End of Grid History Post-Processing'

END PROGRAM PostGridHistory
