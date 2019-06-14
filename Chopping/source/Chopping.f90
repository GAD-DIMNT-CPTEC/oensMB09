PROGRAM Chopping

  USE m_Parameters, ONLY: Ki4, Kr4, Kr8, &
                          MendInp, ImaxInp, JmaxInp, KmaxInp, &
                          MendOut, ImaxOut, JmaxOut, KmaxOut, &
                          Mnwv2Inp, Mnwv2Out, Mnwv3Inp, Mnwv3Out, &
                          KmaxInpp, KmaxOutp, NTracers, Kdim, ICaseDec, Iter, MendCut, &
                          ForecastDay, TimeOfDay, cTv, Smth_Per_Cut, &
                          DirInp, DirOut, DirTop, DirSig, DirGrd, &
                          DataInp, DataOut, DataTop, DataSig, &
                          DGDInp, GDASInp, OzonInp, TracInp, OzonOut, TracOut, &
                          Get_Ozone, Get_Tracers, GrADS, GrADSOnly, GDASOnly, Smooth_Topo, &
                          Get_Parameters

  USE m_Arrays

  USE m_Recomposition

  USE m_Vertical_Interpolation, Only: Vert_Sigma_Inter

  USE m_Utils, Only: New_Sigma, New_Ps

  USE m_Fourier, Only: Init_FFT, ClsMem_FFT

  USE m_Legendre, Only: Init_Legendre, ClsMem_Legendre

  USE m_Decomposition

  IMPLICIT NONE

  INTEGER (KIND=Ki4) :: i, j, k, nt, ios

  LOGICAL (KIND=Ki4) :: Get_NewTop=.FALSE., Get_NewSig=.FALSE., &
                        Exist_GDAS=.FALSE., Exist_GANL=.FALSE., &
                        Vertical_Interp=.TRUE.

  CHARACTER (LEN=7) :: TruncInp='T   L  ', TruncOut='T   L  '

  CHARACTER (LEN=12) :: Tdef='  z         '

  CHARACTER (LEN=3), DIMENSION (12) :: MonChar =  &
                     (/ 'jan', 'feb', 'mar', 'apr', 'may', 'jun', &
                        'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /)

  REAL (KIND=Kr8), DIMENSION (:,:,:), ALLOCATABLE :: gOzonOut

  REAL (KIND=Kr8), DIMENSION (:,:,:,:), ALLOCATABLE :: gTracOut

  CALL Get_Parameters

  CALL Get_Arrays

  WRITE (TruncInp(2:4), '(I3.3)') MendInp
  WRITE (TruncInp(6:7), '(I2.2)') KmaxInp
  WRITE (TruncOut(2:4), '(I3.3)') MendOut
  WRITE (TruncOut(6:7), '(I2.2)') KmaxOut

  INQUIRE (FILE=TRIM(DGDInp)//TRIM(GDASInp), EXIST=Exist_GDAS)
  INQUIRE (FILE=TRIM(DirInp)//TRIM(DataInp), EXIST=Exist_GANL)

  IF (Exist_GDAS .AND. .NOT.Exist_GANL) THEN

    CALL GDAStoGANL
    CALL Get_SpHuTracers

  ELSE

    WRITE (*, '(/,A)') ' GANL File Already Exists'

    INQUIRE (FILE=TRIM(DirInp)//TRIM(OzonInp), EXIST=Get_Ozone)
    INQUIRE (FILE=TRIM(DirInp)//TRIM(TracInp), EXIST=Get_Tracers)
    IF (Get_Ozone) THEN
      NTracers=NTracers+1
      IF (Get_Tracers) THEN
        WRITE (*, '(/,A)') ' Considering Just One Other Tracer Than Ozone'
        NTracers=NTracers+1
      END IF
    END IF
    CALL Get_SpHuTracers

    IF (Get_Ozone) THEN
      WRITE (*, '(/,A)') ' Get Ozone from '//TRIM(OzonInp)
      OPEN (30, FILE=TRIM(DirInp)//TRIM(OzonInp), &
                STATUS='OLD', FORM='UNFORMATTED')
      nt=2
      DO k=1,KmaxInp
        READ (30) qWorkInp
        qSpHuInp(:,k,nt)=qWorkInp
      END DO
      CLOSE(30)

      IF (Get_Tracers) THEN
        WRITE (*, '(/,A)') ' Get Tracers from '//TRIM(TracInp)
        OPEN (35, FILE=TRIM(DirInp)//TRIM(TracInp), &
                  STATUS='OLD', FORM='UNFORMATTED')
        nt=3
        DO k=1,KmaxInp
          READ (35) qWorkInp
          qSpHuInp(:,k,nt)=qWorkInp
        END DO
        CLOSE(35)
      ELSE
        WRITE (*, '(/,A)') ' Other Tracers File Does Not Exist'
      END IF

    ELSE

      WRITE (*, '(/,A)') ' Ozone file Does Not Exist. Ignore Other Tracers'
      Get_Tracers=.FALSE.
      NTracers=1

    END IF

  END IF
  WRITE (*, '(/,A,I5)') ' NTracers = ', NTracers

  CALL IC_Read

  qTopoOut=0.0_Kr8
  gTopoDel=0.0_Kr8
  INQUIRE (FILE=TRIM(DirTop)//TRIM(DataTop), EXIST=Get_NewTop)
  IF (Get_NewTop) THEN
    WRITE (*, '(/,A)') ' Getting New Topography'
    OPEN  (10, FILE=TRIM(DirTop)//TRIM(DataTop), &
               STATUS='OLD', FORM='UNFORMATTED')
    READ  (10) qWorkOut
    CLOSE (10)
    qTopoOut=qWorkOut
    WRITE (*, '(/,A)') ' TopoOut for New Topography:'
    WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))
  ELSE
    IF (Smooth_Topo) THEN
      WRITE (*, '(/,A)') ' Chopping Old Topography for Smoothing'
      CALL Chop (qTopoInp(:), qTopoOut(:), MendInp, MendOut, Kdim)
      WRITE (*, '(/,A)') ' TopoOut after Chopping for Smoothing:'
      WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))
    END IF
  END IF
  IF (Smooth_Topo) THEN
    WRITE (*, '(/,A)') ' Smoothing Topography'
    CALL Smooth_Coef (qTopoOut, MendOut, Kdim, MendCut)
    WRITE (*, '(/,A)') ' TopoOut after Smoothing:'
    WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))
  END IF

  INQUIRE (FILE=TRIM(DirSig)//TRIM(DataSig), EXIST=Get_NewSig)
  IF (Get_NewSig) THEN
    WRITE (*, '(/,A)') ' Getting New Delta Sigma'
    OPEN  (15, FILE=TRIM(DirSig)//TRIM(DataSig), &
               STATUS='OLD', FORM='FORMATTED')
    READ  (15,'(5E16.8)') DelSigmaOut
    CLOSE (15)
    IF (KmaxOut == KmaxInp .AND. MAXVAL(ABS(DelSigmaOut-DelSigmaInp)) < 1.0E-04_Kr8) THEN
      WRITE (*, '(/,A)')   ' KmaxOut = KmaxInp And DelSima Is Quite The Same'
      WRITE (*, '(A,1PG12.5,/)') ' MAXVAL(ABS(DelSigmaOut-DelSigmaInp)) = ', &
                                   MAXVAL(ABS(DelSigmaOut-DelSigmaInp))
      SigInterOut=SigInterInp
      SigLayerOut=SigLayerInp
      DelSigmaOut=DelSigmaInp
    ELSE
      CALL New_Sigma
    END IF
  ELSE
  IF (KmaxOut /= KmaxInp) THEN
    WRITE (*, '(A,/)')   ' Error In Getting New Sigma: KmaxInp /= KmaxOut'
    WRITE (*, '(2(A,I5))') ' KmaxInp = ', KmaxInp, ' KmaxOut = ', KmaxOut
    STOP
  END IF
  SigInterOut=SigInterInp
  SigLayerOut=SigLayerInp
  DelSigmaOut=DelSigmaInp
  END IF

  IF (KmaxOut == KmaxInp .AND. &
     (.NOT.Get_NewTop .AND. .NOT.Smooth_Topo)) Vertical_Interp = .FALSE.

  IF (Vertical_Interp .OR. GrADS) THEN

    CALL IC_Recomposition

    IF (GrADS) THEN
      WRITE (Tdef(1: 2),'(I2.2)') DateCurrent(1)
      WRITE (Tdef(4: 5),'(I2.2)') DateCurrent(3)
      WRITE (Tdef(6: 8),'(A3)')   MonChar(DateCurrent(2))
      WRITE (Tdef(9:12),'(I4.4)') DateCurrent(4)
      CALL Get_GrADSInp
      IF (GrADSOnly) STOP ' GrADS Only'
    END IF

    CALL ClsMem_Recomposition

  END IF

  IF (Vertical_Interp) THEN

  WRITE (*, '(A,/)') 'Doing Vertical Interpolation'

  DO k=1,KmaxInp
    DO j=1,JmaxOut
      DO I=1,ImaxOut
        gPresInp(i,j,k)=gPsfcInp(i,j)*SigLayerInp(k)
      END DO
    END DO
  END DO

  CALL New_Ps

  DO k=1,KmaxOut
    DO j=1,JmaxOut
      DO I=1,ImaxOut
        gPresOut(i,j,k)=gPsfcOut(i,j)*SigLayerOut(k)
      END DO
    END DO
  END DO
  gTvirInp=gTvirInp/(1.0_Kr8+cTv*gSpHuInp(:,:,:,1))
  
  CALL Vert_Sigma_Inter (ImaxOut, JmaxOut, &
                         KmaxInp, KmaxOut, NTracers, &
                         gPresInp(:,:,:), gUvelInp(:,:,:), gVvelInp(:,:,:), &
                         gTvirInp(:,:,:), gSpHuInp(:,:,:,:), &
                         gPresOut(:,:,:), gUvelOut(:,:,:), gVvelOut(:,:,:), &
                         gTvirOut(:,:,:), gSpHuOut(:,:,:,:))

  gTvirInp=gTvirInp*(1.0_Kr8+cTv*gSpHuInp(:,:,:,1))
  gTvirOut=gTvirOut*(1.0_Kr8+cTv*gSpHuOut(:,:,:,1))

  CALL IC_Decomposition

  ELSE

  WRITE (*, '(A,/)') 'Doing Chopping'

  IF (.NOT.Get_NewTop .AND. .NOT.Smooth_Topo) &
  CALL Chop (qTopoInp(:), qTopoOut(:), MendInp, MendOut, Kdim)
  CALL Chop (qLnPsInp(:), qLnPsOut(:), MendInp, MendOut, Kdim)
  CALL Chop (qTvirInp, qTvirOut, MendInp, MendOut, KmaxOut)
  CALL Chop (qVortInp, qVortOut, MendInp, MendOut, KmaxOut)
  CALL Chop (qDivgInp, qDivgOut, MendInp, MendOut, KmaxOut)
  DO nt=1,NTracers
    CALL  Chop (qSpHuInp(:,:,nt), qSpHuOut(:,:,nt), MendInp, MendOut, KmaxOut)
  END DO

  END IF

  CALL IC_Write

  CALL Cls_Arrays

  CALL Cls_SpHuTracers


CONTAINS


SUBROUTINE GDAStoGANL

  IMPLICIT NONE

  CHARACTER (LEN=1), DIMENSION (32) :: label

  WRITE (*, '(/,A,/)') ' Getting GANL from GDAS NCEP File'

  OPEN (20, FILE=TRIM(DGDInp)//TRIM(GDASInp), &
            STATUS='OLD', FORM='UNFORMATTED')

  OPEN (25, FILE=TRIM(DirInp)//TRIM(DataInp), &
            STATUS='REPLACE', FORM='UNFORMATTED')

  ! Label (See NMC Office Note 85) (Not Used at CPTEC)
  READ (20) label

  ! Forecast Hour     - TimeOfDay
  ! Initial Hour      - DateInitial(1)
  ! Initial Month     - DateInitial(2)
  ! Initial Day       - DateInitial(3)
  ! Initial Year      - DateInitial(4)
  ! Sigma Interfaces  - SigInterInp
  ! Sigma Layers      - SigLayerInp
  READ (20) TimeOfDay, DateInitial, SigIInp, SigLInp

  ForecastDay=0
  DateCurrent=DateInitial
  ! Forecast Day      - ForecastDay
  ! Time of Day       - TimeOfDay
  ! Initial Date      - DateInitial
  ! Current Date      - DateCurrent
  WRITE (25) ForecastDay, TimeOfDay, DateInitial, &
             DateCurrent, SigIInp, SigLInp

  ! Spectral Coefficients of Orography (m)
  READ  (20) qWorkInp
  WRITE (25) qWorkInp

  ! Spectral coefficients of ln(Ps) (ln(hPa)/10)
  READ  (20) qWorkInp
  WRITE (25) qWorkInp

  ! Spectral Coefficients of Virtual Temp (K)
  DO k=1,KmaxInp
    READ  (20) qWorkInp
    WRITE (25) qWorkInp
  END DO

  ! Spectral Coefficients of Divergence and Vorticity (1/seg)
  DO k=1,KmaxInp
    READ  (20) qWorkInp
    WRITE (25) qWorkInp
    READ  (20) qWorkInp
    WRITE (25) qWorkInp
  END DO

  ! Spectral Coefficients of Specific Humidity (g/g)
  DO k=1,KmaxInp
    READ  (20) qWorkInp
    WRITE (25) qWorkInp
  END DO

  CLOSE(25)

  IF (Get_Ozone) THEN

    ! Spectral Coefficients of Ozone (?)
    OPEN (30, FILE=TRIM(DirInp)//TRIM(OzonInp), &
              STATUS='REPLACE', FORM='UNFORMATTED')
    DO k=1,KmaxInp
      READ  (20) qWorkInp
      WRITE (30) qWorkInp
    END DO
    CLOSE(30)
    NTracers=NTracers+1

    IF (Get_Tracers) THEN
      ! Spectral Coefficients of Tracers (?)
      OPEN (35, FILE=TRIM(DirInp)//TRIM(TracInp), &
                STATUS='REPLACE', FORM='UNFORMATTED')
      ios=0
      Tracer: DO
        DO k=1,KmaxInp
          READ (20, IOSTAT=ios) qWorkInp
          IF (ios /= 0) THEN
            IF (ios == -1) THEN
              WRITE (*, '(/,A,I5,A)') ' Found End of File - NTracers = ', &
                                        NTracers, '  in:'
            ELSE
              WRITE (*, '(/,A,I5,A)') ' Reading Error - ios = ', ios, '  in:'
            END IF
            WRITE (*, '(1X,A,/)') TRIM(DGDInp)//TRIM(GDASInp)
            EXIT Tracer
          END IF
          WRITE (35) qWorkInp
        END DO
        NTracers=NTracers+1
      END DO Tracer
      CLOSE(35)
    END IF

  END IF

  CLOSE(20)

  IF (GDASOnly) STOP ' GDASOnly = .TRUE. '

END SUBROUTINE GDAStoGANL


SUBROUTINE IC_Read

  IMPLICIT NONE

  write(*,*) TRIM(DirInp)//TRIM(DataInp)
  OPEN (40, FILE=TRIM(DirInp)//TRIM(DataInp), &
            STATUS='OLD', FORM='UNFORMATTED')

  READ (40) ForecastDay, TimeOfDay, DateInitial, &
            DateCurrent, SigIInp, SigLInp

  SigInterInp=SigIInp
  SigLayerInp=SigLInp
  DO k=1,KmaxInp
    DelSigmaInp(k)=SigInterInp(k)-SigInterInp(k+1)
  END DO

  WRITE (*, '(/,A,I5,A,F15.4)') ' ForecastDay = ', ForecastDay, &
                                ' TimeOfDay = ', TimeOfDay
  WRITE (*, '(/,A,4I5)') ' DateInitial = ', DateInitial
  WRITE (*, '(/,A,4I5)') ' DateCurrent = ', DateCurrent
  WRITE (*, '(/,A)')  ' DelSigmaInp:'
  WRITE (*, '(7F10.6)') DelSigmaInp
  WRITE (*, '(/,A)')  ' SigInterInp:'
  WRITE (*, '(7F10.6)') SigInterInp
  WRITE (*, '(/,A)')  ' SigLayerInp:'
  WRITE (*, '(7F10.6)') SigLayerInp
  WRITE (*, '(A)') ' '

  READ (40) qWorkInp
  qTopoInp(:)=qWorkInp
  WRITE (*, '(/,A)') ' TopoInp:'
  WRITE (*, *) qTopoInp(1), MINVAL(qTopoInp(2:)), MAXVAL(qTopoInp(2:))

  READ (40) qWorkInp
  qLnPsInp(:)=qWorkInp
  WRITE (*, '(/,A)') ' LnPsInp:'
  WRITE (*, *) qLnPsInp(1), MINVAL(qLnPsInp(2:)), MAXVAL(qLnPsInp(2:))

  WRITE (*, '(/,A)') ' TvirInp:'
  DO k=1,KmaxInp
    READ (40) qWorkInp
    qTvirInp(:,k)=qWorkInp
    WRITE (*, *) k,qTvirInp(1,k), MINVAL(qTvirInp(2:,k)), MAXVAL(qTvirInp(2:,k))
  END DO

  WRITE (*, '(/,A)') ' DivgInp - VortInp:'
  DO k=1,KmaxInp
    READ (40) qWorkInp
    qDivgInp(:,k)=qWorkInp
    WRITE (*, *) k,qDivgInp(1,k), MINVAL(qDivgInp(2:,k)), MAXVAL(qDivgInp(2:,k))
    READ (40) qWorkInp
    qVortInp(:,k)=qWorkInp
    WRITE (*, *) k,qVortInp(1,k), MINVAL(qVortInp(2:,k)), MAXVAL(qVortInp(2:,k))
  END DO

  WRITE (*, '(/,A)') ' SpHuInp:'
  DO k=1,KmaxInp
    READ (40) qWorkInp
    qSpHuInp(:,k,1)=qWorkInp
    WRITE (*, *) k,qSpHuInp(1,k,1), MINVAL(qSpHuInp(2:,k,1)), MAXVAL(qSpHuInp(2:,k,1))
  END DO

  CLOSE (40)

  IF (Get_Ozone) THEN

    OPEN (45, FILE=TRIM(DirInp)//TRIM(OzonInp), &
              STATUS='Old', FORM='UNFORMATTED')
    nt=2
    DO k=1,KmaxInp
      READ (45) qWorkInp
      qSpHuInp(:,k,nt)=qWorkInp
    END DO
    CLOSE(45)

    IF (Get_Tracers) THEN
      OPEN (50, FILE=TRIM(DirInp)//TRIM(TracInp), &
                STATUS='OLD', FORM='UNFORMATTED')
      Do nt=3,NTracers
        DO k=1,KmaxInp
          READ (50) qWorkInp
          qSpHuInp(:,k,nt)=qWorkInp
        END DO
      END DO
      CLOSE(50)
    END IF

  END IF

END SUBROUTINE IC_Read


SUBROUTINE IC_Write

  IMPLICIT NONE

  OPEN (55, FILE=TRIM(DirOut)//TRIM(DataOut), &
            STATUS='REPLACE', FORM='UNFORMATTED')

  SigIOut=SigInterOut
  SigLOut=SigLayerOut

  WRITE (55) ForecastDay, TimeOfDay, DateInitial, &
             DateCurrent, SigIOut, SigLOut

  WRITE (*, '(/,A,I5,A,F15.4)') ' ForecastDay = ', ForecastDay, &
                                ' TimeOfDay = ', TimeOfDay
  WRITE (*, '(/,A,4I5)') ' DateInitial = ', DateInitial
  WRITE (*, '(/,A,4I5)') ' DateCurrent = ', DateCurrent
  WRITE (*, '(/,A)')  ' DelSigmaOut:'
  WRITE (*, '(7F10.6)') DelSigmaOut
  WRITE (*, '(/,A)')  ' SigInterOut:'
  WRITE (*, '(7F10.6)') SigInterOut
  WRITE (*, '(/,A)')  ' SigLayerOut:'
  WRITE (*, '(7F10.6)') SigLayerOut
  WRITE (*, '(A)') ' '

  qWorkOut=qTopoOut(:)
  WRITE (55) qWorkOut
  WRITE (*, '(/,A)') ' TopoOut:'
  WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))

  qWorkOut=qLnPsOut(:)
  WRITE (55) qWorkOut
  WRITE (*, '(/,A)') ' LnPsOut:'
  WRITE (*, *) qLnPsOut(1), MINVAL(qLnPsOut(2:)), MAXVAL(qLnPsOut(2:))

  WRITE (*, '(/,A)') ' TvirOut:'
  DO k=1,KmaxOut
    qWorkOut=qTvirOut(:,k)
    WRITE (55) qWorkOut
    WRITE (*, *) k,qTvirOut(1,k), MINVAL(qTvirOut(2:,k)), MAXVAL(qTvirOut(2:,k))
  END DO

  WRITE (*, '(/,A)') ' DivgOut - VortOut:'
  DO k=1,KmaxOut
    qWorkOut=qDivgOut(:,k)
    WRITE (55) qWorkOut
    WRITE (*, *) k,qDivgOut(1,k), MINVAL(qDivgOut(2:,k)), MAXVAL(qDivgOut(2:,k))
    qWorkOut=qVortOut(:,k)
    WRITE (55) qWorkOut
    WRITE (*, *) k,qVortOut(1,k), MINVAL(qVortOut(2:,k)), MAXVAL(qVortOut(2:,k))
  END DO

  WRITE (*, '(/,A)') ' SpHuOut:'
  DO k=1,KmaxOut
    qWorkOut=qSpHuOut(:,k,1)
    WRITE (55) qWorkOut
    WRITE (*, *) k,qSpHuOut(1,k,1), MINVAL(qSpHuOut(2:,k,1)), MAXVAL(qSpHuOut(2:,k,1))
  END DO

  CLOSE(55)

  IF (Get_Ozone) THEN

    CALL Init_Recomposition (MendOut, ImaxOut, JmaxOut, KmaxOut)

    ALLOCATE (gOzonOut (ImaxOut,JmaxOut,KmaxOut))
    nt=2
    CALL Recomposition_Scalar (KmaxOut, qSpHuOut(:,:,nt), gOzonOut(:,:,:))

    OPEN (60, FILE=TRIM(DirOut)//TRIM(OzonOut), &
              STATUS='REPLACE', FORM='UNFORMATTED')
    DO k=1,KmaxOut
      gWorkOut=gOzonOut(:,:,k)
      WRITE (60) gWorkOut
    END DO
    CLOSE(60)
    DEALLOCATE (gOzonOut)

    IF (Get_Tracers) THEN

      ALLOCATE (gTracOut (ImaxOut,JmaxOut,KmaxOut,NTracers-2))
      DO nt=1,Ntracers-2
        CALL Recomposition_Scalar (KmaxOut, qSpHuOut(:,:,nt+2), gTracOut(:,:,:,nt))
      END DO

      OPEN (65, FILE=TRIM(DirOut)//TRIM(TracOut), &
                STATUS='REPLACE', FORM='UNFORMATTED')
      DO nt=1,Ntracers-2
        DO k=1,KmaxOut
          gWorkOut=gTracOut(:,:,k,nt)
          WRITE (65) gWorkOut
        END DO
      END DO
      CLOSE(65)
      DEALLOCATE (gTracOut)
    END IF

    CALL ClsMem_Recomposition

  END IF

END SUBROUTINE IC_Write


SUBROUTINE IC_Recomposition

  IMPLICIT NONE

  CALL Init_Recomposition (MendInp, ImaxOut, JmaxOut, KmaxInp)

  CALL Recomposition_Scalar (Kdim, qTopoInp(:), gTopoInp(:,:))
  IF (Get_NewTop .OR. Smooth_Topo) THEN
    WRITE (*, '(/,A)') ' TopoOut before Chopping:'
    WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))
    CALL Chop (qTopoOut(:), qTopoRec(:), MendOut, MendInp, Kdim)
    WRITE (*, '(/,A)') ' TopoRec after Chopping:'
    WRITE (*, *) qTopoRec(1), MINVAL(qTopoRec(2:)), MAXVAL(qTopoRec(2:))
    CALL Recomposition_Scalar (Kdim, qTopoRec(:), gTopoOut(:,:))
    gTopoDel=gTopoOut-gTopoInp

    gWorkOut=gTopoInp(:,:)
    WRITE (77) gWorkOut
    gWorkOut=gTopoOut(:,:)
    WRITE (77) gWorkOut
    gWorkOut=gTopoDel(:,:)
    WRITE (77) gWorkOut
  ELSE
    gTopoOut=gTopoInp
  END IF

  CALL Recomposition_Scalar (Kdim, qLnPsInp(:), gLnPsInp(:,:))
  gPsfcInp=10.0_Kr8*EXP(gLnPsInp)

  CALL Recomposition_Scalar (KmaxInp, qTvirInp, gTvirInp)

  CALL Recomposition_Scalar (KmaxInp, qDivgInp, gDivgInp)

  CALL Recomposition_Scalar (KmaxInp, qVortInp, gVortInp)

  DO nt=1,Ntracers
    CALL Recomposition_Scalar (KmaxInp, qSpHuInp(:,:,nt), gSpHuInp(:,:,:,nt))
  END DO

  CALL DivgVortToUV (qDivgInp, qVortInp, qUvelInp, qVvelInp)
  CALL Recomposition_Vector (KmaxInp, qUvelInp, gUvelInp)
  DO k=1,KmaxInp
    DO j=1,JmaxOut
      DO i=1,ImaxOut
        gUvelInp(i,j,k)=gUvelInp(i,j,k)/coslat(j)
      END DO
    END DO
  END DO
  CALL Recomposition_Vector (KmaxInp, qVvelInp, gVvelInp)
  DO k=1,KmaxInp
    DO j=1,JmaxOut
      DO i=1,ImaxOut
        gVvelInp(i,j,k)=gVvelInp(i,j,k)/coslat(j)
      END DO
    END DO
  END DO

END SUBROUTINE IC_Recomposition


SUBROUTINE IC_Decomposition

  IMPLICIT NONE

  CALL Init_FFT
  CALL Init_Legendre

  WRITE (*, '(/,A)') ' TopoOut before Dec:'
  WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))
  CALL Dec_to_Spher_Harm (gTopoOut, qTopoOut, Kdim)
  WRITE (*, '(/,A)') ' TopoOut after Dec:'
  WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))
  CALL Trans_Spher_Harm (Kdim,    qTopoOut, ICaseDec)
  WRITE (*, '(/,A)') ' TopoOut after Trans:'
  WRITE (*, *) qTopoOut(1), MINVAL(qTopoOut(2:)), MAXVAL(qTopoOut(2:))

  CALL Dec_to_Spher_Harm (gLnPsOut, qLnPsOut, Kdim)
  CALL Trans_Spher_Harm (Kdim,    qLnPsOut, ICaseDec)

  CALL Dec_to_Spher_Harm (gTvirOut, qTvirOut, KmaxOut)
  CALL Trans_Spher_Harm (KmaxOut, qTvirOut, ICaseDec)

  DO nt=1,NTracers
    CALL Dec_to_Spher_Harm (gSpHuOut(:,:,:,nt), qSpHuOut(:,:,nt), KmaxOut)
    CALL Trans_Spher_Harm (KmaxOut, qSpHuOut(:,:,nt), ICaseDec)
  END DO

  CALL UVtoDivgVort (gUvelOut, gVvelOut, qDivgOut, qVortOut)
  CALL Trans_Spher_Harm (KmaxOut, qDivgOut, ICaseDec)
  CALL Trans_Spher_Harm (KmaxOut, qVortOut, ICaseDec)

  CALL ClsMem_FFT
  CALL ClsMem_Legendre

END SUBROUTINE IC_Decomposition


SUBROUTINE Chop (qInp, qOut, MendI, MendO, Kmax)

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT(IN) :: MendI, MendO, Kmax

  REAL (KIND=Kr8), DIMENSION ((MendI+1)*(MendI+2), Kmax), INTENT(IN) :: qInp

  REAL (KIND=Kr8), DIMENSION ((MendO+1)*(MendO+2), Kmax), INTENT(Out) :: qOut

  INTEGER (KIND=Ki4) :: n, ma, mb, mai, mbi, maf, mbf, maif

  IF (Kmax /= 1 .AND. KmaxInp /= KmaxOut) THEN
    WRITE (*, '(A,/))')   ' Error In SUBROUTINE Chop: KmaxInp /= KmaxOut'
    WRITE (*, '(2(A,I5))') ' KmaxInp = ', KmaxInp, ' KmaxOut = ', KmaxOut
    STOP
  END IF

  qOut=0.0_Kr8

  IF (MendO == MendI) THEN

    WRITE (*, '(/,A,1PG12.4,/)') ' SUBROUTINE Chop: MendO == MendI, qInp(1,1) = ', qInp(1,1)
    qOut=qInp

  ELSE
 
    IF (MendO < MendI) THEN
      WRITE (*, '(/,A,1PG12.4,/)') ' SUBROUTINE Chop: MendO < MendI, qInp(1,1) = ', qInp(1,1)
      DO k=1,Kmax
        mbf=0
        maif=0
        DO n=1,MendO+1
          mb=2*(MendO+2-n)
          ma=2*(MendI+2-n)
          mbi=mbf+1
          mbf=mbi+mb-1
          mai=maif+1
          maif=mai+ma-1
          maf=maif-(ma-mb)
          qOut(mbi:mbf,k)=qInp(mai:maf,k)
        END DO
      END DO
    ELSE
      WRITE (*, '(/,A,/)') ' SUBROUTINE Chop: MendO > MendI'
      DO k=1,Kmax
        mbf=0
        maif=0
        DO n=1,MendI+1
          ma=2*(MendO+2-n)
          mb=2*(MendI+2-n)
          mai=maif+1
          maif=mai+ma-1
          maf=maif-(ma-mb)
          mbi=mbf+1
          mbf=mbi+mb-1
          qOut(mai:maf,k)=qInp(mbi:mbf,k)
        END DO
      END DO
    END IF

  END IF

END SUBROUTINE Chop


SUBROUTINE Smooth_Coef (qOut, MendO, Kmax, MendC)

! Smoothes Spherical Harmonics Coefficients
!          Using Hoskin's(?) Filter

  IMPLICIT NONE

  INTEGER (KIND=Ki4), INTENT (IN) :: MendO, Kmax, MendC

  REAL (KIND=Kr8), DIMENSION ((MendO+1)*(MendO+2),Kmax), INTENT (IN OUT) :: qOut

  INTEGER (KIND=Ki4) :: k, mn, nn, mm, Mmax, mns

  REAL (KIND=Kr8) :: rm, rn, cx, red, rmn, ck

  REAL (KIND=Kr8), DIMENSION ((MendO+1)*(MendO+2)) :: Weight

  INTEGER (KIND=Ki4) :: icall=0
  SAVE :: icall

  WRITE (*, '(/,A,I5)') ' Doing Smoothing : ', icall

  IF (icall == 0) THEN
    rm=REAL(2*MendC)*REAL(2*MendC+1)
    rn=REAL(MendC-1)*REAL(MendC)
    red=(Smth_Per_Cut)**(-(rm*rm)/(rn*rn)/Iter)
    WRITE (*, '(/,A,F10.4)') ' From Smooth_Coef: Red = ', red
    cx=-LOG(red)/(rm*rm)
    mn=0
    DO nn=1,MendO+1
      Mmax=MendO-nn+2
      DO mm=1,Mmax
        mn=mn+1
        mns=mm+nn-1
        rmn=REAL(mns-1)*REAL(mns)
        ck=(EXP(cx*rmn*rmn))**Iter
        Weight(2*mn-1)=ck
        Weight(2*mn  )=ck
      END DO
    END DO
    icall=icall+1
  END IF

  DO k=1,Kmax
    DO mn=1,(MendO+1)*(MendO+2)
      qOut(mn,k)=qOut(mn,k)*Weight(mn)
    END DO
  END DO

END SUBROUTINE Smooth_Coef


SUBROUTINE Get_GrADSInp

  IMPLICIT NONE

  WRITE (*, '(/,A,L6,/)') ' GrADS     = ', GrADS
  WRITE (*, '(/,A,L6,/)') ' GrADSOnly = ', GrADSOnly

  OPEN (70, FILE=TRIM(DirGrd)//TRIM(DataOut)//'.GrADS', &
            STATUS='REPLACE', FORM='UNFORMATTED', &
            ACCESS='DIRECT', RECL=ImaxOut*JmaxOut)
  gWorkOut=gTopoOut(:,:)
  WRITE (70, REC=1) gWorkOut
  gWorkOut=gPsfcInp(:,:)
  WRITE (70, REC=2) gWorkOut
  DO k=1,KmaxInp
    gWorkOut=gTvirInp(:,:,k)
    WRITE (70, REC=2+k) gWorkOut
  END DO
  DO k=1,KmaxInp
    gWorkOut=gDivgInp(:,:,k)
    WRITE (70, REC=2+KmaxInp+k) gWorkOut
  END DO
  DO k=1,KmaxInp
    gWorkOut=gVortInp(:,:,k)
    WRITE (70, REC=2+2*KmaxInp+k) gWorkOut
  END DO
  DO k=1,KmaxInp
    gWorkOut=gSpHuInp(:,:,k,1)
    WRITE (70, REC=2+3*KmaxInp+k) gWorkOut
  END DO
  DO k=1,KmaxInp
    gWorkOut=gUvelInp(:,:,k)
    WRITE (70, REC=2+4*KmaxInp+k) gWorkOut
  END DO
  DO k=1,KmaxInp
    gWorkOut=gVvelInp(:,:,k)
    WRITE (70, REC=2+5*KmaxInp+k) gWorkOut
  END DO
  IF (Get_Ozone) THEN
    nt=2
    DO k=1,KmaxInp
      gWorkOut=gSpHuInp(:,:,k,nt)
      WRITE (70, REC=2+6*KmaxInp+k) gWorkOut
    END DO
  END IF
  IF (Get_Tracers) THEN
    DO nt=3,NTracers
      DO k=1,KmaxInp
        gWorkOut=gSpHuInp(:,:,k,nt)
        WRITE (70, REC=2+(4+nt)*KmaxInp+k) gWorkOut
      END DO
    END DO
  END IF
  CLOSE (70)

  OPEN  (75, FILE=TRIM(DirGrd)//TRIM(DataOut)//'.GrADS.ctl', &
             STATUS='REPLACE')
  WRITE (75, '(A)') 'dset ^'//TRIM(DataOut)//'.GrADS'
  WRITE (75, '(A)') 'options yrev big_endian'
  WRITE (75, '(A)') 'undef 1e20'
  WRITE (75, '(A,I5,A,2F12.6)') 'xdef ',ImaxOut,' linear ', &
                                 0.0, 360.0/REAL(ImaxOut)
  WRITE (75, '(A,I5,A)') 'ydef ',JmaxOut,' levels '
  WRITE (75, '(6F10.3)') glat(JmaxOut:1:-1)
  WRITE (75, '(A,I5,A)') 'zdef ',KmaxInp,' levels '
  WRITE (75, '(6F10.3)') 1000.0*SigLayerInp
  WRITE (75, '(3A)') 'tdef 1 linear ', Tdef,' 1dy'
  IF (NTracers == 1) THEN
    WRITE (75, '(A)') 'vars 8'
  ELSE
    WRITE (75, '(A,I5)') 'vars ',7+NTracers
  END IF
  WRITE (75, '(A)') 'topo   0 99 Topography        '//TruncInp//' (m)'
  WRITE (75, '(A)') 'pslc   0 99 Surface Pressure  '//TruncInp//' (hPa)'
  WRITE (75, '(A,I3,A)') 'tvir ',KmaxInp,' 99 Virt Temperature  '// &
                          TruncInp//' (K)'
  WRITE (75, '(A,I3,A)') 'divg ',KmaxInp,' 99 Divergence        '// &
                          TruncInp//' (1/s)'
  WRITE (75, '(A,I3,A)') 'vort ',KmaxInp,' 99 Vorticity         '// &
                          TruncInp//' (1/s)'
  WRITE (75, '(A,I3,A)') 'umes ',KmaxInp,' 99 Specific Humidity '// &
                          TruncInp//' (kg/kg)'
  WRITE (75, '(A,I3,A)') 'uvel ',KmaxInp,' 99 Zonal Wind        '// &
                          TruncInp//' (m/s)'
  WRITE (75, '(A,I3,A)') 'vvel ',KmaxInp,' 99 Meridional Wind   '// &
                          TruncInp//' (m/s)'
  IF (Get_Ozone) THEN
    WRITE (75, '(A,I3,A)') 'ozon ',KmaxInp,' 99 Ozone             '// &
                            TruncInp//' (?)'
  END IF
  IF (Get_Tracers) THEN
    DO nt=3,NTracers
      WRITE (75, '(A,I1,A,I3,A)') 'trc',nt-2,' ',KmaxInp, &
                                  ' 99 Tracer            '//TruncInp//' (?)'
    END DO
  END IF
  WRITE (75, '(A)') 'endvars'
  CLOSE (75)

END SUBROUTINE Get_GrADSInp


END PROGRAM Chopping
