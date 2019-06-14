!
!  $Author: pkubota $
!  $Date: 2007/08/14 16:53:45 $
!  $Revision: 1.5 $
!
MODULE PostLoop

  USE Constants, ONLY : r8, nferr, nfprt, nfpos, nfctl, nfdir, nfrfd, &
                        ndv, mdf, kdv, nvv, iclcd, nudv, nureq, chrdv, chreq

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: postgl

  INTEGER, PARAMETER :: ndi=250
  INTEGER, PARAMETER :: ndp=ndi+ndv!207

  INTEGER :: nharm(ndi) ! either ngaus or Mnwv2
  INTEGER :: nlevs(ndi) ! number of levels for this field
  INTEGER :: ifldcd(ndi)! ????
  INTEGER :: mkdir(ndi)
  INTEGER :: nuc(ndp)
  INTEGER :: mko(ndp)
  INTEGER :: ife(ndp)
  INTEGER :: nvo(ndp)
  INTEGER :: nuco(ndp)
  INTEGER :: nfe(ndp)
  INTEGER :: jfe(ndp)
  INTEGER :: mkdv(ndv)
  INTEGER :: lif(ndv)

  CHARACTER (LEN=4)  :: alias(ndp)
  CHARACTER (LEN=4)  :: aliop(ndp)
  CHARACTER (LEN=4)  :: prodia(ndi) ! either PROG or DIAG
  CHARACTER (LEN=40)  :: chrdsc(ndi) ! input file field name
  CHARACTER (LEN=40)  :: chrdo(ndp)
  CHARACTER (LEN=40)  :: chrop(ndp)


CONTAINS


  SUBROUTINE postgl (nFFrs, nFBeg, nFEnd, nFile)

    USE Constants, ONLY : Undef, Binary
    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, Mnwv2, ngaus, pmand, alnpmd
    USE FileAccess, ONLY : ReadHeader, WriteField
    USE FileGrib, ONLY : GDSPDSSETION, WriteGrbField
    USE GaussRep, ONLY : glat
    USE SigmaToPressure, ONLY : gavint
    USE GaussSigma, ONLY : setsig
    USE TransSpectralGrid, ONLY : rectrg
    USE FileAccess, ONLY : opnpos, CloseFiles, skipf, ReadField
    USE Conversion, ONLY : GiveUnit
    USE Util, ONLY : scase
    USE RegInterp, ONLY : Idim=>IdimOut, Jdim=>JdimOut, mgaus, &
                          RegInt, DoAreaInterpolation,DoAreaGausInterpolation
    USE tables, ONLY: table1,table2,table3,size_tb
    USE infgdspds, only : hh_fct,dd_fct,mm_fct,yy_fct,hh_anl, dd_anl, mm_anl, yy_anl

    IMPLICIT NONE

    ! reads spectral forecast coefficients of topography,
    ! log of sfc pressure, temperature, divergence, vorticity
    ! and humidity in sigma layers. converts these values
    ! to selected mandatory pressure levels.

    INTEGER, INTENT(IN) :: nFFrs
    INTEGER, INTENT(IN) :: nFBeg
    INTEGER, INTENT(IN) :: nFEnd
    INTEGER, INTENT(IN) :: nFile

    INTEGER :: indate(4)
    INTEGER :: idate(4)
    INTEGER :: idatec(4)
    INTEGER :: nfld
    INTEGER :: nflp
    INTEGER :: nof
    INTEGER :: kp
    INTEGER :: ihr
    INTEGER :: iday
    INTEGER :: month
    INTEGER :: iyr
    INTEGER :: kfld
    INTEGER :: kflo
    INTEGER :: ks
    INTEGER :: ifday
    INTEGER :: Ldim

    INTEGER :: kpds(200),kgds(200)
    INTEGER :: ierr

    REAL (KIND=r8) :: fhour
    REAL (KIND=r8) :: hr
    REAL (KIND=r8) :: tod
    REAL (KIND=r8) :: psmb(Imax,Jmax)
    REAL (KIND=r8) :: gz(Mnwv2)
    REAL (KIND=r8) :: ga(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: di(Mnwv2,Kmax)
    REAL (KIND=r8) :: del(Kmax) !1st written by recon
    REAL (KIND=r8) :: og(Imax,Jmax,Lmax) !intent(in) gavint intent(out) rwrite
    REAL (KIND=r8) :: qg(Imax,Jmax) !intent(out) rwrite
    REAL (KIND=r8) :: top(Imax,Jmax), lsmk(Imax,Jmax)
    REAL (KIND=r8) :: topreg(Idim,Jdim), lsmkreg(Idim,Jdim)

    LOGICAL :: mean
    LOGICAL :: newday

    CHARACTER (LEN=4) :: dtin !irrelevant
    CHARACTER (LEN=10) :: labelp
    CHARACTER (LEN=40) :: specal !irrelevant
    CHARACTER (LEN=256) :: fname
    CHARACTER (LEN=4), PARAMETER :: diag='DIAG'
    CHARACTER (LEN=4), SAVE :: rcode='N000' !irrelevant
    CHARACTER (LEN=20), PARAMETER :: type='PRESSURE HISTORY    '
    CHARACTER (LEN=40), SAVE :: title='NEW POSTP RHOMB/TRIANG OUTPUT TEST      '
    CHARACTER (LEN=3), PARAMETER :: cmth(12)=(/&
         'JAN','FEB','MAR','APR','MAY','JUN', &
         'JUL','AUG','SEP','OCT','NOV','DEC'/)

    ! read input sigma file

    IF (nFBeg > nFile) GOTO 4500
    CALL opnpos (labelp)
    CALL recon (nfld, nflp, nof, indate, title, &
         specal, del, rcode, dtin, nFile)
    CALL setsig (del)
    IF (nfld == 0) GOTO 4500

    CALL ReadHeader (ifday, tod, idate, idatec)
    WRITE (UNIT=*, FMT='(A,I6,A,F8.1)') ' ForecastDay =', ifday, '  TimeOfDay =', tod
    WRITE (UNIT=*, FMT='(A,3I3,I5)')    ' InitialDate =', idate
    WRITE (UNIT=*, FMT='(A,3I3,I5)')    ' CurrentDate =', idatec
    fhour=REAL( ifday*24,r8) + tod/3600.0_r8

    if (binary)then
       CALL GeraBinCtl(12,fname,title,labelp,nof,ndp,nvo,aliop,chrop,nuco)
    else
       CALL GeraGribCtl(12,fname,title,labelp,nof,ndp,nvo,aliop)
    endif

    WRITE (UNIT=nfprt, FMT='(2(A,I5))') ' nFFrs = ', nFFrs,' nFEnd =', nFEnd
    WRITE (UNIT=nfprt, FMT='(2(A,I5))') ' nFile = ', nFile,' nFBeg =', nFBeg


    ihr=idatec(1)
    month=idatec(2)
    iday=idatec(3)
    iyr=idatec(4)
    hr=REAL(ihr,r8)+MOD(tod,3600.0_r8)/3600.0_r8
    WRITE (UNIT=*, FMT='(/,A,F6.2,2X,8I5,/)') &
         '  Hour = ', hr, indate, ihr, month, iday, iyr

    !
    !   for  Infgdspds module (common for GDSPDSSETION)
    !
    hh_fct=ihr
    dd_fct=iday
    mm_fct=month
    yy_fct=iyr
    hh_anl=idate(1)
    dd_anl=idate(3)
    mm_anl=idate(2)
    yy_anl=idate(4)

    CALL GDSPDSSETION (kgds, kpds )

    print*,'Dimensions: nx = ',Idim,' ny = ',Jdim
    print*,'LATITUDES = ',GLAT(1),GLAT(Jdim)

    ! read spectral coefficients of orography
    ! and land sea mask

    Ldim=1
    CALL ReadField (Mnwv2, Ldim, gz)
    CALL rectrg (Mnwv2, Ldim, gz, top)
    CALL ReadField (ngaus, Ldim, lsmk)
    IF (RegInt) THEN
       IF (Binary) THEN
          !
          ! write binary format
          !
          CALL DoAreaInterpolation (top, topreg)
          CALL WriteField (mgaus, topreg)
          CALL DoAreaInterpolation (lsmk, lsmkreg)
          CALL WriteField (mgaus, lsmkreg)
	  !
       ELSE
          !
          ! write grib format
          !
          CALL DoAreaInterpolation (top, topreg)
          CALL WriteGrbField ('TOPO',mgaus, kgds, kpds, topreg,Ldim)
          CALL DoAreaInterpolation (lsmk, lsmkreg)
          CALL WriteGrbField ('LSMK',mgaus, kgds, kpds, lsmkreg,Ldim)
          !       
       END IF	  
    ELSE
       IF (Binary) THEN
          !
          ! write binary format
          !
	  CALL DoAreaGausInterpolation(top, topreg)
          CALL WriteField (mgaus, topreg)
	  CALL DoAreaGausInterpolation(lsmk, lsmkreg)
          CALL WriteField (mgaus, lsmkreg)
          !
       ELSE
          !
          ! write grib format
          !
	  CALL DoAreaGausInterpolation(top, topreg)
          CALL WriteGrbField ('TOPO',mgaus, kgds, kpds, topreg,Ldim)
	  CALL DoAreaGausInterpolation(lsmk, lsmkreg)
          CALL WriteGrbField ('LSMK',mgaus, kgds, kpds, lsmkreg,Ldim)
          !
       ENDIF
    END IF

    ! compute sig to p on ggrid

    kfld=1
    kflo=1
    mean=.FALSE.
    newday=.TRUE.
    WRITE (UNIT=nfprt, FMT='(A,I5,2I3,F7.2)') ' Processing: ', iyr, month, iday, hr
    WRITE (UNIT=nfprt, FMT='(A,L6)') ' Starting postgg: Mean = ', mean
    CALL postgg (psmb, pmand, alnpmd, mean, kfld, kflo, newday, top)
    WRITE (UNIT=nfprt, FMT='(/,A,/)') ' postgg Finished'
    ! skip PROG fields
    DO kp=1,nfld
       IF (prodia(kp) == diag) EXIT
    ENDDO
    kp=kp-1
    ks=kfld
    ! loop over remaining PROG fields
    DO kfld=ks,kp
       WRITE (UNIT=nfprt, FMT='(A, I3)') ' Processing Input Field Number: ', kfld
       IF (mkdir(kfld)<1 .OR. mkdir(kfld)==100) THEN
          WRITE (UNIT=nfprt, FMT='(A,I5)') ' Prog skipf(nlevs(kfld)), nLevs = ',nlevs(kfld)
          CALL skipf (nlevs(kfld))
       ELSEIF (nharm(kfld) == ngaus) THEN
          CALL ReadField (ngaus, nlevs(kfld), ga)
          IF (MOD(mkdir(kfld),2) == 1) THEN
             CALL gavint (nlevs(kfld), nvo(kflo), ga, og, psmb, pmand)
             CALL rwrite (nvo(kflo), kflo,og)
             kflo=kflo+1
          ELSEIF (mkdir(kfld) > 1) THEN
             CALL scase (kflo, nlevs(kfld), nfe, iclcd, ga)
             CALL gavint (nlevs(kfld), nvo(kflo), ga, og, psmb, pmand)
             CALL rwrite (nvo(kflo), kflo, og)
             kflo=kflo+1
          ELSE
             WRITE (UNIT=nfprt, FMT='(A,I5)') ' Field Skipped, nLevs = ', nlevs(kfld)
          ENDIF
       ELSEIF (nharm(kfld) == Mnwv2) THEN
          CALL ReadField (Mnwv2, nlevs(kfld), di)
          WRITE (UNIT=nfprt, FMT='(A,L6)') ' Mean   = ', mean
          CALL stog (psmb, alnpmd, nlevs(kfld), mean, kflo, nof, kfld, di, og, qg)
          IF (MOD(mkdir(kfld),2) == 1) THEN
             !og USED
             CALL rwrite (nvo(kflo), kflo, og)
             kflo=kflo+1
          ENDIF
          IF (mkdir(kfld) > 1) THEN
             !qg USED
             CALL rwrite (nvo(kflo), kflo, qg) 
             kflo=kflo+1
          ENDIF
       ENDIF
    END DO
    kfld=kp+1
    IF (kfld > nfld) THEN
       WRITE (UNIT=nfprt, FMT='(/,A,I5,/)') ' Progs Finished, Diags Skipped for nFile = ', nFile
       GOTO 4500
    ENDIF
    IF (nFile <= 0) THEN
       WRITE (UNIT=nfprt, FMT='(/,A,I5,/)') ' Progs Finished, Diags Skipped for nFile = ', nFile
       GOTO 4500
    ENDIF
    WRITE (UNIT=nfprt, FMT='(/,A,/)') ' Progs Finished, Diags Starting ...'

    !     process diagnostic fields

    mean=.TRUE.
    WRITE (UNIT=nfprt, FMT='(A,L6)') ' Starting postgg: Mean = ', mean
    CALL postgg (psmb, pmand, alnpmd, mean, kfld, kflo, newday, top)
    WRITE (UNIT=nfprt, FMT='(/,A,/)') ' postgg Finished'
    ks=kfld
    !loop over remaining DIAG fields
    DO kfld=ks,nfld
       WRITE (UNIT=nfprt, FMT='(A, I3)') ' Processing Input Field Number: ', kfld
       IF (mkdir(kfld)<1 .OR. mkdir(kfld)==100) THEN
          WRITE (UNIT=nfprt, FMT='(A,I5)') ' Prog skipf(nlevs(kfld)), nLevs = ',nlevs(kfld)
          CALL skipf(nlevs(kfld))
       ELSEIF (nharm(kfld) == ngaus) THEN
          CALL ReadField (ngaus, nlevs(kfld), ga)
          IF (MOD(mkdir(kfld),2) == 1) THEN
             CALL gavint (nlevs(kfld), nvo(kflo), ga, og, psmb, pmand)
             CALL rwrite (nvo(kflo), kflo,og)
             kflo=kflo+1
          ENDIF
          IF (mkdir(kfld) > 1) THEN
             CALL scase (kflo, nlevs(kfld), nfe, iclcd, ga)
             CALL gavint (nlevs(kfld), nvo(kflo), ga, og, psmb, pmand)
             CALL rwrite (nvo(kflo), kflo, og)
             kflo=kflo+1
          ENDIF
       ELSEIF (nharm(kfld) == Mnwv2) THEN
          CALL ReadField (Mnwv2, nlevs(kfld), di)
          WRITE (UNIT=nfprt, FMT='(A,L6)') ' Mean   = ',mean
          CALL stog (psmb, alnpmd, nlevs(kfld), mean, kflo, nof, kfld, di, og, qg)
          IF (MOD(mkdir(kfld),2) == 1) THEN
             CALL rwrite (nvo(kflo), kflo, og) 
             kflo=kflo+1
          ENDIF
          IF (mkdir(kfld) > 1) THEN
             CALL rwrite (nvo(kflo), kflo, qg)
             kflo=kflo+1
          ENDIF
       ENDIF
    END DO
    WRITE (UNIT=nfprt, FMT='(/,A,/)') ' Diagnostics Completed ...'

    ! end of diagnostics

4500 CONTINUE

    CALL CloseFiles()

    IF(.NOT. Binary)THEN
       CALL BACLOSE(51,ierr)
       CALL W3TAGE('GCMPOST ')
    ENDIF

  END SUBROUTINE postgl


  SUBROUTINE postgg (psmb, pmand, alnpmd, mean, kfld, kflo, newday, top)

    USE Constants, ONLY : EMRad1
    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, ngaus, Jmaxhf, &
         Mend1, Mend2, Mnwv2, Mnwv3
    USE SpectralRep, ONLY : snnp1, dellnp
    USE GaussRep, ONLY : colrad
    USE FileAccess, ONLY : ReadField
    USE TransSpectralGrid, ONLY : dectrg, rectrg, rectrd
    USE Util, ONLY : uvtodz

    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: kfld
    INTEGER, INTENT(INOUT) :: kflo

    REAL (KIND=r8), INTENT(IN) :: pmand(Lmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)
    REAL (KIND=r8), INTENT(IN) :: top(Imax,Jmax)
    REAL (KIND=r8), INTENT(OUT) :: psmb(Imax,Jmax)

    LOGICAL, INTENT(IN) :: mean
    LOGICAL, INTENT(INOUT) :: newday

    INTEGER, PARAMETER :: nopf=25
    INTEGER :: id
    INTEGER :: i
    INTEGER :: kt
    INTEGER :: mm
    INTEGER :: ii
    INTEGER :: j
    INTEGER :: m
    INTEGER :: l
    INTEGER :: ll
    INTEGER :: nn
    INTEGER :: nmax
    INTEGER :: Ldim

    REAL (KIND=r8) :: di(Mnwv2,Kmax)
    REAL (KIND=r8) :: ze(Mnwv2,Kmax)
    REAL (KIND=r8) :: wd(Mnwv2,Lmax)
    REAL (KIND=r8) :: wz(Mnwv2,Lmax)
    REAL (KIND=r8) :: wk(Mnwv2,Lmax)
    REAL (KIND=r8) :: wl(Mnwv2,Lmax)
    REAL (KIND=r8) :: wq(Mnwv2)
    REAL (KIND=r8) :: coslai(Jmax)
    REAL (KIND=r8) :: dpdphi(Mnwv3)    
    REAL (KIND=r8) :: dpdlam(Mnwv2)   
    REAL (KIND=r8) :: ug(Imax,Jmax,Lmax)
    REAL (KIND=r8) :: vg(Imax,Jmax,Lmax)
    REAL (KIND=r8) :: og(Imax,Jmax,Lmax)
    REAL (KIND=r8) :: pscb(Imax,Jmax)      ! intent(out)
    REAL (KIND=r8) :: lnpscb(Imax,Jmax)    ! intent(out)

    LOGICAL :: timemean
    LOGICAL, SAVE :: first=.TRUE.
    LOGICAL, SAVE :: second=.FALSE.
    LOGICAL, SAVE :: dopf(nopf,2)

    CHARACTER (LEN=40), DIMENSION (5,2), SAVE :: inf
    CHARACTER (LEN=40), DIMENSION (nopf,2), SAVE :: opf

    CHARACTER (LEN=40), SAVE :: t2ps='TIME MEAN SURFACE PRESSURE              '

    inf(:,1) = (/ &
         'LN SURFACE PRESSURE                     ', &
         'DIVERGENCE                              ', &
         'VORTICITY                               ', &
         'SPECIFIC HUMIDITY                       ', &
         'VIRTUAL TEMPERATURE                     ' /)
    inf(:,2) = (/ &
         'TIME MEAN LN SURFACE PRESSURE           ', &
         'TIME MEAN DIVERGENCE                    ', &
         'TIME MEAN VORTICITY                     ', &
         'TIME MEAN SPECIFIC HUMIDITY             ', &
         'TIME MEAN VIRTUAL TEMPERATURE           ' /)

    opf(:,1) = (/ &
         'SURFACE PRESSURE                        ', &
         'MASK                                    ', &
         'SURFACE ZONAL WIND (U)                  ', &
         'ZONAL WIND (U)                          ', &
         'SURFACE MERIDIONAL WIND (V)             ', &
         'MERIDIONAL WIND (V)                     ', &
         'OMEGA                                   ', &
         'DIVERGENCE                              ', &
         'VORTICITY                               ', &
         'STREAM FUNCTION                         ', &
         'ZONAL WIND PSI                          ', &
         'MERIDIONAL WIND PSI                     ', &
         'VELOCITY POTENTIAL                      ', &
         'ZONAL WIND CHI                          ', &
         'MERIDIONAL WIND CHI                     ', &
         'VIRTUAL TEMPERATURE                     ', &
         'GEOPOTENTIAL HEIGHT                     ', &
         'SEA LEVEL PRESSURE                      ', &
         'SURFACE ABSOLUTE TEMPERATURE            ', &
         'ABSOLUTE TEMPERATURE                    ', &
         'SURFACE RELATIVE HUMIDITY               ', &
         'RELATIVE HUMIDITY                       ', &
         'SPECIFIC HUMIDITY                       ', &
         'INST. PRECIP. WATER                     ', &
         'POTENTIAL TEMPERATURE                   ' /)
    opf(:,2) = (/ &
         'TIME MEAN SURFACE PRESSURE              ', &
         'TIME MEAN MASK                          ', &
         'TIME MEAN SURFACE ZONAL WIND (U)        ', &
         'TIME MEAN ZONAL WIND (U)                ', &
         'TIME MEAN SURFACE MERIDIONAL WIND (V)   ', &
         'TIME MEAN MERIDIONAL WIND (V)           ', &
         'TIME MEAN DERIVED OMEGA                 ', &
         'TIME MEAN DIVERGENCE                    ', &
         'TIME MEAN VORTICITY                     ', &
         'TIME MEAN STREAM FUNCTION               ', &
         'TIME MEAN ZONAL WIND PSI                ', &
         'TIME MEAN MERIDIONAL WIND PSI           ', &
         'TIME MEAN VELOCITY POTENTIAL            ', &
         'TIME MEAN ZONAL WIND CHI                ', &
         'TIME MEAN MERIDIONAL WIND CHI           ', &
         'TIME MEAN VIRTUAL TEMPERATURE           ', &
         'TIME MEAN GEOPOTENTIAL HEIGHT           ', &
         'TIME MEAN SEA LEVEL PRESSURE            ', &
         'TIME MEAN SURFACE ABSOLUTE TEMPERATURE  ', &
         'TIME MEAN ABSOLUTE TEMPERATURE          ', &
         'TIME MEAN SURFACE RELATIVE HUMIDITY     ', &
         'TIME MEAN RELATIVE HUMIDITY             ', &
         'TIME MEAN SPECIFIC HUMIDITY             ', &
         'TIME MEAN PRECIP. WATER                 ', &
         'TIME MEAN POTENTIAL TEMPERATURE         ' /)

    timemean=.TRUE.
    IF (first) THEN

       IF (mean) THEN
          WRITE (UNIT=nferr, FMT='(A3L4)') &
                ' First, Second, Mean Inconsistent:',&
                  first, second, mean
          STOP 8100
       END IF
       id=1

       ! input field has to be complete and well ordered,

       kt=kfld
       DO mm=1,5
          IF (inf(mm,1) == chrdsc(kt)) THEN
             kt=kt+1
          ELSE
             WRITE (UNIT=nferr, FMT='(A,L2,2(A,I4),A40,/,40X,A40)') &
                   ' At Mean = ', mean, ' mm = ', mm, ' kt = ', kt, &
                   ' inf = ', inf(mm,id), ' chrdsc = ', chrdsc(kt)
             STOP 9100
          END IF
       END DO

       ! output field has to be well ordered.
       ! ignore undesired output fields.

       kt=kflo
       DO ii=1,nopf
          IF (opf(ii,1) == chrop(kt)) THEN
             dopf(ii,1)=.TRUE.
             kt=kt+1
          ELSE
             dopf(ii,1)=.FALSE.
          END IF
       END DO
       first=.FALSE.
       second=.TRUE.
       WRITE (UNIT=nfprt, FMT='(" dopf(,1) = ",50L2)') (dopf(i,1),i=1,nopf)

    ELSE IF (second) THEN
       IF (.NOT. newday) THEN
          IF (.NOT.mean) THEN
             WRITE (UNIT=nferr, FMT='(A3L4)') &
                   ' First, Second, Mean Inconsistent:',&
                     first, second, mean
             STOP 8100
          END IF

          id=2
          inf(1,2)=t2ps

          ! check that the time mean input field is complete and well ordered

          kt=kfld
          DO mm=1,5
             IF (inf(mm,2) == chrdsc(kt)) THEN
                kt=kt+1
             ELSE
                WRITE (UNIT=nfprt, FMT='(A)') ' Time Mean is not Available, Ignore it ...'
                timemean=.FALSE.
             END IF
          END DO

          ! check that the time mean output field is well ordered.
          ! ignore undesired output fields.

          kt=kflo
          DO ii=1,nopf
             IF (opf(ii,2) == chrop(kt)) THEN
                dopf(ii,2)=.TRUE.
                kt=kt+1
             ELSE
                dopf(ii,2)=.FALSE.
             END IF
          END DO
          WRITE (UNIT=nfprt, FMT='(" dopf(,2) = ",50L2)') (dopf(i,2),i=1,nopf)

       END IF
    END IF

    IF (timemean) THEN

       ! surface pressure

       IF (.NOT.mean) THEN

          ! case full field:
          ! read spectral ln surface pressure;
          ! legandre transform to grid;
          ! get surface pressure and convert from centibar to milibar

          id=1
          Ldim=1
          CALL ReadField (nharm(kfld), nlevs(kfld), wq)
          CALL rectrg (Mnwv2, Ldim, wq, pscb)
          DO j=1,Jmax
             DO i=1,Imax
                pscb(i,j)=EXP(pscb(i,j))
                psmb(i,j)=pscb(i,j)*10.0_r8
             END DO
          END DO
       ELSE

          ! case time mean:
          ! read grid surface pressure;
          ! convert from centibar to milibar and get log
          ! legendre transform from spectral to grid

          id=2
          Ldim=1
          CALL ReadField (nharm(kfld), nlevs(kfld), pscb)
          DO j=1,Jmax
             DO i=1,Imax
                psmb(i,j)=pscb(i,j)*10.0_r8
                lnpscb(i,j)=LOG(pscb(i,j))
             END DO
          END DO
          CALL dectrg (Ldim, lnpscb, wq)
       END IF

       ! get latitudinal and longitudinal derivatives of ln surface pressure

       CALL dellnp (wq, dpdphi, dpdlam)

       ! divergence

       kfld=kfld+1
       CALL ReadField (nharm(kfld), nlevs(kfld), di)

       ! vorticity

       kfld=kfld+1
       CALL ReadField (nharm(kfld), nlevs(kfld), ze)

       kfld=kfld+1
       IF (dopf(1,id) .OR. dopf(2,id)) THEN
       WRITE (UNIT=nfprt, FMT='(A)') ' postgg: ps and masK'

          ! generates mask array containing
          ! ones above surface and -ones below surface.

          DO l=1,Lmax
             DO j=1,Jmax
                DO i=1,Imax
                   IF (psmb(i,j) < pmand(l)) THEN
                      og(i,j,l)=-pmand(l)/psmb(i,j)
                   ELSE
                      og(i,j,l)=pmand(l)/psmb(i,j)
                   END IF
                END DO
             END DO
          END DO

          ! surface pressure

          IF (dopf(1,id)) THEN
             CALL rwrite (nvo(kflo), kflo, pscb)
             kflo=kflo+1
          END IF

          ! mask for vertical extrapolation inside terrain

          IF (dopf(2,id)) THEN
             CALL rwrite (nvo(kflo), kflo, og)
             kflo=kflo+1
          END IF

       END IF

       IF (dopf(3,id) .OR. dopf(4,id) .OR. dopf(5,id) .OR. &
            dopf(6,id) .OR. dopf(7,id) .OR. dopf(8,id) .OR. &
            dopf(10,id) .OR. dopf(11,id) .OR. dopf(12,id) .OR. &
            dopf(13,id) .OR. dopf(14,id) .OR. dopf(15,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' Winds: us, up, vs, vp and omega'
          CALL Winds (psmb, alnpmd, kflo, dopf, id, nvo, ndp, &
               dpdphi, dpdlam, di, ze, ug, vg, og)
       END IF

       ! this is a subset of the above condition with the exception of
       ! dopf(9,id)
       IF (dopf(8,id) .OR. dopf(9,id) .OR. &
            dopf(10,id) .OR. dopf(11,id) .OR. dopf(12,id) .OR. &
            dopf(13,id) .OR. dopf(14,id) .OR. dopf(15,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' uvtodz: div and vort'
          ! Recompute Divergence and Vorticy using U an V at p-levels.
          CALL uvtodz (ug, vg, wz, wd) 
          CALL rectrg (Mnwv2, Lmax, wd, ug)
          CALL rectrg (Mnwv2, Lmax, wz, vg)
       END IF

       ! horizontal divergence at p levels

       IF (dopf(8,id)) THEN
          CALL rwrite (nvo(kflo), kflo, ug)
          kflo=kflo+1
       END IF

       ! vertical componente of vorticity at p levels

       IF (dopf(9,id)) THEN
          CALL rwrite (nvo(kflo), kflo, vg)
          kflo=kflo+1
       END IF

       ! stream function at p levels

       IF (dopf(10,id) .OR. dopf(11,id) .OR. dopf(12,id)) THEN
          DO l=1,Lmax
             wk(1,l)=0.0_r8
             wk(2,l)=0.0_r8
             DO m=3,Mnwv2
                wk(m,l)=wz(m,l)*snnp1(m)
             END DO
          END DO
          IF (dopf(10,id)) THEN
             WRITE (UNIT=nfprt, FMT='(A)') ' postgg: psi'
             CALL rectrg (Mnwv2, Lmax, wk, ug)
             CALL rwrite (nvo(kflo), kflo, ug)
             kflo=kflo+1
          END IF
       END IF

       IF (dopf(11,id) .OR. dopf(12,id) .OR. &
            dopf(14,id) .OR. dopf(15,id)) THEN
          DO j=1,Jmaxhf
             coslai(j)=1.0_r8/SIN(colrad(j))
             coslai(Jmax+1-j)=coslai(j)
          END DO
       END IF

       ! zonal wind psi at p levels

       IF (dopf(11,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' postgg: upsi'
          DO l=1,Lmax
             DO m=1,Mnwv2
                wl(m,l)=-wk(m,l)*EMRad1
             END DO
          END DO
          CALL rectrd (Lmax, wl, ug)
          DO l=1,Lmax
             DO j=1,Jmax
                DO i=1,Imax
                   ug(i,j,l)=ug(i,j,l)*coslai(j)
                END DO
             END DO
          END DO
          CALL rwrite (nvo(kflo), kflo, ug)
          kflo=kflo+1
       END IF

       ! meridional wind psi at p levels

       IF (dopf(12,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' postgg: vpsi'
          DO l=1,Lmax
             ll=0
             DO mm=1,Mend1
                nmax=Mend2-mm
                DO nn=1,nmax
                   ll=ll+1
                   wl(2*ll-1,l)=-REAL(nn-1,r8)*wk(2*ll,l)*EMRad1
                   wl(2*ll,l)=+REAL(nn-1,r8)*wk(2*ll-1,l)*EMRad1
                END DO
             END DO
          END DO
          CALL rectrg (Mnwv2, Lmax, wl, ug)
          DO l=1,Lmax
             DO j=1,Jmax
                DO i=1,Imax
                   ug(i,j,l)=ug(i,j,l)*coslai(j)
                END DO
             END DO
          END DO
          CALL rwrite (nvo(kflo), kflo, ug)
          kflo=kflo+1
       END IF

       ! velocity potential at p levels

       IF (dopf(13,id) .OR. dopf(14,id) .OR. dopf(15,id)) THEN
          DO l=1,Lmax
             wk(1,l)=0.0_r8
             wk(2,l)=0.0_r8
             DO m=3,Mnwv2
                wk(m,l)=wd(m,l)*snnp1(m)
             END DO
          END DO
          IF (dopf(13,id)) THEN
             WRITE (UNIT=nfprt, FMT='(A)') ' postgg: chi'
             CALL rectrg (Mnwv2, Lmax, wk, vg)
             CALL rwrite (nvo(kflo), kflo, vg)
             kflo=kflo+1
          END IF
       END IF

       ! zonal wind chi at p levels

       IF (dopf(14,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' postgg: uchi'
          DO l=1,Lmax
             ll=0
             DO mm=1,Mend1
                nmax=Mend2-mm
                DO nn=1,nmax
                   ll=ll+1
                   wl(2*ll-1,l)=-REAL(nn-1,r8)*wk(2*ll,l)*EMRad1
                   wl(2*ll,l)=+REAL(nn-1,r8)*wk(2*ll-1,l)*EMRad1
                END DO
             END DO
          END DO
          CALL rectrg (Mnwv2, Lmax, wl, vg)
          DO l=1,Lmax
             DO j=1,Jmax
                DO i=1,Imax
                   vg(i,j,l)=vg(i,j,l)*coslai(j)
                END DO
             END DO
          END DO
          CALL rwrite (nvo(kflo), kflo, vg)
          kflo=kflo+1
       END IF

       ! meridional wind chi at p levels

       IF (dopf(15,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' postgg: vchi'
          DO l=1,Lmax
             DO m=1,Mnwv2
                wl(m,l)=wk(m,l)*EMRad1
             END DO
          END DO
          CALL rectrd (Lmax, wl, vg)
          DO l=1,Lmax
             DO j=1,Jmax
                DO i=1,Imax
                   vg(i,j,l)=vg(i,j,l)*coslai(j)
                END DO
             END DO
          END DO
          CALL rwrite (nvo(kflo), kflo, vg)
          kflo=kflo+1
       END IF

       CALL ReadField (nharm(kfld), nlevs(kfld), di)
       kfld=kfld+1
       CALL ReadField (nharm(kfld), nlevs(kfld), ze)
       kfld=kfld+1

       ! it is necessary to call Heights in the case of
       ! absolute temperature (dopf(20,id)),
       ! relative humidity (dopf(22,id)),
       ! specific humidity (dopf(23,id)) and
       ! to save the virtual temperature in  og
       ! that will be used in getsh called at Humidity

       IF (dopf(16,id) .OR. dopf(17,id) .OR. dopf(18,id) .OR. &
            dopf(20,id) .OR. dopf(22,id) .OR. dopf(23,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' Heights: Tv, zp and slp'
          CALL Heights (psmb, pmand, alnpmd, kflo, dopf, id, &
               nvo, ndp, ze, top, ug, vg, og)
       END IF

       ! the above includes 16, 17, 18
       ! the one below includes 19, 21, 24
       ! both include 20, 22, 23
       IF (dopf(19,id) .OR. dopf(20,id) .OR. dopf(21,id) .OR. &
            dopf(22,id) .OR. dopf(23,id) .OR. dopf(24,id)) THEN
          WRITE (UNIT=nfprt, FMT='(A)') ' Humidity: Ts, Tp, rhs, rhp, sh, pw and theta'
          CALL Humidity (psmb, pmand, alnpmd, kflo, dopf, id, &
               nvo, ndp, di, ze, ug, vg, og)
       END IF

       newday=.FALSE.

    END IF

  END SUBROUTINE postgg


  SUBROUTINE Winds (psmb, alnpmd, kflo, dopf, id, nvo, ndp, &
       dpdphi, dpdlam, di, ze, ug, vg, og)

    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, &
         Jmaxhf, Imx, Mnwv2, Mnwv3
    USE GaussRep, ONLY : colrad, rcs2
    USE GaussSigma, ONLY : omegas
    USE SpectralRep, ONLY : dztouv
    USE FFT, ONLY : InvFFT
    USE LegTrans, ONLY : Spec2Four
    USE SigmaToPressure, ONLY : sig2po

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ndp
    INTEGER, INTENT(IN) :: id
    INTEGER, INTENT(INOUT) :: kflo
    INTEGER, INTENT(IN) :: nvo(ndp)

    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)
    REAL (KIND=r8), INTENT(IN) :: dpdphi(Mnwv3)
    REAL (KIND=r8), INTENT(IN) :: dpdlam(Mnwv2)
    REAL (KIND=r8), INTENT(IN) :: di(Mnwv2,Kmax)
    REAL (KIND=r8), INTENT(IN) :: ze(Mnwv2,Kmax)
    REAL (KIND=r8), INTENT(OUT) :: ug(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: vg(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: og(Imax,Jmax,Lmax)

    LOGICAL, INTENT(IN) :: dopf(:,:)

    INTEGER :: j
    INTEGER :: l
    INTEGER :: i

    REAL (KIND=r8) :: coslat(Jmax)
    REAL (KIND=r8) :: rcl(Jmax)
    REAL (KIND=r8) :: uln(Mnwv3,Kmax)
    REAL (KIND=r8) :: vln(Mnwv3,Kmax)
    REAL (KIND=r8) :: four2D(Imx,Jmax)
    REAL (KIND=r8) :: four3D(Imx,Jmax,Kmax)
    REAL (KIND=r8) :: gdlam(Imax,Jmax)
    REAL (KIND=r8) :: gdphi(Imax,Jmax)
    REAL (KIND=r8) :: gdi(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: gus(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: gvs(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: gomeg(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: us(Imax,Jmax)       ! intent(out)  
    REAL (KIND=r8) :: vs(Imax,Jmax)       ! intent(out)

    uln=0.0_r8
    vln=0.0_r8

    CALL dztouv (di, ze, uln, vln)

    ! spectral to grid legendre transforms

    CALL Spec2Four (dpdlam, four2D)
    CALL InvFFT (four2D, gdlam)

    CALL Spec2Four (di, four3D)
    CALL InvFFT (four3D, gdi)

    CALL Spec2Four (dpdphi, four2D)
    CALL InvFFT (four2D, gdphi)

    CALL Spec2Four (uln, four3D)
    CALL InvFFT (four3D, gus)

    CALL Spec2Four (vln, four3D)
    CALL InvFFT (four3D, gvs)

    !cdir nodep
    DO j=1,Jmaxhf
       rcl(j)=rcs2(j)
       rcl(Jmax-j+1) = rcl(j)
       coslat(j)=1.0_r8/SIN(colrad(j))
       coslat(Jmax-j+1)=coslat(j)
    END DO
    DO j=1,Jmax
       DO i=1,Imax
          us(i,j)=gus(i,j,1)*coslat(j)
          vs(i,j)=gvs(i,j,1)*coslat(j)
       END DO
    END DO

    ! omegas calculates vertical motions.

    CALL omegas (gdphi, gdlam, gus, gvs, gdi, rcl, gomeg, psmb)

    ! sig2po performs vertical interpolation

    !ug,vg and og SET
    CALL sig2po (psmb, alnpmd, gus, ug, gvs, vg, gomeg, og)

    ! convert pseudo-wind to wind field

    DO l=1,Lmax
       DO j=1,Jmax
          DO i=1,Imax
             ug(i,j,l)=ug(i,j,l)*coslat(j)
             vg(i,j,l)=vg(i,j,l)*coslat(j)
          END DO
       END DO
    END DO

    ! zonal wind at first sigma layer

    IF (dopf(3,id)) THEN
       CALL rwrite (nvo(kflo), kflo, us)
       kflo=kflo+1
    END IF

    ! zonal wind at p levels

    IF (dopf(4,id)) THEN
       CALL rwrite (nvo(kflo), kflo, ug)
       kflo=kflo+1
    END IF

    ! meridional wind at first sigma layer

    IF (dopf(5,id)) THEN
       CALL rwrite (nvo(kflo), kflo, vs)
       kflo=kflo+1
    END IF

    ! meridional wind at p levels

    IF (dopf(6,id)) THEN
       CALL rwrite (nvo(kflo), kflo, vg)
       kflo=kflo+1
    END IF

    ! vertical p-velocity at p levels

    IF (dopf(7,id)) THEN
       CALL rwrite (nvo(kflo), kflo, og)
       kflo=kflo+1
    END IF

  END SUBROUTINE Winds


  SUBROUTINE Heights (psmb, pmand, alnpmd, kflo, dopf, id, &
       nvo, ndp, qtv, top, zp, tv, tvsav)

    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, ngaus, Imx, Mnwv2
    USE FFT, ONLY : InvFFT
    USE LegTrans, ONLY : Spec2Four
    USE SigmaToPressure, ONLY : sig2pz
    USE GaussPressure, ONLY : getslp, lowtmp

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ndp
    INTEGER, INTENT(IN) :: id
    INTEGER, INTENT(INOUT) :: kflo
    INTEGER, INTENT(IN) :: nvo(ndp)

    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN) :: pmand(Lmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)
    REAL (KIND=r8), INTENT(IN) :: qtv(Mnwv2,Kmax)
    REAL (KIND=r8), INTENT(IN) :: top(Imax,Jmax)
    REAL (KIND=r8), INTENT(OUT) :: zp(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: tv(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: tvsav(Imax,Jmax,Lmax)

    LOGICAL, INTENT(IN) :: dopf(:,:)

    INTEGER :: i, j, l

    REAL (KIND=r8) :: tc(Imax,Jmax,Lmax)
    REAL (KIND=r8) :: tf(Imx,Jmax,Kmax)
    REAL (KIND=r8) :: ts(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: slpg(Imax,Jmax)

    zp=0.0_r8
    tv=0.0_r8
    slpg=0.0_r8

    ! legendre transform from spectral to fourier
    ! fourier transform from fourier to grid

    CALL Spec2Four (qtv, tf)
    CALL InvFFT (tf, ts)

    CALL sig2pz (ts, psmb, top, tv, zp, alnpmd)

    ! calculation for below ground virtual temp. based on height
    ! and save virtual temperature at p levels for next routine

    CALL lowtmp (zp, tc, pmand)
    DO l=1,Lmax
       DO j=1,Jmax
          DO i=1,Imax
             IF (pmand(l) > psmb(i,j)) tv(i,j,l)=tc(i,j,l)
             tvsav(i,j,l)=tv(i,j,l)
          ENDDO
       ENDDO
    ENDDO

    ! virtual temperature at p levels

    IF (dopf(16,id)) THEN
       CALL rwrite (nvo(kflo), kflo, tv)
       kflo=kflo+1
    ENDIF

    ! geopotential height at p levels

    IF (dopf(17,id)) THEN
       CALL rwrite (nvo(kflo), kflo, zp)
       kflo=kflo+1
    ENDIF

    ! reduced sea level pressure

    IF (dopf(18,id)) THEN
       CALL getslp (slpg, zp, pmand)
       CALL rwrite (nvo(kflo), kflo, slpg)
       kflo=kflo+1
    ENDIF

  END SUBROUTINE Heights


  SUBROUTINE Humidity (psmb, pmand, alnpmd, kflo, dopf, id, &
       nvo, ndp, qsh, qtv, ta, rh, tv)

    USE Constants, ONLY : SHmin
    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, ngaus, Imx, Mnwv2
    USE FFT, ONLY : InvFFT
    USE LegTrans, ONLY : Spec2Four
    USE SigmaToPressure, ONLY : sigtop
    USE GaussPressure, ONLY : getth, getsh
    USE GaussSigma, ONLY : pwater

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ndp
    INTEGER, INTENT(IN) :: id
    INTEGER, INTENT(INOUT) :: kflo
    INTEGER, INTENT(IN) :: nvo(ndp)

    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN) :: pmand(Lmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)
    REAL (KIND=r8), INTENT(IN) :: qsh(Mnwv2,Kmax)
    REAL (KIND=r8), INTENT(IN) :: qtv(Mnwv2,Kmax)
    REAL (KIND=r8), INTENT(OUT) :: ta(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: rh(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(INOUT) :: tv(Imax,Jmax,Lmax)

    LOGICAL, INTENT(IN) :: dopf(:,:)

    INTEGER :: j, i

    REAL (KIND=r8) :: gta(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: gtv(Imx,Jmax,Kmax)
    REAL (KIND=r8) :: gsh(Imx,Jmax,Kmax)
    REAL (KIND=r8) :: gss(Imx,Jmax,Kmax)
    REAL (KIND=r8) :: four(Imx,Jmax,Kmax)
    REAL (KIND=r8) :: ts(Imax,Jmax)
    REAL (KIND=r8) :: rs(Imax,Jmax)
    REAL (KIND=r8) :: pw(Imax,Jmax)

    ta=0.0_r8
    rh=0.0_r8
    ts=0.0_r8
    rs=0.0_r8
    pw=0.0_r8

    ! legendre transforms from
    ! spectral to fourier representations

    CALL Spec2Four (qtv, four)
    CALL InvFFT (four, gtv)

    CALL Spec2Four (qsh, four)
    CALL InvFFT (four, gsh)
    gsh(:,:,:)=MAX(gsh(:,:,:),SHmin)

    ! compute precipitable water from specific humidity

    CALL pwater (Imx, Imax, Kmax, Jmax, gsh, pw, psmb)

    !     vertical interpolation.

    CALL sigtop (gta, gtv, gsh, gss, psmb, ta, rh, pmand, alnpmd)

    DO j=1,Jmax
       DO i=1,Imax
          ts(i,j)=gta(i,j,1)
          rs(i,j)=gss(i,j,1)
       END DO
    END DO

    ! getsh calculates specific humidity from relative humidity
    ! and virtual temperature saved in tv (tvsav) at subroutine Heights.

    IF (dopf(20,id) .OR. dopf(22,id) .OR. dopf(23,id)) THEN
       CALL getsh (tv, rh, pmand, ta, psmb)
    END IF

    ! absolute temperature of the first sigma layer

    IF (dopf(19,id)) THEN
       CALL rwrite (nvo(kflo), kflo, ts)
       kflo=kflo+1
    END IF

    ! absolute temperature at p levels

    IF (dopf(20,id)) THEN
       CALL rwrite (nvo(kflo), kflo, ta)
       kflo=kflo+1
    END IF

    ! relative humidity at first sigma layer

    IF (dopf(21,id)) THEN
       CALL rwrite (nvo(kflo), kflo, rs)
       kflo=kflo+1
    END IF

    ! relative humidity at p levels

    IF (dopf(22,id)) THEN
       CALL rwrite (nvo(kflo), kflo, rh)
       kflo=kflo+1
    END IF

    ! specific humidity at p levels

    IF (dopf(23,id)) THEN
       CALL rwrite (nvo(kflo), kflo, tv)
       kflo=kflo+1
    END IF

    ! precipitable water integrated over sigma layers

    IF (dopf(24,id)) THEN
       CALL rwrite (nvo(kflo), kflo, pw)
       kflo=kflo+1
    END IF

    ! potential temperature at p levels

    IF (dopf(25,id)) THEN
       CALL getth (tv, pmand, ta)
       CALL rwrite (nvo(kflo), kflo, tv)
       kflo=kflo+1
    END IF

  END SUBROUTINE Humidity


  SUBROUTINE stog (psmb, alnpmd, Ldim, mean, kflo, nof, kfld, di, og, qg)

    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, Kmaxp, Imx, Mnwv2, ijKmax
    USE FFT, ONLY : InvFFT
    USE LegTrans, ONLY : Spec2Four
    USE SigmaToPressure, ONLY : sig2po
    USE GaussSigma, ONLY : pwater
    USE Conversion, ONLY : cnvout

    IMPLICIT NONE

    ! general purpose single field spectral sigma to regular pressure
    ! conversion subroutine.  input array assumed to be in 1st slot

    INTEGER, INTENT(IN) :: Ldim
    INTEGER, INTENT(IN) :: kflo
    INTEGER, INTENT(IN) :: nof
    INTEGER, INTENT(IN) :: kfld

    REAL (KIND=r8), INTENT(IN) :: psmb(Imax,Jmax)
    REAL (KIND=r8), INTENT(IN) :: alnpmd(Lmax)
    REAL (KIND=r8), INTENT(IN) :: di(Mnwv2,Kmax)
    REAL (KIND=r8), INTENT(OUT) :: og(Imax,Jmax,Lmax)
    REAL (KIND=r8), INTENT(OUT) :: qg(Imax,Jmax)

    LOGICAL, INTENT(IN) :: mean

    INTEGER :: lout, j, l, i
    INTEGER :: jjj, mmm, nnn, mm1, nn1, jj1, nnx, k, ier

    REAL (KIND=r8) :: four(Imx,Jmax,Kmax)
    REAL (KIND=r8) :: gdi(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: zea(Imax,Jmax,Kmax)
    REAL (KIND=r8) :: ha(Imax,Jmax)
    REAL (KIND=r8) :: dip(Imax,Jmax,Lmax)

    IF (Ldim == 1) THEN
       lout=1
    ELSE
       lout=Lmax
    END IF

    qg=0.0_r8
    og(:,:,1:lout)=0.0_r8

    jjj=jfe(kflo)
    mmm=mko(jjj)
    nnn=ife(jfe(kflo))
    mm1=0
    nn1=1
    IF (mmm == 1 .AND. kflo < nof) THEN
       jj1=jfe(kflo+1)
       mm1=mko(jj1)
       nn1=ife(jj1)
       IF (kfld /= lif(nn1)) mm1=0
    END IF

    CALL Spec2Four (di, four)
    CALL InvFFT (four, gdi)

    IF (Ldim==Kmax .AND. mean) THEN

       IF (  (mmm==2 .AND. iclcd(nnn)==3)  .OR.&
            (mm1==2 .AND. iclcd(nn1)==3)) THEN
          IF (mmm==2) nnx=nnn
          IF (mm1==2) nnx=nn1
          IF (ifldcd(kfld) /= nureq(nnx,2)) THEN
             ier=0
             CALL cnvout (ijKmax, ifldcd(kfld), nureq(nnx,2), gdi, zea, ier)
             IF (ier > 0) WRITE (UNIT=nfprt, FMT='(6(A,I4))') &
                 ' Conversion Error at kflo = ', kflo, ' kfld = ', kfld, &
                 ' Error = ', ier, ' nfe = ', nnx, &
                 ' ifldcd = ', ifldcd(kfld), ' nureq = ', nureq(nnx,2)
          ELSE
             DO l=1,Kmax
                DO j=1,Jmax
                   DO i=1,Imax
                      zea(i,j,l)=gdi(i,j,l)
                   END DO
                END DO
             END DO
          END IF
          CALL pwater (Imax, Imax, Kmax, Jmax, zea, ha, psmb)
       END IF

       CALL sig2po (psmb, alnpmd, gdi, dip)
    ELSE
       DO j=1,Jmax
          DO i=1,Imax
             dip(i,j,1)=gdi(i,j,1)
          END DO
       END DO
    END IF


    IF ((mmm==2 .AND. iclcd(nnn)==4) .OR. &
         (mm1==2 .AND. iclcd(nn1)==4)) THEN
       DO j=1,Jmax
          DO i=1,Imax
             ha(i,j)=EXP(dip(i,j,1))
          END DO
       END DO
    END IF

    IF (mmm /= 2) THEN
       DO k=1,Lmax
          DO j=1,Jmax
             DO i=1,Imax
                og(i,j,k)=dip(i,j,k)
             END DO
          END DO
       END DO
    END IF

    IF ((mmm==2 .AND. iclcd(nnn)>=3) .OR. &
         (mm1==2 .AND. iclcd(nn1)>=3)) THEN
       DO j=1,Jmax
          DO i=1,Imax
             qg(i,j)=ha(i,j)
          END DO
       END DO
    END IF
    IF (ALL(qg == 0.0_r8) .AND. ALL(og(:,:,1:lout) == 0.0_r8)) &
       WRITE (UNIT=nferr, FMT=*) " stog: og and qg NOT SET"

  END SUBROUTINE stog


  SUBROUTINE recon (nfld, nflp, nof, indate, title, &
       specal, del, rcode, dtin, nFile)

    USE PrblSize, ONLY : Kmax, Lmax

    IMPLICIT NONE

    ! It is used to reconcile the differences between the
    ! input file directory and the requested field directory and
    ! produce a final output directory.  Input datasets are:
    ! 1.  input file directory on dataset unit nfdir
    !     order of fields in directory must correspond to fields
    !     in the input file
    ! 2.  requested field directory on dataset unit nfrfd
    !     order of fields is independent
    !
    ! Derived field table is at Constants Module
    ! order of fields to be derived must correspond to the
    ! order they are processed in the code.  order of fields
    ! required for deriving a field is independent


    INTEGER, INTENT(IN) :: nFile
    INTEGER, INTENT(OUT) :: nfld
    INTEGER, INTENT(OUT) :: nflp
    INTEGER, INTENT(OUT) :: nof
    INTEGER, INTENT(OUT) :: indate(4)

    REAL (KIND=r8), INTENT(OUT) :: del(Kmax)

    CHARACTER (LEN=4), INTENT(OUT) :: rcode !irrelevant
    CHARACTER (LEN=4), INTENT(OUT) :: dtin !irrelevant
    CHARACTER (LEN=40), INTENT(OUT) :: title
    CHARACTER (LEN=40), INTENT(OUT) :: specal

    INTEGER :: nwn !irrelevant
    INTEGER :: Mend1 ! irrelevant
    INTEGER :: levin ! # of values read into del(Kmax) == Kmax
    INTEGER :: levqin !irrelevant
    INTEGER :: in
    INTEGER :: kk
    INTEGER :: mm
    INTEGER :: ll
    INTEGER :: nn
    INTEGER :: mxdir
    INTEGER :: ii
    INTEGER :: jj
    INTEGER :: ifex
    INTEGER :: idate(4)

    CHARACTER (LEN=1) :: trunc !irrelevant
    CHARACTER (LEN=4) :: sdain !irrelevant
    CHARACTER (LEN=20) :: type
    CHARACTER (LEN=20) :: type1='COLA SIGMA HISTORY 4'
    CHARACTER (LEN=20) :: type2='CPTEC SIGMA VERS 2.0'

    ! read in input file directory

    READ (UNIT=nfdir, FMT='(A20)', END=4000) TYPE
    WRITE (UNIT=nfprt, FMT='(1X,A)') TYPE
    WRITE (UNIT=nfprt, FMT='(1X,A)') type1
    WRITE (UNIT=nfprt, FMT='(1X,A)') type2

    !only uses levin and indate
    READ (UNIT=nfdir, FMT='(A4,1X,A4,1X,A1,I3,1X,11I5,1X,A4)', END=4000) &
          rcode, sdain, trunc, nwn, Mend1, levin, levqin, idate, indate, dtin
    WRITE (UNIT=nfprt, FMT='(1X,A4,1X,A4,1X,A1,I3,1X,11I5,1X,A4)') &
           rcode, sdain, trunc, nwn, Mend1, levin, levqin, idate, indate, dtin
    READ (UNIT=nfdir, FMT='(2A40)', END=4000) title, specal
    WRITE (UNIT=nfprt, FMT='(1X,2A40)') title, specal
    READ (UNIT=nfdir, FMT='(5E16.8)', END=4000) (del(in),in=1,levin)
    WRITE (UNIT=nfprt, FMT='(1X,5E16.8)') (del(in),in=1,levin)

    kk=0
    nflp=0
    ! read in nfld field descriptions, skipping those where
    ! prodia == FIXD
    DO nfld=1,ndi
       DO
          READ (UNIT=nfdir, FMT='(A40,2X,A4,2X,I8,3X,I4,4X,I3)', END=30) &
                chrdsc(nfld), prodia(nfld), nharm(nfld), nlevs(nfld), ifldcd(nfld)
          IF (prodia(nfld) /= 'FIXD') EXIT
       END DO
    END DO
    WRITE (UNIT=nfprt, FMT='(3(A,I5),A,I3)') ' Directory Table Overflow.  nfld = ', &
           nfld, ' nflp = ', nflp, ' ndv = ',ndv, ' kk = ', kk
    STOP 3100
30  CONTINUE
    nfld=nfld-1
    mkdir(1:nfld)=0

    ! read in requested field directory

    DO nflp=1,ndp
       READ (UNIT=nfrfd, FMT='(A40,I5,1X,A4)', END=40) &
                   chrdo(nflp), nuc(nflp), alias(nflp)
    END DO
    WRITE (UNIT=nfprt, FMT='(3(A,I5),A,I3)') ' Directory Table Overflow.  nfld = ', &
           nfld, ' nflp = ', nflp, ' ndv = ',ndv, ' kk = ', kk
    STOP 3100
40  CONTINUE
    nflp=nflp-1
    mko(1:nflp)=0
    ife(1:nflp)=0
    mkdv(1:ndv)=0
    lif(1:ndv)=0

    ! determine direct availability of requested fields

    DO mm=1,nfld
       ! see if field is to be defered
       DO ll=1,ndv
          IF (chrdsc(mm) == chrdv(ll)) THEN
             ! mark input field as to be defered (derived from itself?)
             ! mark derived field as pending derivation from input field
             mkdir(mm)=100
             mkdv(ll)=2
             EXIT
          END IF
       END DO
       IF (ll <= ndv) CYCLE 

       DO nn=1,nflp
          IF (chrdsc(mm) == chrdo(nn)) THEN
             ! mark input field as needed directly
             ! mark requested field as being available directly from inputfield
             ! save input field number
             mkdir(mm)=1
             mko(nn)=1
             ife(nn)=mm
             EXIT
          END IF
       END DO
    END DO

    ! examine derived field table
    DO nn=1,nflp
       ! ignore directly available fields
       IF (mko(nn) == 1) CYCLE
       DO ll=1,ndv
          ! ignore undesired derivable fields
          IF (chrdv(ll) /= chrdo(nn)) CYCLE
          kk=kdv(ll)
          DO jj=1,kk
             DO mm=1,nfld
                ! verify availability of required fields for derivation
                ! matches required? if not, skip this input
                IF (chrdsc(mm) /= chreq(ll,jj)) CYCLE
                ! matches target? if so, dont save it's number
                IF (chrdsc(mm) /= chrdv(ll)) EXIT
                ! save input file field number
                IF (mkdv(ll) == 2) ife(nn)=mm
                EXIT
             END DO
             ! all inputs were inspected and no match occurred
             ! some required field is missing from the input
             IF (mm > nfld) EXIT
          END DO
          ! all inputs were inspected and no match occurred
          ! some required field is missing from the input
          IF (jj <= kk) EXIT
          ! ife(nn) contains last input required for this derivation
          IF (mkdv(ll) == 2) THEN
             ! mark requested field as defered if listed in input file directory
             ! save derivation rule number on "high" part of ife
             mko(nn)=3
             ife(nn)=ife(nn)+1000*ll
          ELSE 
             ! mark requested field as available by derivation
             ! save derived field number corresponding to requested field
             mko(nn)=2
             ife(nn)=ll
          ENDIF
          ! mark derived field as computable from input fields
          mkdv(ll)=1
          EXIT
       END DO
    END DO

    ! review input file directory
    DO ll=1,ndv
       ! ignore underivable or undesired fields in derived field table
       IF (mkdv(ll) /= 1) CYCLE
       kk=kdv(ll)
       DO  mm=1,nfld
          ! this is the same test as the first
          ! successfully derivable fields have mkdir == 200
          IF (chrdv(ll) == chrdsc(mm)) mkdir(mm)=mkdir(mm)+100
          DO jj=1,kk
             ! match current input field with a required field
             IF (chreq(ll,jj) /= chrdsc(mm)) CYCLE
             ! mark input field as needed for computing derived fields
             ! if it is not the derived field itself
             IF (chrdv(ll) /= chrdsc(mm)) mkdir(mm)=mkdir(mm)+2
             mxdir=mm
             EXIT
          END DO
       END DO
       ! save last required field input file sequence number
       lif(ll)=mxdir
    END DO

    ! create output file directory
    nof=0
    ll=1
    DO mm=1,nfld
       IF (nFile <= 0) THEN
          IF (prodia(mm) == 'DIAG') mkdir(mm)=-1
       ENDIF
       IF (mkdir(mm) < 1) CYCLE
       ! if field is directly available
       IF (MOD(mkdir(mm),2) == 1) THEN
          nof=nof+1
          ! add directly requested field to output directory
          chrop(nof)=chrdsc(mm)
          nvo(nof)=Lmax
          IF (nlevs(mm) == 1) nvo(nof)=1
       ENDIF
       ! any derivation rules left?
       IF (ll > ndv) CYCLE
       DO
          IF (mkdv(ll) == 1) THEN
             ! mkdir >= 1 means is required for some computation
             IF (mkdir(mm)>1 .AND. lif(ll)<=mm) THEN
                ! add requested derived field to output directory after last
                ! required input field has been processed
                nof=nof+1
                chrop(nof)=chrdv(ll)
                IF (nvv(ll) == 1) nvo(nof)=1
                IF (nvv(ll) == 2) nvo(nof)=Lmax
             ELSE
                EXIT
             ENDIF
          END IF
          ll=ll+1
          IF (ll > ndv) EXIT
       END DO
    END DO
    DO ii=1,nof
       nfe(ii)=0
       jfe(ii)=0
       DO nn=1,nflp
          IF (chrop(ii) == chrdo(nn)) THEN
             ifex=MOD(ife(nn),1000)
             IF (nuc(nn) == -1) THEN
                IF (mko(nn) == 1) nuco(ii)=ifldcd(ifex)
                IF (mko(nn) == 2) nuco(ii)=nudv(ifex)
                IF (mko(nn) == 3) nuco(ii)=ifldcd(ifex)
                ! if (mko(nn) == 3) nuco(ii)=nudv(ifex)
             ELSE
                nuco(ii)=nuc(nn)
             END IF
             IF ( mko(nn) == 3) THEN
                nfe(ii)=ife(nn)/1000
             ELSE
                nfe(ii)=ifex
             END IF
             jfe(ii)=nn
             ife(nn)=ifex
             EXIT
          ENDIF
       END DO
    END DO
    ! print input file directory anotated by field requirements
    WRITE (UNIT=nfprt, FMT='(/,A,/,A,3I3,I5,A,3I3,I5,/,&
                         &A,T22,A,T51,A,T60,A,T67,A,T73,A,T78,A)') &
          ' I n p u t   F i l e   D i r e c t o r y', &
          '   idate = ', idate, ' indate = ', indate, &
          ' number', 'chrdsc', 'prodia', 'len', 'lev', 'code', 'mkdir'
    WRITE (UNIT=nfprt, FMT='(1X,I3,T9,A40,3X,A4,2X,I8,I4,2X,I5,3X,I3)') &
          (mm, chrdsc(mm), prodia(mm), nharm(mm), nlevs(mm), &
           ifldcd(mm), mkdir(mm), mm=1,nfld)
    ! print requested file directory anotated by field availability
    WRITE (UNIT=nfprt, FMT='(/,A,/,A,T24,A,T56,A,T64,A,T71,A)') &
          ' R e q u e s t e d   F i e l d   D i r e c t o r y', &
          ' number', 'chrdo', 'nuc', 'mko', 'ife'
    WRITE (UNIT=nfprt, FMT='(1X,T3,I3,T11,A40,3X,I5,4X,I3,4X,I5,1X,A4)') &
          (nn, chrdo(nn), nuc(nn), mko(nn), ife(nn), alias(nn),nn=1,nflp)
    ! print derived field table anotated by field availability
    WRITE (UNIT=nfprt, FMT='(/,A,/)') ' D e r i v e d   F i e l d   T a b l e'
    DO ll=1,ndv
       WRITE (UNIT=nfprt, FMT='(A,I3,A,A40,/,6(A,I3))') &
             ' number', ll, ' chrdv:  ', chrdv(ll), ' kdv', kdv(ll), &
             ' nvv', nvv(ll), ' mkdv', mkdv(ll), ' lif', lif(ll), &
             ' iclcd', iclcd(ll), ' nudv', nudv(ll)
       WRITE (UNIT=nfprt, FMT='(1X,T18,A40,2X,I3)') &
             (chreq(ll,kk), nureq(ll,kk), kk=1,kdv(ll))
    END DO
    DO ii=1,nof
       aliop(ii)=alias(jfe(ii))
    ENDDO
    ! print output file directory
    WRITE (UNIT=nfprt, FMT='(/,A,/,A,T22,A,T54,A,T60,A,T67,A,T73,A)') &
          ' O u t p u t   F i l e   D i r e c t o r y', &
          ' number', 'chrop', 'nvo', 'nuco', 'nfe', 'jfe'
    WRITE (UNIT=nfprt, FMT='(1X,I3,5X,A40,3X,I3,2X,I5,4X,I3,3X,I3,1X,A4)') &
          (ii, chrop(ii), nvo(ii), nuco(ii), nfe(ii), &
           jfe(ii), aliop(ii), ii=1,nof)

    REWIND (UNIT=nfrfd)

    RETURN
    4000 WRITE (UNIT=nferr, FMT='(A)') &
               ' Unexpected End of File in Input File Directory.'
    STOP 4100

  END SUBROUTINE recon


  SUBROUTINE rwrite (lev, kflo, bfr)

    USE Constants, ONLY : Binary
    USE PrblSize, ONLY : Imax, Jmax, ngaus
    USE FileAccess, ONLY : WriteField
    USE FileGrib, ONLY : GDSPDSSETION,WriteGrbField
    USE Conversion, ONLY : cnvout
    USE RegInterp, ONLY : Idim=>IdimOut, Jdim=>JdimOut, mgaus, RegInt, DoAreaInterpolation,&
                           DoAreaGausInterpolation

    IMPLICIT NONE

    !     write gaussian grid to output file

    INTEGER, INTENT(IN) :: lev
    INTEGER, INTENT(IN) :: kflo

    REAL (KIND=r8), INTENT(IN) :: bfr(Imax,Jmax,lev)

    INTEGER :: l,ier,j,i

    LOGICAL :: t1, t2, t3, t4

    REAL (KIND=r8) :: bbfr(Imax,Jmax), breg(Idim,Jdim)

    INTEGER :: kpds(200),kgds(200)
    
    WRITE (UNIT=nfprt, FMT='(2(A,I5))') ' Lev = ', lev, ' kflo = ', kflo

    t1 = mko(jfe(kflo)) == 2
    t2 = mko(jfe(kflo)) == 0
    t3 = nuco(kflo)     == nudv(nfe(kflo)) 
    t4 = nuco(kflo)     == ifldcd(ife(jfe(kflo))) 

    !    
    CALL GDSPDSSETION (kgds, kpds )

    IF (.NOT. (t1 .OR. t2 .OR. t4)) THEN
       DO l=1,lev
          ier=0
          CALL cnvout (ngaus, ifldcd(ife(jfe(kflo))), nuco(kflo), &
               bfr(1,1,l), bbfr, ier)
          IF (ier > 0) WRITE (UNIT=nfprt, FMT='(6(A,I4))') &
             ' Conversion Error at kflo = ', kflo, ' Error = ', ier, &
             ' jfe = ', jfe(kflo), ' ife = ', ife(jfe(kflo)), &
             ' ifldcd = ', ifldcd(ife(jfe(kflo))), ' nuco = ',nuco(kflo)
          IF (RegInt) THEN
             IF (Binary) THEN
                !
                ! binary format
                !
                CALL DoAreaInterpolation (bbfr, breg)
                CALL WriteField (mgaus, breg)
             ELSE
                !
                ! grib format
                !
                CALL DoAreaInterpolation (bbfr, breg)
                CALL WriteGrbField (aliop(kflo),mgaus,kgds,kpds,breg,l)
	     END IF
	  ELSE
             IF (Binary) THEN
                !
                ! binary format
                !
                CALL DoAreaGausInterpolation(bbfr, breg)
		CALL WriteField (mgaus, breg)
             ELSE             
                !
                ! grib format
                !
		CALL DoAreaGausInterpolation(bbfr, breg)
                CALL WriteGrbField (aliop(kflo),mgaus,kgds,kpds,breg,l)
             ENDIF
          END IF
       END DO
    ELSE IF (t1 .AND. (.NOT. t3)) THEN
       DO l=1,lev
          ier=0
          CALL cnvout (ngaus, nudv(nfe(kflo)), nuco(kflo), &
               bfr(1,1,l), bbfr, ier)
          IF (ier > 0) WRITE (UNIT=nfprt, FMT='(5(A,I4))') &
             ' Conversion Error at kflo = ', kflo, ' Error = ', ier, &
             ' nfe = ', nfe(kflo), ' nudv = ', nudv(nfe(kflo)), ' nuco = ',nuco(kflo)
          IF (RegInt) THEN
            IF (Binary) THEN
                !
                ! binary format
                !
                CALL DoAreaInterpolation (bbfr, breg)
                CALL WriteField (mgaus, breg)
	    ELSE
                !
                ! grib format
                !
                CALL DoAreaInterpolation (bbfr, breg)
                CALL WriteGrbField (aliop(kflo),mgaus,kgds,kpds, breg,l)
	    END IF 
          ELSE
             IF (Binary) THEN
                !
                ! binary format
                !
                CALL DoAreaGausInterpolation(bbfr, breg)		
                CALL WriteField (mgaus, breg)
                !
             ELSE
                !
                ! grib format
                !
                CALL DoAreaGausInterpolation(bbfr, breg)		
                CALL WriteGrbField (aliop(kflo),mgaus,kgds,kpds, breg,l)
                !
             ENDIF
          END IF
       END DO
    ELSE IF ((t1 .AND. t3) .OR. t2 .OR. &
         ((.NOT. t1) .AND. (.NOT. t2) .AND. t4)) THEN
       DO l=1,lev
          DO j=1,Jmax
             DO i=1,Imax
                bbfr(i,j)=bfr(i,j,l)
             END DO
          END DO
          IF (RegInt) THEN
             IF (Binary) THEN
                !
                ! binary format
                !
                CALL DoAreaInterpolation (bbfr, breg)
                CALL WriteField (mgaus, breg)
             ELSE
                !
                ! grib format
                !
                CALL DoAreaInterpolation (bbfr, breg)
                CALL WriteGrbField (aliop(kflo),mgaus,kgds,kpds,breg,l)	     
	     END IF
	  ELSE
             IF (Binary) THEN
                !
                ! binary format
                !
                CALL DoAreaGausInterpolation(bbfr, breg)		
                CALL WriteField (mgaus, breg)
                !
             ELSE
                !
                ! grib format
                !
                CALL DoAreaGausInterpolation(bbfr, breg)		
                CALL WriteGrbField (aliop(kflo),mgaus,kgds,kpds,breg,l)
                !
             ENDIF
          END IF
       END DO
    END IF

  END SUBROUTINE rwrite
  
  SUBROUTINE GeraGribCtl(unt,fname,title,labelp,nof,ndp,nvo,aliop)
    USE tables, ONLY: table1,table2,table3,size_tb
    USE Constants, ONLY : res
    USE GaussRep, ONLY : glat
    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, Mnwv2, ngaus, pmand, alnpmd
    USE RegInterp, ONLY : Idim=>IdimOut, Jdim=>JdimOut, mgaus, &
         RegInt, DoAreaInterpolation,DoAreaGausInterpolation,gLats

   IMPLICIT NONE
   INTEGER    ,INTENT(IN   ) :: unt
   CHARACTER(LEN=256),INTENT(INOUT) :: fname
   CHARACTER(LEN=40) ,INTENT(IN   ) :: title
   CHARACTER(LEN=10) ,INTENT(IN   ) :: labelp
   INTEGER  ,INTENT(IN   ) :: nof
   INTEGER  ,INTENT(IN   ) :: ndp
   INTEGER  ,INTENT(IN   ) :: nvo  (ndp)
   CHARACTER(LEN=4),INTENT(IN   ) :: aliop(ndp)

   !
   INTEGER :: ifna
   INTEGER :: ifnb
   INTEGER :: it,iy, im, id, ih,ii,j,k,inv,itypelev,iiplev,i
   CHARACTER (LEN=20), PARAMETER :: type='PRESSURE HISTORY    '
   CHARACTER (LEN=3), PARAMETER :: cmth(12)=(/&
         'JAN','FEB','MAR','APR','MAY','JUN', &
         'JUL','AUG','SEP','OCT','NOV','DEC'/)

   !
   !  write output directory
   !
   INQUIRE (UNIT=unt, NAME=fname)
   ifnb=INDEX(fname//' ',' ')-5
   ifna=ifnb+1
   DO
      ifna=ifna-1
      IF (fname(ifna:ifna) == '/') EXIT
   END DO
   ifna=ifna+1
   PRINT*,' OUTPUT FILE: '//fname,ifna,ifna
   WRITE (UNIT=*, FMT='(A)') ' OUTPUT FILE: '//fname(ifna:ifnb)//'.grb'  
   WRITE (UNIT=unt, FMT='(A)') 'dset ^'//fname(ifna:ifnb)//'.grb'    
   WRITE (UNIT=unt, FMT='(A)') '*'
   WRITE (UNIT=unt, FMT='(A)') 'index ^'//fname(ifna:ifnb)//'.idx'    
   WRITE (UNIT=unt, FMT='(A)') '*'
   IF (RegInt) THEN
      WRITE (UNIT=unt, FMT='(A)') 'undef -2.56E+33'
   ELSE
      WRITE (UNIT=unt, FMT='(A)') 'undef 9.999E+20'
   END IF
   WRITE (UNIT=unt, FMT='(A)') '*'
   WRITE (UNIT=unt, FMT='(3A)') 'title ',type,title
   WRITE (UNIT=unt, FMT='(A)') '*'
   WRITE (UNIT=unt, FMT='(A,I6)') 'dtype grib',table1(size_tb(1))%id 
   WRITE (UNIT=unt, FMT='(A)') '*'
   WRITE (UNIT=unt, FMT='(A)') 'options yrev'
   WRITE (UNIT=unt, FMT='(A)') '*'
   IF (RegInt) THEN
      WRITE (UNIT=unt, FMT='(A,I5,A,F8.3,F15.10)') &
            'xdef ', Idim, ' linear ', 0.0_r8, 360.0_r8/REAL(Idim,r8)
      WRITE (UNIT=unt, FMT='(A,I5,A,F8.3,F15.10)') &
           'ydef ', Jdim, ' linear ',  -90.0_r8, 180.0_r8/REAL(Jdim-1,r8)
   ELSE
      WRITE (UNIT=unt, FMT='(A,I5,A,F8.3,F15.10)') &
            'xdef ', Idim, ' linear ', 0.0_r8, 360.0_r8/REAL(Idim,r8)
      WRITE (UNIT=unt, FMT='(A,I5,A)') 'ydef ', Jdim, ' levels '
      IF(res<=0)THEN
        WRITE (UNIT=12, FMT='(8F10.5)') (gLat(j),j=jMax,1,-1)
      ELSE
        WRITE (UNIT=12, FMT='(8F10.5)') (gLats(j),j=Jdim,1,-1)
      END IF
   END IF
   it=1
   READ (labelp, FMT='(I4,3I2)') iy, im, id, ih
   WRITE (UNIT=unt, FMT='(A,I5,A,I2.2,A,I2.2,A,I4,A)') &
        'tdef ', it, ' linear ', ih, 'Z', id, cmth(im), iy, ' 6hr'
   WRITE (UNIT=unt, FMT='(A)') '*'
   IF (Lmax <= 10) THEN
      WRITE (UNIT=unt, FMT='(A,I5,A,10I5)') 'zdef ', Lmax,'  levels ', &
           (NINT(pmand(k)),k=1,Lmax)
   ELSE
      WRITE (UNIT=unt, FMT='(A,I5,A,10I5)') 'zdef ', Lmax, ' levels ', &
           (NINT(pmand(k)),k=1,10)
      WRITE (UNIT=unt, FMT='((16X,10I5))') (NINT(pmand(k)),k=11,Lmax)
   END IF
   WRITE (UNIT=unt, FMT='(A,I5)') 'vars ', nof+2
   WRITE (UNIT=unt, FMT='(A)') 'topo  0 132,1,0 '// &
        '** surface TOPOGRAPHY [m]'
   WRITE (UNIT=unt, FMT='(A)') 'lsmk  0  81,1,0 '// &
        '** surface LAND SEA MASK [0,1]'
   DO ii=1,nof
       inv=nvo(ii)
       IF (inv == 1) inv=0
       DO i=1,size_tb(1)
           IF(aliop(ii) == table1(i)%name) THEN
                itypelev=100
                iiplev  = 0
               DO k=1,size_tb(2)
                  IF(TRIM(table1(i)%level) == TRIM(table2(k)%level_type))THEN 
                     itypelev=table2(k)%default 
                     iiplev  =table2(k)%id
                  END IF 
               END DO   
              WRITE (UNIT=unt, FMT='(A,I5,I5,A1,I5,A1,I5,1X,A)')table1(i)%name, inv,table1(i)%id,',',&
                     itypelev,',',iiplev,' ** '//table1(i)%level//table1(i)%title//'('//table1(i)%unit//')'
           END IF
       END DO
    ENDDO
    WRITE (UNIT=unt, FMT='(A)') 'endvars'  
  END SUBROUTINE GeraGribCtl
  
  
  SUBROUTINE GeraBinCtl(unt,fname,title,labelp,nof,ndp,nvo,aliop,chrop,nuco)
    USE GaussRep, ONLY : glat
    USE Constants, ONLY : Undef,res
    USE PrblSize, ONLY : Imax, Jmax, Kmax, Lmax, Mnwv2, ngaus, pmand, alnpmd
    USE RegInterp, ONLY : Idim=>IdimOut, Jdim=>JdimOut, mgaus, &
         RegInt, DoAreaInterpolation,DoAreaGausInterpolation,gLats
    USE Conversion, ONLY : GiveUnit

   IMPLICIT NONE
   INTEGER    ,INTENT(IN   ) :: unt
   CHARACTER(LEN=256),INTENT(INOUT) :: fname
   CHARACTER(LEN=40) ,INTENT(IN   ) :: title
   CHARACTER(LEN=10) ,INTENT(IN   ) :: labelp
   INTEGER  ,INTENT(IN   ) :: nof
   INTEGER  ,INTENT(IN   ) :: ndp
   INTEGER  ,INTENT(IN   ) :: nvo  (ndp)
   CHARACTER(LEN=4),INTENT(IN   ) :: aliop(ndp)
   CHARACTER (LEN=40),INTENT(IN   ):: chrop(ndp)
   INTEGER,INTENT(IN   ) :: nuco(ndp)

   !
   INTEGER :: ifna
   INTEGER :: ifnb
   INTEGER :: it,iy, im, id, ih,ii,j,k,inv
   CHARACTER (LEN=20), PARAMETER :: type='PRESSURE HISTORY    '
   CHARACTER (LEN=3), PARAMETER :: cmth(12)=(/&
         'JAN','FEB','MAR','APR','MAY','JUN', &
         'JUL','AUG','SEP','OCT','NOV','DEC'/)

   !
   !  write output directory
   !
       INQUIRE (UNIT=11, NAME=fname)
       ifnb=INDEX(fname//' ',' ')-1
       ifna=ifnb+1
       DO
          ifna=ifna-1
          IF (fname(ifna:ifna) == '/') EXIT
       END DO
       ifna=ifna+1
       WRITE (UNIT=*, FMT='(/,A,/)') ' OUTPUT FILE: '//fname(ifna:ifnb)
       WRITE (UNIT=12, FMT='(A)') 'DSET ^'//fname(ifna:ifnb)
       WRITE (UNIT=12, FMT='(A)') '*'
       WRITE (UNIT=12, FMT='(A)') 'OPTIONS SEQUENTIAL YREV BIG_ENDIAN'
       WRITE (UNIT=12, FMT='(A)') '*'
       WRITE (UNIT=12, FMT='(A,1PE9.2)') 'UNDEF ', Undef
       WRITE (UNIT=12, FMT='(A)') '*'
       WRITE (UNIT=12, FMT='(3A)') 'TITLE ',type,title
       WRITE (UNIT=12, FMT='(A)') '*'
       IF (RegInt) THEN
          WRITE (UNIT=12, FMT='(A,I5,A,F8.3,F15.10)') &
            'XDEF ', Idim, ' LINEAR ', 0.0_r8, 360.0_r8/REAL(Idim,r8)
          WRITE (UNIT=12, FMT='(A,I5,A,F8.3,F15.10)') &
               'YDEF ', Jdim, ' LINEAR ', -90.0_r8, 180.0_r8/REAL(Jdim-1,r8)
       ELSE
          WRITE (UNIT=12, FMT='(A,I5,A,F8.3,F15.10)') &
            'XDEF ', Idim, ' LINEAR ', 0.0_r8, 360.0_r8/REAL(Idim,r8)
          WRITE (UNIT=12, FMT='(A,I5,A)') 'YDEF ', Jdim, ' LEVELS '
          IF(res<=0)THEN
	    WRITE (UNIT=12, FMT='(8F10.5)') (gLat(j),j=jMax,1,-1)
	  ELSE
	    WRITE (UNIT=12, FMT='(8F10.5)') (gLats(j),j=Jdim,1,-1)
	  END IF
       END IF
       IF (Lmax <= 10) THEN
          WRITE (UNIT=12, FMT='(A,I5,A,10I5)') 'ZDEF ', Lmax,'  LEVELS ', &
               (NINT(pmand(k)),k=1,Lmax)
       ELSE
          WRITE (UNIT=12, FMT='(A,I5,A,10I5)') 'ZDEF ', Lmax, ' LEVELS ', &
               (NINT(pmand(k)),k=1,10)
          WRITE (UNIT=12, FMT='((16X,10I5))') (NINT(pmand(k)),k=11,Lmax)
       END IF
       it=1
       READ (labelp, FMT='(I4,3I2)') iy, im, id, ih
       WRITE (UNIT=12, FMT='(A,I5,A,I2.2,A,I2.2,A,I4,A)') &
            'TDEF ', it, ' LINEAR ', ih, 'Z', id, cmth(im), iy, ' 6HR'
       WRITE (UNIT=12, FMT='(A)') '*'
       WRITE (UNIT=12, FMT='(A,I5)') 'VARS ', nof+2
       WRITE (UNIT=12, FMT='(A)') 'TOPO    0 99 '// &
            'TOPOGRAPHY                              (M               )'
       WRITE (UNIT=12, FMT='(A)') 'LSMK    0 99 '// &
            'LAND SEA MASK                           (NO DIM          )'
       DO ii=1,nof
          inv=nvo(ii)
          IF (inv == 1) inv=0
          WRITE (UNIT=12, FMT='(A,I5,I3,1X,A)') aliop(ii), inv, 99, &
               chrop(ii)//'('//GiveUnit(nuco(ii))//')'
       ENDDO
       WRITE (UNIT=12, FMT='(A)') 'ENDVARS'
  END SUBROUTINE GeraBinCtl
END MODULE PostLoop
