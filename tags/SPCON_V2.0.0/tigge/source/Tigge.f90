!-------------------------------------------------------------------------
!
!  le saidas grib do modelo global e escolhe as variaveis de superficie e
!  outros niveis padroes para o projeto TIGGE (ensemble) em formato grib1
!
!  Autor: Julio Pablo Reyes Fernandez
!
!-------------------------------------------------------------------------

PROGRAM Tigge

  USE Constants 

  IMPLICIT NONE

  INTEGER ivar,nfi
  CHARACTER(LEN=256) :: prefx, fname2(200)

  !
  ! simple variables
  !
  CALL simple(fname2)

  !
  ! accumulated variables
  ! 
  WRITE (*,*) fname2
!AAF  DO ivar = 15,18
  DO ivar = 13,19

     CALL accum ( ivar, fname2 )

  ENDDO

  CLOSE(5)

  STOP 9999

CONTAINS 

  SUBROUTINE simple (fname2)

    USE Constants

    IMPLICIT NONE

    INTEGER i,iunit,ierr,ivar,klev,k,iret
    CHARACTER*256 varname, fname2(200)
    i=0
    iret=0
    iunit=10
    aok=.TRUE.
    !
    !    read input file name
    !
    nfi=0
    READ(5,80,END=999) fname
    prefx=fname(5:7)

    PRINT*,TRIM(prefx)
80  FORMAT(a91)
    DO WHILE(fname.NE.'done')

       nfi=nfi+1
       fname2(nfi)=fname
       PRINT*,'FNAME: ',fname2(nfi),nfi

       i=i+1
       iunit=i+1
       !
       lengh=INDEX(fname,' ')-1
       WRITE(6,*) 'trying to open ', fname(1:lengh)

       CALL baopen(iunit,fname(1:lengh),ierr)
       IF (ierr .NE. 0) WRITE(6,*) 'baopen error!'

       !
       ! Single level data
       !

       PRINT*,' Single level data'

       DO ivar = 1, 12

          varname=tiggename(ivar)

          !     JPDS         INTEGER (200) PDS PARAMETERS FOR WHICH TO SEARCH
          !                  (=-1 FOR WILDCARD)
          !          (1)   - ID OF CENTER
          !          (2)   - GENERATING PROCESS ID NUMBER
          !          (3)   - GRID DEFINITION
          !          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
          !          (5)   - INDICATOR OF PARAMETER
          !          (6)   - TYPE OF LEVEL
          !          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
          !          (8)   - YEAR INCLUDING (CENTURY-1)
          !          (9)   - MONTH OF YEAR
          !          (10)  - DAY OF MONTH
          !          (11)  - HOUR OF DAY
          !          (12)  - MINUTE OF HOUR
          !          (13)  - INDICATOR OF FORECAST TIME UNIT
          !          (14)  - TIME RANGE 1
          !          (15)  - TIME RANGE 2
          !          (16)  - TIME RANGE FLAG
          !          (17)  - NUMBER INCLUDED IN AVERAGE
          !          (18)  - VERSION NR OF GRIB SPECIFICATION
          !          (19)  - VERSION NR OF PARAMETER TABLE
          !          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
          !          (21)  - CENTURY OF REFERENCE TIME OF DATA
          !          (22)  - UNITS DECIMAL SCALE FACTOR
          !          (23)  - SUBCENTER NUMBER
          !          (24)  - PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
          !                  128 IF FORECAST FIELD ERROR
          !                   64 IF BIAS CORRECTED FCST FIELD
          !                   32 IF SMOOTHED FIELD
          !                  WARNING: CAN BE COMBINATION OF MORE THAN 1
          !          (25)  - PDS BYTE 30, NOT USED

          jpds(:)=-1
          jpds(5)=jpdssuns(5,ivar)
          jpds(6)=jpdssuns(6,ivar)
          jpds(7)=jpdssuns(7,ivar)
          jgds(:)=-1
          jens(:)=-1

          CALL postigge ( iunit,fname,varname ,ivar)

       ENDDO

       !
       ! Upper air data
       !
       PRINT*,' Upper air data'

       DO ivar = 20, 24

          varname=tiggename(ivar)

          IF(ivar == 21 ) THEN
             !
             ! addition geopotential height at 50 hPa
             !
             klev = 9
          ELSE
             klev = 8
          ENDIF

          DO k = 1, klev

             jpds(:)=-1
             jpds(5)=jpdssuns(5,ivar)
             jpds(6)=jpdssuns(6,ivar)
             jpds(7)=ilev(k)
             write(*,*)jpds(7),ilev(k)
             jgds(:)=-1
             jens(:)=-1

             CALL postigge ( iunit,fname,varname,ivar )

          ENDDO

       ENDDO

       !
       ! CLOSE GRIB UNIT
       !        
       CALL baclose(iunit,ierr)
       !
       IF(iunit.GT.90) iunit=10

       READ(5,80,END=999) fname
       IF (iret.NE.0) THEN
          fname='done'
          aok=.FALSE.
       ENDIF
    ENDDO
    !
    !       end while
    ! 
999 CONTINUE

  END SUBROUTINE simple
  !
  !--------------------------------------------------------------------------
  !   
  SUBROUTINE accum (ivar, fname2)

    USE Constants

    IMPLICIT NONE
    INTEGER kpdso(200),kgdso(200),kenso(200)
    INTEGER i,iunit,ierr,ivar,iret,kf,kr,iacc,n,ibs,nbits,ifi, hh_str
    REAL SUM(ji), fact, rtemp
    CHARACTER*256 varname, fname2(200), char_kpdso15
    ! character*256 fname,name,tiggename

    i=0
    ibs = 0
    nbits = 16
    iunit=10
    iacc=0
    sum=0.
    lisum=.TRUE.
    aok=.TRUE.

    !
    !    read input file name
    !
    !REWIND 5
    !READ(5,80)fname
    !READ(5,80,END=999) fname
    80  FORMAT(a91)
    ifi=2
    fname=fname2(ifi)
    prefx=fname(5:7)
    
    hh_str=6
    DO WHILE(fname.NE.'done')

       i=i+1
       iunit=i+1
       !
       lengh=INDEX(fname,' ')-1
       write (*,'(A11,A48)') "acc FNAME=",fname
       IF((fname(1:2)) == '')RETURN
       CALL baopen(iunit,fname(1:lengh),ierr)
       IF (ierr .NE. 0) THEN
           exit
	        WRITE(6,*) 'baopen error!'   
   	 END IF

       varname=tiggename(ivar)

       jpds(:)=-1
       jpds(5)=jpdssuns(5,ivar)
       jpds(6)=jpdssuns(6,ivar)
       jpds(7)=jpdssuns(7,ivar)
       jgds(:)=-1
       jens(:)=-1

       !
       ! accum variables
       !

       CALL GETGBE(iunit,0,JI,0,JPDS,JGDS,JENS,KF,KR,        &
            KPDS,KGDS,KENS,LI,dummy,IRET)
       PRINT*,'KF=',KF,' KR=' ,KR, 'IRET=',iret
       lengh=INDEX(varname,' ')-1
       IF (iret .NE. 0) THEN
          WRITE(6,*) varname(1:lengh)//' TROUBLE AHEAD!!!'
          WRITE(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
          RETURN
       ELSE
          IF (i.EQ.1) THEN
             !
             !   assume files are in cronological order, first file will
             !   have needed date info
             !
             kpdso=kpds
             kgdso=kgds
             kenso=kens
!             PRINT*,'kens =',kens(1),kens(2),kens(3),kens(4),kens(5)
!             PRINT*,'jpds =',jpds(5),jpds(6),jpds(7)
!             PRINT*,'kpds =',kpds(5),kpds(6),kpds(7),kpds(16)
             kpdso(16)=4		! fix bug accum radiation variables
          ENDIF
 
          IF (ivar.lt.13.and.ivar.gt.19) THEN
             kpdso(14)=hh_str/6-1
             kpdso(15)=hh_str/6
          ELSE
             kpdso(14)=0
             kpdso(15)=hh_str/6
!             kpds(15)=kpds(15)*6
!             kpds(14)=kpds(14)*6
          END IF

          kpdso(13)=11
          hh_str=hh_str+6
          
          !          (14)  - TIME RANGE 1
          !          (15)  - TIME RANGE 2
          !          (16)  - TIME RANGE FLAG
!          iacc=iacc+kpds(15)-kpds(14)
!          PRINT*,'iacc, kpds13, kpds15, kpds14 =',iacc,kpds(13),kpds(15),kpds(14)
          !      print*,'kpds =',kpds

          IF(ivar.GE.15) THEN
             fact=3600.*6.
          ELSE
             fact=0.25
          ENDIF

          DO N=1,kf
             SUM(N)=SUM(N) + dummy(N)*fact     ! warning only for 6 hours
             lisum(N)=lisum(N).AND.li(N)
          ENDDO
          !        endif
          !      
          !   save acc data
          !
!          PRINT*,'paulo',kpds(16),kpds(15),kpds(14),iacc
!	       PRINT*,'paulo',fname

!          KPDSo(15)=kpdso(14)+iacc
!          KPDSo(15)=kpdso(14)+6
          
          KPDSo(22)=2

!          PRINT*,'Max('//varname(1:lengh)//')=',MAXVAL(sum), &
!                 ' Min('//varname(1:lengh)//')=',MINVAL(sum)

          if (kpdso(15).lt.100) then
            if (kpdso(15).lt.10) then
               write (char_kpdso15,'(I1)') kpdso(15)
            else
               write (char_kpdso15,'(I2)') kpdso(15)
            end if
          else 
            write (char_kpdso15,'(I3)') kpdso(15)   
          end if

          name='tigge_'//varname(1:lengh)//'_sfc_'//TRIM(fname(18:27))//'_'//TRIM(char_kpdso15)//'.grb'  
          write (*,*) "Writing: ",name 
          
          lengh1=INDEX(name,' ')-1
          !
          ! Save data in individual files
          !
          WRITE (*,*)"KPDSO(15)=",kpdso(15),name(1:lengh1)//TRIM(char_kpdso15)
          CALL BAOPEN(91,name(1:lengh1),ierr)
          IF (ierr .NE. 0) WRITE(6,*) 'output baopen error!'

          !     kpdso    kpdsoo     INTEGER (200) PDS PARAMETERS
          kpdso(1)=jpdssuns2(1,ivar)!          (1)   - ID OF CENTER
          kpdso(2)= 255!          (2)   - GENERATING PROCESS ID NUMBER
          !          (3)   - GRID DEFINITION
          !          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
          kpdso(5)=jpdssuns2(5,ivar)!	    (5)   - INDICATOR OF PARAMETER
          kpdso(6)=jpdssuns2(6,ivar)!	    (6)   - TYPE OF LEVEL
          kpdso(7)=jpdssuns2(7,ivar)!	    (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
          !          (8)   - YEAR INCLUDING (CENTURY-1)
          !          (9)   - MONTH OF YEAR
          !          (10)  - DAY OF MONTH
          !          (11)  - HOUR OF DAY
          !          (12)  - MINUTE OF HOUR
          !          (13)  - INDICATOR OF FORECAST TIME UNIT
          !          (14)  - TIME RANGE 1
          !          (15)  - TIME RANGE 2
          !          (16)  - TIME RANGE FLAG
          !          (17)  - NUMBER INCLUDED IN AVERAGE
          kpdso(18)=jpdssuns2(18,ivar) !          (18)  - VERSION NR OF GRIB SPECIFICATION
          kpdso(19)=jpdssuns2(19,ivar) !          (19)  - VERSION NR OF PARAMETER TABLE
          !          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
          !          (21)  - CENTURY OF REFERENCE TIME OF DATA
          !          (22)  - UNITS DECIMAL SCALE FACTOR
          !          (23)  - SUBCENTER NUMBER
          !          (24)  - PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
          !                  128 IF FORECAST FIELD ERROR
          !                   64 IF BIAS CORRECTED FCST FIELD
          !                   32 IF SMOOTHED FIELD
          !                  WARNING: CAN BE COMBINATION OF MORE THAN 1
          !          (25)  - PDS BYTE 30, NOT USED

          !     KGDS         INTEGER (200) GDS PARAMETERS
          !          (1)   - DATA REPRESENTATION TYPE
          !          (19)  - NUMBER OF VERTICAL COORDINATE PARAMETERS
          !          (20)  - OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE
          !                  PARAMETERS
          !                  OR
          !                  OCTET NUMBER OF THE LIST OF NUMBERS OF POINTS
          !                  IN EACH ROW
          !                  OR
          !                  255 IF NEITHER ARE PRESENT
          !          (21)  - FOR GRIDS WITH PL, NUMBER OF POINTS IN GRID
          !          (22)  - NUMBER OF WORDS IN EACH ROW
          !       LATITUDE/LONGITUDE GRIDS
          !          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
          !          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
          !          (4)   - LA(1) LATITUDE OF ORIGIN
          !          (5)   - LO(1) LONGITUDE OF ORIGIN
          !          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
          !          (7)   - LA(2) LATITUDE OF EXTREME POINT
          !          (8)   - LO(2) LONGITUDE OF EXTREME POINT
          !          (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
          !          (10)  - DJ LATITUDINAL DIRECTION INCREMENT
          !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
          !       GAUSSIAN  GRIDS
          !          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
          !          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
          !          (4)   - LA(1) LATITUDE OF ORIGIN
          !          (5)   - LO(1) LONGITUDE OF ORIGIN
          !          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
          !          (7)   - LA(2) LATITUDE OF EXTREME POINT
          !          (8)   - LO(2) LONGITUDE OF EXTREME POINT
          !          (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
          !          (10)  - N - NR OF CIRCLES POLE TO EQUATOR
          !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
          !          (12)  - NV - NR OF VERT COORD PARAMETERS
          !          (13)  - PV - OCTET NR OF LIST OF VERT COORD PARAMETERS
          !                             OR
          !                  PL - LOCATION OF THE LIST OF NUMBERS OF POINTS IN
          !                       EACH ROW (IF NO VERT COORD PARAMETERS
          !                       ARE PRESENT
          !                             OR
          !                  255 IF NEITHER ARE PRESENT
          !       POLAR STEREOGRAPHIC GRIDS
          !          (2)   - N(I) NR POINTS ALONG LAT CIRCLE
          !          (3)   - N(J) NR POINTS ALONG LON CIRCLE
          !          (4)   - LA(1) LATITUDE OF ORIGIN
          !          (5)   - LO(1) LONGITUDE OF ORIGIN
          !          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
          !          (7)   - LOV GRID ORIENTATION
          !          (8)   - DX - X DIRECTION INCREMENT
          !          (9)   - DY - Y DIRECTION INCREMENT
          !          (10)  - PROJECTION CENTER FLAG
          !          (11)  - SCANNING MODE (RIGHT ADJ COPY OF OCTET 28)
          !       SPHERICAL HARMONIC COEFFICIENTS
          !          (2)   - J PENTAGONAL RESOLUTION PARAMETER
          !          (3)   - K      "          "         "
          !          (4)   - M      "          "         "
          !          (5)   - REPRESENTATION TYPE
          !          (6)   - COEFFICIENT STORAGE MODE
          !       MERCATOR GRIDS
          !          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
          !          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
          !          (4)   - LA(1) LATITUDE OF ORIGIN
          !          (5)   - LO(1) LONGITUDE OF ORIGIN
          !          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
          !          (7)   - LA(2) LATITUDE OF LAST GRID POINT
          !          (8)   - LO(2) LONGITUDE OF LAST GRID POINT
          !          (9)   - LATIT - LATITUDE OF PROJECTION INTERSECTION
          !          (10)  - RESERVED
          !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
          !          (12)  - LONGITUDINAL DIR GRID LENGTH
          !          (13)  - LATITUDINAL DIR GRID LENGTH
          !       LAMBERT CONFORMAL GRIDS
          !          (2)   - NX NR POINTS ALONG X-AXIS
          !          (3)   - NY NR POINTS ALONG Y-AXIS
          !          (4)   - LA1 LAT OF ORIGIN (LOWER LEFT)
          !          (5)   - LO1 LON OF ORIGIN (LOWER LEFT)
          !          (6)   - RESOLUTION (RIGHT ADJ COPY OF OCTET 17)
          !          (7)   - LOV - ORIENTATION OF GRID
          !          (8)   - DX - X-DIR INCREMENT
          !          (9)   - DY - Y-DIR INCREMENT
          !          (10)  - PROJECTION CENTER FLAG
          !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
          !          (12)  - LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
          !          (13)  - LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER

!          PRINT*,'pkubota=',kenso(1:5)
          !     kenso         INTEGER (200) ENSEMBLE PDS PARMS
          kenso(1)=1!          (1)   - APPLICATION IDENTIFIER! no change, 1=ensemble (identifies application)

          IF ( TRIM(prefx) =='AVN' ) THEN
             kenso(2)=1! (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
             kenso(3)=1! (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
          ELSE IF ( TRIM(prefx) =='NMC' ) THEN
             kenso(2)=1! (2)   - ENSEMBLE TYPE	  ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
             kenso(3)=1! (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
          ELSE
             !
             !  perturbed members
             !
             IF ( TRIM(prefx)=='01P' ) THEN
                kenso(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=1 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSE IF( TRIM(prefx)=='02P' ) THEN
                kenso(2)=3 !	      (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=2 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='03P' ) THEN
                kenso(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=3 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='04P' ) THEN
                kenso(2)=3 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=4 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='05P' ) THEN
                kenso(2)=3 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=5 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='06P' ) THEN
                kenso(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=6 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='07P' ) THEN
                kenso(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=7 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='01N' ) THEN
                kenso(2)=2 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=1 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='02N' ) THEN
                kenso(2)=2 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=2 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='03N' ) THEN
                kenso(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=3 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='04N' ) THEN
                kenso(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=4 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='05N' ) THEN
                kenso(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=5 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='06N' ) THEN
                kenso(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=6 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ELSEIF( TRIM(prefx)=='07N' ) THEN
                kenso(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
                kenso(3)=7 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kenso(2)=1 -> 1 high res, 2 low res if kenso(2)=2 or 3 -> will = 1, 2, 3, ...
             ENDIF
          ENDIF
          kenso(4)=1  !          (4)   - PRODUCT IDENTIFIER! no change, 1=full field
          kenso(5)=255!          (5)   - SMOOTHING FLAG ! no change, 255 original resolution


          CALL putgben(91,kf,kpdso,kgdso,kenso,ibs,nbits,lisum,sum,iret)
!          WRITE(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
          IF (iret .NE. 0) WRITE(6,*) 'putgben error!'
          CALL BACLOSE(91,ierr)


       ENDIF
       !
       ! CLOSE GRIB UNIT
       !        
       CALL baclose(iunit,ierr)
       !
       !
       IF(iunit.GT.90) iunit=10

       !READ(5,80,END=999) fname
	    ifi=ifi+1
       fname=fname2(ifi)

       IF (iret.NE.0) THEN
          fname='done'
          aok=.FALSE.
       ENDIF
    ENDDO
    !
    !       end while
    ! 
    !END DO
999 CONTINUE   

  END SUBROUTINE accum
  !   
  ! -------------------------------------------------------------------------
  !
  SUBROUTINE postigge (iunit, fname,tiggename,ivar )

    USE Constants, ONLY: jpds,jgds,jens, kpds, kgds, kens, ji, &
         lengh, lengh1, li, dummy

    IMPLICIT NONE
    INTEGER ibs,ii,iunit,nbits,kf,kr,iret,ierr,ivar
    CHARACTER(LEN=*) :: fname,tiggename
    CHARACTER(LEN=256) :: name
    CHARACTER*4 nivel

    ibs = 0
    nbits = 16  

    CALL GETGBE(iunit,0,JI,0,JPDS,JGDS,JENS,KF,KR,        &
         KPDS,KGDS,KENS,LI,dummy,IRET)

    KPDS(13)=1
    KPDS(14)=KPDS(14)*6
    WRITE (*,*) "===>",KPDS(13:16)

    lengh=INDEX(tiggename,' ')-1

    IF (iret .NE. 0) THEN
       WRITE(6,*) tiggename(1:lengh)//' TROUBLE AHEAD!!!'
       WRITE(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
       WRITE(6,*) jpds(5),jpds(6),jpds(7)
       RETURN
    ENDIF

    IF(jpds(5)==81) THEN	! mask

       DO ii=1,ji
          IF( dummy(ii)==1.  ) dummy(ii)=0.       ! ocean for TIGGE
          IF( dummy(ii)==-1. ) dummy(ii)=1.       ! land for TIGGE
       ENDDO

    ENDIF

    IF(jpds(5)==135 .OR. jpds(5)==2) THEN	! pressure

       DO ii=1,ji
          dummy(ii) = dummy(ii)*100. ! from hPa to Pascal for TIGGE
       ENDDO

    ENDIF

    IF(jpds(5)==71) THEN	! cloud cover

       DO ii=1,ji
          dummy(ii) = dummy(ii)*100. ! from 0-1 to % for TIGGE
       ENDDO

    ENDIF
    !
    ! create individual names
    !
       PRINT*,'FNAME: ',fname

    IF(jpds(6)/=100 .and. LEN_TRIM(fname) /= 0) THEN
       PRINT*,'Max('//tiggename(1:lengh)//')=',MAXVAL(dummy), &
            ' Min('//tiggename(1:lengh)//')=',MINVAL(dummy)

       name='tigge_'//TRIM(tiggename(1:lengh))//'_sfc_'//TRIM(fname(18:27))//'.grb'
    ELSE
       PRINT*,'Level=',jpds(7),'Max('//tiggename(1:lengh)//')=',MAXVAL(dummy), &
            ' Min('//tiggename(1:lengh)//')=',MINVAL(dummy)

       WRITE(nivel,50) jpds(7)
50     FORMAT(I4.4)
       name='tigge_'//tiggename(1:lengh)//'_'//nivel//'_'//fname(18:27)//'.grb'
    ENDIF
    lengh1=INDEX(name,' ')-1
    !
    ! Save data in individual files
    !
    CALL BAOPEN(91,name(1:lengh1),ierr)
    IF (ierr .NE. 0) WRITE(6,*) 'output baopen error!'

    !     KPDS         INTEGER (200) PDS PARAMETERS
    KPDS(1)=jpdssuns2(1,ivar)!          (1)   - ID OF CENTER
    KPDS(2)= 255!          (2)   - GENERATING PROCESS ID NUMBER
    !          (3)   - GRID DEFINITION
    !          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
    KPDS(5)=jpdssuns2(5,ivar)!	    (5)   - INDICATOR OF PARAMETER
    KPDS(6)=jpdssuns2(6,ivar)!	    (6)   - TYPE OF LEVEL
!    KPDS(7)=jpdssuns2(7,ivar)!	    (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
    !          (8)   - YEAR INCLUDING (CENTURY-1)
    !          (9)   - MONTH OF YEAR
    !          (10)  - DAY OF MONTH
    !          (11)  - HOUR OF DAY
    !          (12)  - MINUTE OF HOUR
    !          (13)  - INDICATOR OF FORECAST TIME UNIT
    !          (14)  - TIME RANGE 1
    KPDS(13)=11
    KPDS(14)=KPDS(14)/6
    !          (15)  - TIME RANGE 2
    !          (16)  - TIME RANGE FLAG
    !          (17)  - NUMBER INCLUDED IN AVERAGE
    KPDS(18)=jpdssuns2(18,ivar) !          (18)  - VERSION NR OF GRIB SPECIFICATION
    KPDS(19)=jpdssuns2(19,ivar) !          (19)  - VERSION NR OF PARAMETER TABLE
    !          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
    !          (21)  - CENTURY OF REFERENCE TIME OF DATA
    !          (22)  - UNITS DECIMAL SCALE FACTOR
    !          (23)  - SUBCENTER NUMBER
    !          (24)  - PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
    !                  128 IF FORECAST FIELD ERROR
    !                   64 IF BIAS CORRECTED FCST FIELD
    !                   32 IF SMOOTHED FIELD
    !                  WARNING: CAN BE COMBINATION OF MORE THAN 1
    !          (25)  - PDS BYTE 30, NOT USED

    !     KGDS         INTEGER (200) GDS PARAMETERS
    !          (1)   - DATA REPRESENTATION TYPE
    !          (19)  - NUMBER OF VERTICAL COORDINATE PARAMETERS
    !          (20)  - OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE
    !                  PARAMETERS
    !                  OR
    !                  OCTET NUMBER OF THE LIST OF NUMBERS OF POINTS
    !                  IN EACH ROW
    !                  OR
    !                  255 IF NEITHER ARE PRESENT
    !          (21)  - FOR GRIDS WITH PL, NUMBER OF POINTS IN GRID
    !          (22)  - NUMBER OF WORDS IN EACH ROW
    !       LATITUDE/LONGITUDE GRIDS
    !          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
    !          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
    !          (4)   - LA(1) LATITUDE OF ORIGIN
    !          (5)   - LO(1) LONGITUDE OF ORIGIN
    !          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
    !          (7)   - LA(2) LATITUDE OF EXTREME POINT
    !          (8)   - LO(2) LONGITUDE OF EXTREME POINT
    !          (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
    !          (10)  - DJ LATITUDINAL DIRECTION INCREMENT
    !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
    !       GAUSSIAN  GRIDS
    !          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
    !          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
    !          (4)   - LA(1) LATITUDE OF ORIGIN
    !          (5)   - LO(1) LONGITUDE OF ORIGIN
    !          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
    !          (7)   - LA(2) LATITUDE OF EXTREME POINT
    !          (8)   - LO(2) LONGITUDE OF EXTREME POINT
    !          (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
    !          (10)  - N - NR OF CIRCLES POLE TO EQUATOR
    !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
    !          (12)  - NV - NR OF VERT COORD PARAMETERS
    !          (13)  - PV - OCTET NR OF LIST OF VERT COORD PARAMETERS
    !                             OR
    !                  PL - LOCATION OF THE LIST OF NUMBERS OF POINTS IN
    !                       EACH ROW (IF NO VERT COORD PARAMETERS
    !                       ARE PRESENT
    !                             OR
    !                  255 IF NEITHER ARE PRESENT
    !       POLAR STEREOGRAPHIC GRIDS
    !          (2)   - N(I) NR POINTS ALONG LAT CIRCLE
    !          (3)   - N(J) NR POINTS ALONG LON CIRCLE
    !          (4)   - LA(1) LATITUDE OF ORIGIN
    !          (5)   - LO(1) LONGITUDE OF ORIGIN
    !          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
    !          (7)   - LOV GRID ORIENTATION
    !          (8)   - DX - X DIRECTION INCREMENT
    !          (9)   - DY - Y DIRECTION INCREMENT
    !          (10)  - PROJECTION CENTER FLAG
    !          (11)  - SCANNING MODE (RIGHT ADJ COPY OF OCTET 28)
    !       SPHERICAL HARMONIC COEFFICIENTS
    !          (2)   - J PENTAGONAL RESOLUTION PARAMETER
    !          (3)   - K      "          "         "
    !          (4)   - M      "          "         "
    !          (5)   - REPRESENTATION TYPE
    !          (6)   - COEFFICIENT STORAGE MODE
    !       MERCATOR GRIDS
    !          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
    !          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
    !          (4)   - LA(1) LATITUDE OF ORIGIN
    !          (5)   - LO(1) LONGITUDE OF ORIGIN
    !          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
    !          (7)   - LA(2) LATITUDE OF LAST GRID POINT
    !          (8)   - LO(2) LONGITUDE OF LAST GRID POINT
    !          (9)   - LATIT - LATITUDE OF PROJECTION INTERSECTION
    !          (10)  - RESERVED
    !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
    !          (12)  - LONGITUDINAL DIR GRID LENGTH
    !          (13)  - LATITUDINAL DIR GRID LENGTH
    !       LAMBERT CONFORMAL GRIDS
    !          (2)   - NX NR POINTS ALONG X-AXIS
    !          (3)   - NY NR POINTS ALONG Y-AXIS
    !          (4)   - LA1 LAT OF ORIGIN (LOWER LEFT)
    !          (5)   - LO1 LON OF ORIGIN (LOWER LEFT)
    !          (6)   - RESOLUTION (RIGHT ADJ COPY OF OCTET 17)
    !          (7)   - LOV - ORIENTATION OF GRID
    !          (8)   - DX - X-DIR INCREMENT
    !          (9)   - DY - Y-DIR INCREMENT
    !          (10)  - PROJECTION CENTER FLAG
    !          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
    !          (12)  - LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
    !          (13)  - LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER

!    PRINT*,'pkubota=',KENS(1:5)
    !     KENS         INTEGER (200) ENSEMBLE PDS PARMS
    KENS(1)=1!          (1)   - APPLICATION IDENTIFIER! no change, 1=ensemble (identifies application)

    IF ( TRIM(prefx) =='AVN' ) THEN
       kens(2)=1! (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
       kens(3)=1! (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
    ELSE IF ( TRIM(prefx) =='NMC' ) THEN
       kens(2)=1! (2)   - ENSEMBLE TYPE	  ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
       kens(3)=1! (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
    ELSE
       !
       !  perturbed members
       !
       IF ( TRIM(prefx)=='01P' ) THEN
          kens(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=1 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSE IF( TRIM(prefx)=='02P' ) THEN
          kens(2)=3 !	      (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=2 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='03P' ) THEN
          kens(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=3 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='04P' ) THEN
          kens(2)=3 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=4 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='05P' ) THEN
          kens(2)=3 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=5 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='06P' ) THEN
          kens(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=6 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='07P' ) THEN
          kens(2)=3 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=7 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='01N' ) THEN
          kens(2)=2 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=1 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='02N' ) THEN
          kens(2)=2 !	      (2)   - ENSEMBLE TYPE	    ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=2 !	      (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='03N' ) THEN
          kens(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=3 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='04N' ) THEN
          kens(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=4 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='05N' ) THEN
          kens(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=5 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='06N' ) THEN
          kens(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=6 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ELSEIF( TRIM(prefx)=='07N' ) THEN
          kens(2)=2 !	     (2)   - ENSEMBLE TYPE	   ! type, 1=unperturbed crtl fcst, 2=ind neg,3=indv pos, 4=cluster,5=whole ensemble
          kens(3)=7 !	     (3)   - ENSEMBLE IDENTIFIER ident number, if kens(2)=1 -> 1 high res, 2 low res if kens(2)=2 or 3 -> will = 1, 2, 3, ...
       ENDIF
    ENDIF
    KENS(4)=1!          (4)   - PRODUCT IDENTIFIER! no change, 1=full field
    KENS(5)=255!          (5)   - SMOOTHING FLAG ! no change, 255 original resolution

    CALL putgben(91,kf,kpds,kgds,kens,ibs,nbits,li,dummy,iret)
    WRITE(6,*) jpds(5),jpds(6),jpds(7),'IRET=' ,IRET
    IF (iret .NE. 0) WRITE(6,*) 'putgben error!'
    CALL BACLOSE(91,ierr)

  END SUBROUTINE postigge

END PROGRAM Tigge
