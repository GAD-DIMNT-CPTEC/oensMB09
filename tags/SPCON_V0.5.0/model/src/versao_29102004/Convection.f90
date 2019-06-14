!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:38 $
!  $Revision: 1.1.1.1 $
!
MODULE Convection

  !   InitConvection
  !
  !   gwater2-------|qnegat
  !                 |
  !                 |arprep-------|ras--------|qsat
  !                 |             |           |
  !                 |             |           |cloud-------|acritn
  !                 |             |                        |
  !                 |             |                        |rncl
  !                 |             |rnevp------|qstar9
  !                 |
  !                 |shllcl-------|mstad2
  !                 |
  !                 |kuolcl
  !                 |
  !                 |shalv2
  !                 |
  !                 |lrgscl


  USE Constants, ONLY :  &
       cp                 , &
       hl                 , &
       gasr               , &
       grav               , &
       rmwmd              , &
       rmwmdi             , &
       e0c                , &
       delq               , &     
       p00

  USE Diagnostics, ONLY:   &
       ndavl             , &
       dodia             , &
       updia             , &
       ndkuo             , &
       ndclh             , &
       ndcmc             , &
       ndsclh            , &
       ndscmc            , &
       ndto              , &
       ndsf              , &
       ndlslh            , &
       ndlsmc            , &
       ndls              , &
       ndnhcr 

  USE GridHistory, ONLY:   &
       IsGridHistoryOn   , &
       StoreGridHistory  , &
       ngkuo             , &
       ngclh             , &
       ngcmc             , &
       ngsclh            , &
       ngscmc            , &
       ngto              , &
       ngsf              , &
       nglslh            , &
       nglsmc            , &
       dogrh

  USE InputOutput, ONLY:   &
       nfprt

  USE Options, ONLY :       &
       rccmbl            , &
       sthick            , &
       sacum             , &
       acum0             , &
       tbase             , &
       ki                , &
       mlrg              , &
       is                , &
       iccon             , &
       ilcon             , &
       iscon             , &
       doprec            , &
       cflric            , &
       ifilt             , & 
       dt                , &
       kt                , & 
       ktp               , &
       jdt

  USE Init, ONLY :       &
       nls

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: InitConvection
  PUBLIC :: gwater2

  REAL :: aa(15) 
  REAL :: ad(15) 
  REAL :: ac(15)
  REAL :: actop  
  REAL :: thetae(151,181)       
  REAL :: tfmthe(431,241)
  REAL :: qfmthe(431,241)
  REAL :: ess
  INTEGER :: kbase
  INTEGER :: kcr
  REAL, ALLOCATABLE :: dels  (:)
  REAL, ALLOCATABLE :: gams  (:)
  REAL, ALLOCATABLE :: gammod(:)
  REAL, ALLOCATABLE :: delmod(:)   
  REAL :: rlocp
  REAL :: rgrav
  REAL :: rlrv
  REAL :: const1
  REAL :: const2
  REAL :: xx1

  REAL, PARAMETER :: xkapa=0.2857143

CONTAINS






  SUBROUTINE InitConvection(si, del, sl, cl, kmax)
    INTEGER, INTENT(IN) :: kmax
    REAL,    INTENT(IN) :: si (kmax+1)
    REAL,    INTENT(IN) :: del(kmax  )
    REAL,    INTENT(IN) :: sl (kmax  )
    REAL,    INTENT(IN) :: cl (kmax  )     

    CALL InitAcritn()

    CALL InitMstad2()

    CALL InitShalv2(si, del, sl, cl, kmax)

    CALL InitLrgscl()
  END SUBROUTINE InitConvection



  REAL FUNCTION es(t)
    REAL, INTENT(IN) :: t
    REAL :: tx
    REAL, PARAMETER  :: d1=.6107042e0 
    REAL, PARAMETER  :: d2=4.441157e-2
    REAL, PARAMETER  :: d3=1.432098e-3
    REAL, PARAMETER  :: d4=2.651396e-5
    REAL, PARAMETER  :: d5=3.009998e-7
    REAL, PARAMETER  :: d6=2.008880e-9
    REAL, PARAMETER  :: d7=6.192623e-12

    ! statement function

    tx = t - tbase
    IF (tx >= -50.0) THEN
       es = d1 + tx*(d2 + tx*(d3 + tx*(d4 + tx*(d5 + tx*(d6 + d7*tx)))))
    ELSE
       es=.00636e0*EXP(25.6e0*(tx+50.e0)/(tbase-50.e0))
    END IF
  END FUNCTION es





  SUBROUTINE InitAcritn()
    REAL,    PARAMETER :: actp=1.7
    REAL,    PARAMETER :: facm=1.0
    REAL    :: au(15) 
    REAL    :: ph(15) 
    REAL    :: tem
    INTEGER :: l

    ph =(/150.0, 200.0, 250.0, 300.0, 350.0, 400.0, 450.0, 500.0, &
         550.0, 600.0, 650.0, 700.0, 750.0, 800.0, 850.0/)

    aa =(/ 1.6851, 1.1686, 0.7663, 0.5255, 0.4100, 0.3677, &
         0.3151, 0.2216, 0.1521, 0.1082, 0.0750, 0.0664, &
         0.0553, 0.0445, 0.0633/)

    ad = 0.0
    ac = 0.0

    actop   = actp*facm

    DO l=1,15
       aa(l) = aa(l)*facm
    END DO

    DO l=2,15
       tem   = ph(l) - ph(l-1)
       au(l) = aa(l-1) / tem
       ad(l) = aa(l)   / tem
       ac(l) = ph(l)*au(l) - ph(l-1)*ad(l)
       ad(l) = ad(l) - au(l)
    END DO
  END SUBROUTINE InitAcritn



  ! acritn :relaxed arakawa-schubert.



  SUBROUTINE acritn(len, pl, plb, acr)
    INTEGER, INTENT(IN ) :: len
    REAL,    INTENT(IN ) :: pl(len) 
    REAL,    INTENT(IN ) :: plb(len) 
    REAL,    INTENT(OUT) :: acr(len)

    INTEGER :: i
    INTEGER :: iwk

    DO i = 1, len
       iwk = pl(i) * 0.02 - 0.999999999
       IF (iwk > 1) THEN
          IF (iwk <= 15) THEN
             acr(i) = ac(iwk) + pl(i) * ad(iwk)
          ELSE
             acr(i) = aa(15)
          END IF
       ELSE   
          acr(i) = actop
       END IF
       acr(i) = acr(i) * (plb(i) - pl(i))
    END DO
  END SUBROUTINE acritn



  ! qsat   :relaxed arakawa-schubert.



  SUBROUTINE qsat(tt,p,q,dqdt,ldqdt)
    REAL,    INTENT(IN )    :: tt
    REAL,    INTENT(IN )    :: p
    REAL,    INTENT(OUT)    :: q
    REAL,    INTENT(OUT)    :: dqdt  
    LOGICAL, INTENT(IN )    :: ldqdt

    REAL, PARAMETER :: airmw = 28.97   
    REAL, PARAMETER :: h2omw = 18.01   
    REAL, PARAMETER :: one   = 1.0   
    REAL, PARAMETER :: esfac = h2omw/airmw        
    REAL, PARAMETER :: erfac = (one-esfac)/esfac 
    REAL, PARAMETER :: b6  = 6.136820929e-11*esfac
    REAL, PARAMETER :: b5  = 2.034080948e-8 *esfac
    REAL, PARAMETER :: b4  = 3.031240396e-6 *esfac
    REAL, PARAMETER :: b3  = 2.650648471e-4 *esfac
    REAL, PARAMETER :: b2  = 1.428945805e-2 *esfac
    REAL, PARAMETER :: b1  = 4.436518521e-1 *esfac
    REAL, PARAMETER :: b0  = 6.107799961e+0 *esfac
    REAL, PARAMETER :: c1  = b1   
    REAL, PARAMETER :: c2  = b2*2.
    REAL, PARAMETER :: c3  = b3*3.
    REAL, PARAMETER :: c4  = b4*4.
    REAL, PARAMETER :: c5  = b5*5.
    REAL, PARAMETER :: c6  = b6*6.
    REAL, PARAMETER :: tmin=223.15
    REAL, PARAMETER :: tmax=323.15 
    REAL, PARAMETER :: tice=273.16
    REAL :: t
    REAL :: d

    t =  MIN ( MAX (tt,tmin),tmax) - tice
    q = (t*(t*(t*(t*(t*(t*b6+b5)+b4)+b3)+b2)+b1)+b0)
    d = one / (p-erfac*q)
    q = q * d

    IF (ldqdt)  THEN
       dqdt = (t*(t*(t*(t*(t*c6+c5)+c4)+c3)+c2)+c1)
       dqdt = (dqdt + erfac*q) * d
    END IF

  END SUBROUTINE qsat



  ! rncl   :relaxed arakawa-schubert



  SUBROUTINE rncl(len, pl, rno, clf)
    INTEGER, INTENT(IN ) :: len
    REAL,    INTENT(IN ) :: pl (len)  
    REAL,    INTENT(OUT) :: rno(len)
    REAL,    INTENT(OUT) :: clf(len)

    REAL, PARAMETER :: p5=500.0
    REAL, PARAMETER :: p8=800.0
    REAL, PARAMETER :: pt8=0.8
    REAL, PARAMETER :: pfac=0.2/(p8-p5)
    REAL, PARAMETER :: cucld=0.5
    INTEGER :: i

    DO i = 1, len
       rno(i) = 1.0
       clf(i) = cucld
       IF (pl(i) >= p5 .AND. pl(i) <= p8) THEN
          rno(i) = (p8-pl(i))*pfac + pt8
       ELSE IF (pl(i) > p8) THEN
          rno(i) = pt8
       END IF
    END DO
  END SUBROUTINE rncl





  SUBROUTINE InitMstad2()
    REAL          :: t
    REAL          :: esat  (151)
    REAL          :: kappa
    REAL          :: eps  
    REAL          :: el
    REAL          :: p
    REAL          :: ratio
    REAL          :: power  
    REAL          :: pd
    REAL          :: pdkap
    REAL          :: crit   
    REAL          :: thee
    REAL          :: fun
    REAL          :: dfun
    REAL          :: chg
    INTEGER       :: i
    INTEGER       :: k
    INTEGER       :: l
    REAL, PARAMETER :: rv=461.5e0
    REAL, PARAMETER :: cl=4187.e0

    ! set up tables of theta e of p and t, and of t of theta e and p

    kappa=gasr/cp
    eps=gasr/rv
    t=180.0

    DO i=1,151
       el=hl-(cl-cp)*(t-tbase)
       esat(i) = es(t)
       p=0.3
       DO k=1,181
          ratio=eps*esat(i)/(1.e2*p-esat(i) )
          power=el*ratio/(cp*t)
          pd=p-1.0e-2*esat(i)
          pdkap=kappa*LOG(pd)
          pdkap=EXP(pdkap)
          thetae(i,k)=t* EXP (power)/(pdkap)
          p=p+0.005
       END DO
       t=t+1.0
    END DO

    ! construct table to get temperature from thetae and p
    ! the table is tfmthe(i,k), where
    ! thetae=220,230,--,500 = 210+10*i, and
    ! p=press/100 cb = 0,.1,.2,--,1.1 =  0.1*(k-1)
    ! for p=0, tfmthe(i,1) is identically 0.0e0

    crit=2.0**(-23)
    thee=170.0
    DO i=1,431
       tfmthe(i,1) = 0.0e0
       qfmthe(i,1) = 0.0e0
       p=0.005
       DO k=2,241

          ! first guess

          IF (p < 0.025) THEN
             t=100.0
          ELSE IF (p < 0.05) THEN
             t=tbase
          ELSE
             t=300.0
          END IF

          ! newton iteration method

          DO l=1,100
             !     
             !     sat vapor pressure
             !     
             ess = es(t)
             ratio=eps*ess/(1.e2*p-ess)
             el=hl-(cl-cp)*(t-tbase)
             power=ratio*el/(cp*t)
             pdkap=p-1.0e-2*ess
             pdkap=kappa*LOG(pdkap)
             pdkap=EXP(pdkap)
             fun=t* EXP (power)/pdkap
             dfun=(fun/t)*(1.0e0+(ratio/(cp*t) )*((cp-cl)*t &
                  +(p/(p-1.0e-2*ess))*(el*el/(rv*t))))
             chg=(thee-fun)/dfun
             t=t+chg
             IF( ABS(chg) .LT. crit) EXIT
          END DO
          tfmthe(i,k)=t
          !     
          !     sat vapor pressure
          !     
          ess = es(t)
          qfmthe(i,k)=eps*ess/(1.e2*p-(1.0e0-eps)*ess)
          p=p+0.005
       END DO
       thee=thee+1.0
    END DO
  END SUBROUTINE InitMstad2




  ! mstad2 :this subroutine lifts the parcel from the lcl and computes the
  !         parcel temperature at each level; temperature is retrieved from
  !         lookup tables defined in the first call.
  !         This version-mstad2-lifts parcel from lifting condensation
  !         level, where pres is slcl and temp is tlcl
  !         level ll is the first regular level above the lcl
  !         this version contains double resolution tables
  !         and uses virtual temp in the buoyancy test
  !         new sat vapor pressure used





  SUBROUTINE mstad2(h, press, tin, tmst, qmst, ktop, ier, &
       slcl, ll, qin, tlcl, llift, ncols, kmax)

    !
    ! this routine accepts input data of
    !         h                  = surface pressure dvded by 100 cbs
    !         press(k=1,--,km)   = factor such that p at lvl k = h*press(k)
    !         tin(1 + (k-1)*it ) = input temps in a column
    !         km                 = number of levels
    ! and returns
    !         the = equiv pot temp calc from parcel p and t at its lcl
    !         ktop (g.t.e 1 and l.t.e. km) is highest lvl for which
    !              tin is colder than moist adiabat given by the
    !              (ktop=1 denotes tin(k=2) is already g.t.e. moist adb.)
    !                   allowance is made for perhaps one level below ktop
    !                  at which tin was warmer than tmst.
    !         tmst(k) and qmst(k) for k=2,--,ktop, are temp and sat spec
    !         hums on moist adb the.
    !         pressure in layer one must lie between 50 and 110 cbs.
    !         temp in layer one must lie between 220 and 330 degrees
    !         the resulting the must lie between 220 and 500 degrees.
    !         ( the is tested for this possibility, an error return of
    !         ier=0 denoting okeh conditions, a return of ier=1 denoting
    !         violation of this range.)
    !==========================================================================
    ! ncols......Number of grid points on a gaussian latitude circle  
    ! kmax......Number of sigma levels
    ! cp........Specific heat of air           (j/kg/k)        
    ! hl........heat of evaporation of water     (j/kg)     
    ! gasr......gas constant of dry air        (j/kg/k)   
    ! tbase.....temperature of fusion of ice  
    ! tmst......temperature on moist adb
    ! qmst......spec hums on moist adb
    ! qin.......spec hums in a column  
    ! slcl......pressure of  lifting condensation level
    ! tlcl......temp of  lifting condensation level 
    ! ll........is the first regular level above the lcl 
    ! llift
    !==========================================================================
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    REAL,    INTENT(in   ) :: press(kmax)
    REAL,    INTENT(in   ) :: tin  (ncols,kmax)
    REAL,    INTENT(inout) :: tmst (ncols,kmax)
    REAL,    INTENT(inout) :: qmst (ncols,kmax)
    REAL,    INTENT(in   ) :: qin  (ncols,kmax)
    REAL,    INTENT(in   ) :: h    (ncols)
    REAL,    INTENT(in   ) :: slcl (ncols)
    REAL,    INTENT(in   ) :: tlcl (ncols)       
    INTEGER, INTENT(inout) :: ktop (ncols)
    INTEGER, INTENT(out  ) :: ier  (ncols)
    INTEGER, INTENT(in   ) :: ll   (ncols)
    LOGICAL, INTENT(in   ) :: llift(ncols)
    !
    !  local arrays (temporary storage)
    !   
    REAL          :: pp    (ncols)
    REAL          :: ti    (ncols)
    INTEGER       :: jt    (ncols)
    REAL          :: x     (ncols)
    REAL          :: pk    (ncols)
    INTEGER       :: kp    (ncols)
    REAL          :: y     (ncols)
    REAL          :: yy    (ncols)
    REAL          :: xx    (ncols)
    REAL          :: the   (ncols)
    REAL          :: tk    (ncols)
    INTEGER       :: kt    (ncols)  
    REAL          :: pi    (ncols)
    INTEGER       :: ip    (ncols)
    INTEGER       :: lstb  (ncols)
    INTEGER       :: lcld  (ncols)
    REAL          :: tvdiff(ncols)
    INTEGER       :: ktop1 (ncols)

    INTEGER       :: i
    INTEGER       :: k
    CHARACTER(LEN=*), PARAMETER :: hName="**(mstad2)**"

    !
    !     surface press is h, pressure at other lvls is h times press(k)
    !     compute theta e  = the  , for layer one
    !
    DO i=1,ncols
       ier(i) = 0
       IF(llift(i)) THEN
          pp(i)=h(i)*slcl(i)
          ti(i)=tlcl(i)-179.0
          jt(i)=ti(i)
          IF(jt(i).LT.1.OR.jt(i).GT.150) THEN
             ier(i) = 1
          ENDIF
       ENDIF
    END DO

    DO i=1,ncols
       IF(llift(i).AND.ier(i).EQ.0) THEN
          x(i)=ti(i)-jt(i)
          pk(i)=200.0 *pp(i)-59.0
          kp(i)=pk(i)
          !
          !     check for pressure less than 500 mb at lcl -
          !
          IF (kp(i).LT.1 .OR. kp(i).GT.180) THEN
             ier(i) = 1
          ENDIF
       ENDIF
    END DO

    DO i=1,ncols
       IF(llift(i).AND.ier(i).EQ.0) THEN
          y(i)=pk(i)-kp(i)
          yy(i)=1.0e0-y(i)
          xx(i)=1.0e0-x(i)
          the(i)=xx(i)*(yy(i)*thetae(jt(i),kp(i)) &
               +y(i)*thetae(jt(i),kp(i)+1)) &
               +x(i)*(yy(i)*thetae(jt(i)+1,kp(i)) &
               +y(i)*thetae(jt(i)+1,kp(i)+1))
          !     
          !     get t and q on mst adiabat for layers which are colder than
          !     the mst adiabat
          !
          tk(i)=the(i)-169.0
          kt(i)=tk(i)
          y(i)=tk(i)-kt(i)
          yy(i)=1.0e0-y(i)
          !     
          !     test limits on the
          !
          IF(kt(i).LT.1 .OR. kt(i).GT.430) THEN
             ier(i) = 1
          ENDIF

       ENDIF
    END DO
    !     
    !     first put tmst=tin and qmst=qsat(tin) for k=1
    !     
    DO i=1,ncols
       IF(llift(i).AND.ier(i).EQ.0) THEN
          tmst(i,1)=tlcl(i)
          pi(i)=200.0*pp(i)+1.0
          ip(i)=pi(i)
          x(i)=pi(i)-ip(i)
          qmst(i,1)=(1.0e0-x(i))*(yy(i)*qfmthe(kt(i),ip(i)) &
               +y(i)*qfmthe(kt(i)+1,ip(i)) ) &
               +x(i)*(yy(i)*qfmthe(kt(i),ip(i)+1) &
               +y(i)*qfmthe(kt(i)+1,ip(i)+1) )
       ENDIF
    END DO

    DO k = 2,kmax
       DO i=1,ncols
          tmst(i,k) = tmst(i,1)
          qmst(i,k) = qmst(i,1)
       END DO
    END DO
    !     
    !     we will allow one stable layer (with tin g.t. tmst) to
    !     interrupt a sequence of unstable layers.
    !
    DO i=1,ncols
       IF(llift(i).AND.ier(i).EQ.0) THEN
          ktop(i)=ll(i)-1
          IF (ktop(i) <= 0) ktop(i)=1
          lstb(i)=0
          lcld(i)=0
       ENDIF
    END DO

    DO k=1,kmax
       DO i=1,ncols
          IF(llift(i).AND.ier(i).EQ.0.AND.lcld(i).EQ.0  &
               .AND.k.GE.ll(i)) THEN
             pp(i)=h(i)*press(k)
             pi(i)=200.0 *pp(i)+1.0
             ip(i)=pi(i)
             x(i)=pi(i)-ip(i)
             tmst(i,k)=(1.0e0-x(i))*(yy(i)*tfmthe(kt(i),ip(i)) &
                  +y(i)*tfmthe(kt(i)+1,ip(i))) &
                  +x(i)*(yy(i)*tfmthe(kt(i),ip(i)+1) &
                  +y(i)*tfmthe(kt(i)+1,ip(i)+1))
             qmst(i,k)=(1.0e0-x(i))*(yy(i)*qfmthe(kt(i),ip(i)) &
                  +y(i)*qfmthe(kt(i)+1,ip(i))) &
                  +x(i)*(yy(i)*qfmthe(kt(i),ip(i)+1) &
                  +y(i)*qfmthe(kt(i)+1,ip(i)+1))
             !
             !     buoyancy test with virtual temp correction
             !
             tvdiff(i) = tmst(i,k)-tin(i,k)  &
                  + .61e0 *tin(i,k)*(qmst(i,k) - qin(i,k))
             IF(tvdiff(i).LE.0.) THEN 
                IF(lstb(i).EQ.0) THEN
                   lstb(i) = 1
                   ktop1(i) = ktop(i)
                   ktop(i)  = ktop(i) + 1
                ELSE
                   lcld(i) = 1
                   IF((ktop(i)-ktop1(i)-1).LE.0) THEN
                      ktop(i) = ktop1(i)
                   ENDIF
                ENDIF
             ELSE
                ktop(i)=ktop(i)+1
             ENDIF

          ENDIF
       END DO
    END DO

    IF (ANY(ier /=0)) THEN
       WRITE (0, '(a, " some ier /= 0; will stop")') hName
       STOP "** ERROR AT MSTAD2 **"
    END IF
  END SUBROUTINE mstad2



  ! kuolcl :calculates the temperature and specific humidity tendencies due
  !         to the kuo convective precipitation parameterization.



  SUBROUTINE kuolcl(dt, nkuo, ps, del, sl, si, qn, qn1, &
       tn1, dq, geshem, msta, kuo, plcl, kktop, &
       kkbot, ncols, kmax)

    !
    !==========================================================================
    !==========================================================================
    ! imx.......=ncols+1 or ncols+2   :this dimension instead of ncols
    !              is used in order to avoid bank conflict of memory
    !              access in fft computation and make it efficient. the
    !              choice of 1 or 2 depends on the number of banks and
    !              the declared type of grid variable (real*4,real*8)
    !              to be fourier transformed.
    !              cyber machine has the symptom.
    !              cray machine has no bank conflict, but the argument
    !              'imx' in subr. fft991 cannot be replaced by ncols     
    ! kmax.......Number of sigma levels
    ! ncols.......Number of grid points on a gaussian latitude circle  
    ! dt.........time interval,usually =delt,but changes
    !            in nlnmi (dt=1.) and at dead start(delt/4,delt/2)
    ! nkuo.......index used by routine "conkuo"
    !            to accumulate number of
    !            points for which trouble was
    !            encountered while computing
    !            a moist adiabat.
    ! ps.........sfc pres (cb)   
    ! del........sigma spacing for each layer computed in routine "setsig".     
    ! sl.........sigma value at midpoint of
    !                                         each layer : (k=287/1005)
    !
    !                                                                     1
    !                                             +-                   + ---
    !                                             !     k+1         k+1!  k
    !                                             !si(l)   - si(l+1)   !
    !                                     sl(l) = !--------------------!
    !                                             !(k+1) (si(l)-si(l+1)!
    !                                             +-                  -+     
    ! si........si(l)=1.0-ci(l).  
    ! ci........sigma value at each level.       
    ! qn........qn is q (specific humidit) at the n-1 time level 
    ! qn1.......qn1 is q (specific humidit) at the n+1 time level     
    ! tn1.......tn1 is tmp (temperature) at the n+1 time level         
    ! dq.........specific humidit difference between levels  
    ! geshem.....set aside convective precip in separate array for diagnostics
    ! msta.......
    ! kuo........flag to indicate that deep convection was done
    !            kuo, ktop and plcl are longitude arrays   
    ! plcl.......pressure at the lcl
    ! kktop......ktop (g.t.e 1 and l.t.e. km) is highest lvl for which
    !            tin is colder than moist adiabat given by the
    !            (ktop=1 denotes tin(k=2) is already g.t.e. moist adb.)
    !            allowance is made for perhaps one level below ktop
    !            at which tin was warmer than tmst.
    ! kkbot......is the first regular level above the lcl   
    ! cp.........Specific heat of air           (j/kg/k)      
    ! hl.........heat of evaporation of water     (j/kg)      
    ! gasr.......gas constant of dry air        (j/kg/k)   
    ! g..........grav   gravity constant        (m/s**2)      
    ! rmwmd......fracao molar entre a agua e o ar  
    ! sthick.....upper limit for originating air for lcl.  replaces kthick. 
    ! sacum......top level for integrated moisture convergence test. replaces
    !            kacum
    ! acum0......threshold moisture convergence such that integrated moisture
    !            convergence > - acum0 for convection to occur. 
    ! tbase......constant tbase =  273.15e00  
    ! ki.........lowest level from which parcels can be lifted to find lcl    
    INTEGER, INTENT(in   ) :: kmax
    INTEGER, INTENT(in   ) :: ncols
    REAL,    INTENT(in   ) :: dt
    INTEGER, INTENT(inout) :: nkuo
    REAL,    INTENT(in   ) :: ps     (ncols)
    REAL,    INTENT(in   ) :: del    (kmax)
    REAL,    INTENT(in   ) :: sl     (kmax)
    REAL,    INTENT(in   ) :: si     (kmax+1)
    REAL,    INTENT(in   ) :: qn     (ncols,kmax)
    REAL,    INTENT(inout) :: qn1    (ncols,kmax)
    REAL,    INTENT(inout) :: tn1    (ncols,kmax)
    REAL,    INTENT(inout) :: dq     (ncols,kmax)  
    REAL,    INTENT(inout) :: geshem (ncols)
    INTEGER, INTENT(inout) :: msta
    INTEGER, INTENT(out  ) :: kuo    (ncols)  
    REAL,    INTENT(inout) :: plcl   (ncols) 
    INTEGER, INTENT(inout) :: kktop  (ncols)
    INTEGER, INTENT(out  ) :: kkbot  (ncols)
    !
    !     these are for monitoring of gpv in gfidi.
    !
    REAL    :: press (ncols,kmax)
    REAL    :: tin   (ncols,kmax)
    REAL    :: qin   (ncols,kmax)
    REAL    :: tmst  (ncols,kmax)
    REAL    :: qmst  (ncols,kmax)
    REAL    :: dtkuo (ncols,kmax)
    REAL    :: dqkuo (ncols,kmax)
    REAL    :: esat  (ncols)
    REAL    :: dtvirt(ncols,kmax)
    REAL    :: deltaq(ncols,kmax)
    REAL    :: tpar  (ncols)
    REAL    :: espar (ncols)
    REAL    :: qspar (ncols)
    REAL    :: qpar  (ncols)
    REAL    :: qex1  (ncols)
    REAL    :: tlcl  (ncols)
    REAL    :: qexces(ncols)
    REAL    :: dqdp  (ncols)
    REAL    :: deltap(ncols)
    REAL    :: unstab(ncols)
    REAL    :: water (ncols)
    REAL    :: q1    (ncols)
    REAL    :: q2    (ncols)
    REAL    :: qsatsm(ncols)
    REAL    :: qsum  (ncols)
    REAL    :: qsatk (ncols)
    REAL    :: x     (ncols)
    REAL    :: ubar  (ncols)
    REAL    :: b     (ncols)
    REAL    :: qeff1 (ncols)
    REAL    :: qeff2 (ncols)
    REAL    :: pcpwat(ncols)
    REAL    :: hnew  (ncols)
    REAL    :: slcl  (ncols)
    REAL    :: localAcum(ncols)
    INTEGER :: ier(ncols)

    LOGICAL :: llift(ncols)
    LOGICAL :: lconv(ncols)
    INTEGER :: ll   (ncols)  
    REAL    :: sigtop
    INTEGER :: ksgtop
    REAL    :: cappa
    REAL    :: rdt
    REAL    :: cpovl
    INTEGER :: kthick
    INTEGER :: kacum
    INTEGER :: i
    INTEGER :: k
    INTEGER :: kk

    !INTEGER, SAVE :: ifp
    !DATA ifp/1/     

    cappa=gasr/cp

    IF(dt.EQ.0.0e0) RETURN
    !
    !     set default values
    !
    DO i=1,ncols
       kktop(i)=1
       kkbot(i)=1
       kuo  (i)=0
       ll(i)=0
       llift(i)=.FALSE.
       lconv(i)=.FALSE.
    END DO
    !     
    !     define kthick in terms of sigma values
    !
    DO k=1,kmax
       IF(si(k).GE.sthick.AND.si(k+1).LT.sthick) THEN
          kthick = k
          go to 12
       ENDIF
    END DO
12  CONTINUE
    DO k=1,kmax
       IF(si(k).GE.sacum.AND.si(k+1).LT.sacum) THEN
          kacum = k
          go to 14
       ENDIF
    END DO
14  CONTINUE
    sigtop=0.075
    DO k=1,kmax
       IF(si(k).GE.sigtop.AND.si(k+1).LT.sigtop) THEN
          ksgtop = k
          go to 16
       ENDIF
    END DO
16  CONTINUE
    !IF (ifp .EQ. 1) THEN
    !   ifp=0
    !   WRITE (*, '(3(a,i5))') ' kthick= ', kthick, &
    !        ' kacum= ', kacum, ' ksgtop= ', ksgtop
    !ENDIF
    rdt=1.0e0/dt
    cpovl= cp/hl
    kk = kmax
    !
    !     qn is q at the n-1 time level
    !     del=del sigma (del p ovr psfc)
    !     ps=sfc pres (cb)
    !     calculate dq only up to levh - set to zero above levh
    !
    DO k=1,kmax
       DO i=1,ncols
          dq(i,k) = qn1(i,k) - qn(i,k)
       END DO
    END DO
    !
    !     net time rate moisture inflow ..lwst kacum.. lyrs
    !     acum is a leapfrog quantity, like dq
    !
    DO i=1, ncols
       localAcum(i)= acum0
       hnew(i) = 1.0e-2*ps(i)
    END DO

    DO k=1,kacum
       DO i=1, ncols
          localAcum(i)=localAcum(i)+rdt*dq(i,k)*del(k)
       END DO
    END DO

    DO i=1,ncols
       IF(localAcum(i).LT.0.) kuo(i) =  2
    END DO

    DO k=1,kmax
       DO i=1,ncols
          press(i,k)=sl(k)*ps(i)
       END DO
    END DO
    !
    !     tin, qin are prelim tmp and q at n+1
    !     zero q if it is  negative
    !
    DO k=1,kmax
       DO i=1,ncols
          qin(i,k) = qn1(i,k)
          IF (qn1(i,k).LE.0.0e0) qin(i,k) = 1.0e-12
       END DO
    END DO

    DO k=1,kmax
       DO i=1,ncols
          tin(i,k) = tn1(i,k)
       END DO
    END DO

    DO i=1,ncols
       qex1(i) = 0.0e0
       qpar(i) = qin(i,ki)
    END DO
    !     
    !     lift parcel from k=ki until it becomes saturated
    !
    DO k =ki,kthick
       DO i=1,ncols
          IF(.NOT.llift(i)) THEN
             tpar(i) = tin(i,ki)* &
                  EXP(cappa*LOG(press(i,k)/press(i,ki)))
             espar(i) = es(tpar(i))
             qspar(i) =rmwmd*espar(i)/ &
                  (press(i,k)-(1.0e0-rmwmd)*espar(i))
             qexces(i) = qpar(i) - qspar(i)
             !
             !     if parcel not saturated,  try next level
             !
             IF (qexces(i).LT.0.0e0) THEN
                qex1(i) = qexces(i)
                !
                !     saturated - go down and find p,t, sl at the lcl;
                !     if sat exists in first layer (k=ki), use this as lcl
                !
             ELSE IF (k.EQ.ki) THEN
                plcl(i) = press(i,k)
                tlcl(i) = tpar(i)
                tlcl(i) = tlcl(i) + 1.0e0
                slcl(i) = plcl(i)/ps(i)
                ll(i)   = k
                kkbot(i) = ll(i)
                llift(i) = .TRUE.
             ELSE
                dqdp(i) = (qexces(i)-qex1(i))/ &
                     (press(i,k-1)-press(i,k))
                deltap(i)= qexces(i)/dqdp(i)
                plcl(i)=press(i,k) + deltap(i)
                tlcl(i)=  tpar(i) * (1.0e0+2.0e0*cappa*deltap(i) &
                     /(press(i,k)+press(i,k-1)))
                tlcl(i) = tlcl(i) + 1.0e0
                slcl(i) = plcl(i)/ps(i)
                ll(i)   = k
                kkbot(i) = ll(i)
                llift(i) = .TRUE.
                !
                !     give parcel a one-degree goose
                !
             ENDIF
             !
             !     lifting cond level found - get out of loop
             !
          ENDIF
       END DO
    END DO
    !
    !     quit if parcel still unsat at k = kthick - - -
    !     in this case,set low value of plcl as signal to shalmano
    !
    DO i=1,ncols
       IF(.NOT.llift(i)) THEN
          plcl(i) = 1.0e0
          kuo  (i) = 5
       ENDIF
    END DO

    CALL mstad2(hnew  ,sl    ,tin   ,tmst  ,qmst  ,kktop ,ier   , &
         slcl  ,ll    ,qin   ,tlcl  ,llift ,ncols  ,kmax)
    !
    !     tmst and qmst,k=1...ktop contain cloud temps and specific humid.
    !     store values of plcl and ktop to pass to shalmano
    !
    DO i=1,ncols
       IF(llift(i)) THEN
          IF(ier(i).NE.0) THEN
             msta = msta+1
             kuo(i) =6
             kktop(i) = 1
             plcl(i) = 10.e0
             lconv(i)=.FALSE.
          ELSE
             lconv(i)=.TRUE.
          ENDIF
       ENDIF
    END DO
    !
    !     test 2...thickness of unstable region must exceed 300 mb
    !
    DO i=1,ncols
       IF(lconv(i)) THEN
          unstab(i) = sl(ll(i)) - sl(kktop(i))
          IF (unstab(i).LT.0.3e0)kuo(i)=7
          !
          !     here  kuo requirements are met, first compute water=tons water
          !     subst accum in zero leap-frog time step per sq m in lyrs 1-k
          !     avl  water  (water=water*g/ps).gt.zero)...
          !
          water(i)=0.0e0
       ENDIF
    END DO

    DO k=1,kmax
       DO i=1,ncols
          IF(lconv(i).AND.k.LE.kktop(i)) THEN
             water(i)=water(i)+  dq(i,k)*del(k)
          ENDIF
       END DO
    END DO

    DO i=1,ncols
       IF(lconv(i)) THEN
          IF(water(i).LE.0.0e0) kuo(i)=8
          IF(kuo(i).GT.0) lconv(i)=.FALSE.
       ENDIF
    END DO

    DO i=1,ncols
       IF(lconv(i)) THEN
          q1(i)=0.0e0
          q2(i)=0.0e0
          qsatsm(i)=0.0e0
          qsum(i)  =0.0e0
       ENDIF
    END DO
    !
    !     calculate four vertical averages - sat deficit of environment,
    !     virt temp excess of cloud, and  q, qsat for environment ---
    !
    DO k=1,kmax
       DO i=1,ncols
          IF(lconv(i).AND.k.GE.ll(i).AND.k.LE.kktop(i)) THEN
             esat(i) = es(tin(i,k))
             qsatk(i)=rmwmd*esat(i)/(press(i,k)-(1.0e0-rmwmd)*esat(i))
             x(i) = qsatk(i) - qin(i,k)
             deltaq(i,k) = x(i)
             q1(i) = q1(i) + x(i)*del(k)
             qsum(i) = qsum(i) + qin(i,k)*del(k)
             qsatsm(i) = qsatsm(i) + qsatk(i)*del(k)
             x(i)=tmst(i,k)-tin(i,k)+0.61e0*tin(i,k)*(qmst(i,k)-qin(i,k))
             dtvirt(i,k) = x(i)
             q2(i)=q2(i)+x(i)*del(k)
          ENDIF
       END DO
    END DO

    DO i=1,ncols
       IF(lconv(i)) THEN
          q2(i)=q2(i)*cpovl
          IF (q1(i).LE.0.0e0) q1(i) = 1.0e-9
          IF (q2(i).LE.0.0e0) q2(i) = 1.0e-9
          ubar(i) = qsum(i)/qsatsm(i)
          IF (ubar(i).GE.1.0e0) ubar(i) =0.999e0
          b(i) = 1.0e0-ubar(i)
          IF (b(i).GT.1.0e0) b(i)=1.0e0
          qeff1(i) = water(i) * b(i)/q1(i)
          qeff2(i) = water(i) *(1.0e0-b(i))/q2(i)
          IF(qeff1(i).LT.0.002e0) lconv(i)=.FALSE.
          qeff1(i)=MIN(qeff1(i),1.0e0)
          qeff2(i)=MIN(qeff2(i),1.0e0)
       ENDIF
    END DO
    !
    ! exclude convective clouds from top of model
    ! 
    DO i=1,ncols
       ! if (kktop(i) .eq. kmax) kktop(i)=kmax-1
       kktop(i)=MIN(kktop(i),ksgtop)
    END DO

    DO k=1,kmax
       DO i=1,ncols
          IF(lconv(i).AND.k.LE.kktop(i)) THEN
             IF(k.LT.ll(i)) THEN
                dtkuo(i,k) = 0.0e0
                dqkuo(i,k) = 0.0e0
             ELSE
                dqkuo(i,k) = qeff1(i) * deltaq(i,k)
                dtkuo(i,k) = qeff2(i) * dtvirt(i,k)
             ENDIF
             !
             !     start loop at 1 to record loss of dq in diagnostics for k lt. ll
             !
             tin(i,k)=tin(i,k)+dtkuo(i,k)
             qin(i,k)=dqkuo(i,k)+ qin(i,k)
          ENDIF
       END DO
    END DO
    !     
    !     calculate convective precipitation from latent heating
    !
    DO i=1,ncols
       IF(lconv(i)) THEN
          pcpwat(i) = qeff2(i)*q2(i)
          !
          !     set aside convective precip in separate array for diagnostics
          !
          geshem(i) = geshem(i)+ps(i)*pcpwat(i)/(grav*2.0e0)
       ENDIF
    END DO
    !
    !     remove all water vapor that was set aside in 'water' sum---
    !
    DO k=1,kmax
       DO i=1,ncols
          IF(lconv(i).AND.k.LE.kktop(i)) THEN
             qin(i,k) = qin(i,k) -dq(i,k)
          ENDIF
       END DO
    END DO
    !
    !     restore basic fields
    !
    DO k=1,kmax
       DO i=1,ncols
          IF(lconv(i).AND.k.LE.kktop(i)) THEN
             tn1(i,k)=tin(i,k)
             qn1(i,k)=qin(i,k)
          ENDIF
       END DO
    END DO

    DO i=1,ncols
       IF(lconv(i)) THEN
          nkuo=nkuo+1
          kuo(i)=1
       ENDIF
    END DO
  END SUBROUTINE kuolcl







  SUBROUTINE InitShalv2(si, del, sl, cl, kmax)
    INTEGER, INTENT(IN) :: kmax
    REAL,    INTENT(IN) :: si (kmax+1)
    REAL,    INTENT(IN) :: del(kmax  )
    REAL,    INTENT(IN) :: sl (kmax  )
    REAL,    INTENT(IN) :: cl (kmax  )     

    INTEGER :: n
    REAL    :: gr
    REAL    :: ggrr
    REAL    :: dryl

    ALLOCATE (dels  (kmax-1))
    ALLOCATE (gams  (kmax-1))
    ALLOCATE (gammod(kmax-1))
    ALLOCATE (delmod(kmax-1))

    gr=2.0e0*grav/gasr
    ggrr=gr*gr
    dryl=grav/cp
    DO n = 1, kmax-1
       dels(n)=ggrr*si(n+1)*si(n+1)/ ( &
            del(n)*( cl(n+1) - cl(n) )    )
       delmod(n)=gr*dryl*si(n+1)/del(n)
       gams(n)=del(n)*dels(n)/del(n+1)
       gammod(n)=del(n)*delmod(n)/del(n+1)
       IF(sl(n).GT.0.7e0) THEN
          kbase=n
          kcr=n
       END IF
    END DO
  END SUBROUTINE InitShalv2




  ! shalv2 :subgrid scale shallow cumulus parameterization - enhanced
  !         vertical temperature and moisture diffusion returning adjusted
  !         temperature and specific humidity.



  SUBROUTINE shalv2(si, sl, tin, qin, ps, deltim, &
       ktop, plcl, kuo, kmaxp, kctop1, kcbot1, noshal1, &
       newr, ncols, kmax)
    !
    !**************************************************************************
    !
    !         use:
    !         sr to compute the parameterized effects of shallow
    !         convective clouds (vector version).  routine used in
    !         gwater  moist convection kuolcl.
    !
    !         purpose:
    !         sub-grid-scale shallow convective cloud parameterization.
    !         this routine computes the effects of shallow convection
    !         based on tiedtke (1984), ecmwf workshop on convection in
    !         large-scale numerical models.
    !         tapered k profile in cloud  developed by caplan and long.
    !
    !         srs called ... none
    !
    !
    !         jpg modifications nov 6 85
    !
    ! input
    !
    !     si    : p/ps at sigma level interfaces (array from surface up)
    !     del   : del p positive across sigma lyr  (array from surface up)
    !     sl    : p/ps at sigma layers               (array from surface up)
    !     cl    : 1 - sl                     (array from surface up)
    !     tin   : temperature       (longitude, height array, deg k)
    !     qin   : specific humidity (longitude, height array, gm/gm)
    !     ps    : surface pressure  (longitude array        , cb   )
    !     deltim: timestep (  sec  )
    !     ktop  : cloud tops (sigma layer index) passed from conkuo
    !     plcl  : pressure   (cb) at lcl   passed from conkuo
    !     kuo   : flag to indicate that deep convection was done
    !             kuo, ktop and plcl are longitude arrays
    !     newr  : flag for ccm3 based cloud radiation 
    !     output
    !
    !     qin   : updated specific humidity    (gm/gm)
    !     tin   : updated temperature          (deg k)
    !
    !
    !     external variables
    !**************************************************************************
    ! ncols.......Number of grid points on a gaussian latitude circle
    ! kmax.......Number of sigma levels  
    ! kmaxp......kmaxp=kmax+1
    ! deltim.....deltim=dt where dt time interval,usually =delt,but changes
    !            in nlnmi (dt=1.) and at dead start(delt/4,delt/2)
    ! si.........si(l)=1.0-ci(l).  
    ! ci.........sigma value at each level.       
    ! del........sigma spacing for each layer computed in routine "setsig".     
    ! ps.........surface pressure        
    ! sl.........sigma value at midpoint of
    !                                         each layer : (k=287/1005)
    !
    !                                                                     1
    !                                             +-                   + ---
    !                                             !     k+1         k+1!  k
    !                                             !si(l)   - si(l+1)   !
    !                                     sl(l) = !--------------------!
    !                                             !(k+1) (si(l)-si(l+1)!
    !                                             +-                  -+     
    ! kcbot1.... 
    ! kctop1.... 
    ! noshal1...
    ! cp........Specific heat of air           (j/kg/k)       
    ! hl........heat of evaporation of water     (j/kg)     
    ! gasr......gas constant of dry air        (j/kg/k)     
    ! grav......grav   gravity constant        (m/s**2)     
    !**************************************************************************
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    INTEGER, INTENT(in   ) :: kmaxp
    REAL,    INTENT(in   ) :: deltim    
    REAL,    INTENT(in   ) :: si     ( kmaxp )
    REAL,    INTENT(in   ) :: ps     ( ncols )  
    REAL,    INTENT(in   ) :: sl     ( kmax  )
    REAL,    INTENT(inout) :: plcl   ( ncols )
    INTEGER, INTENT(in   ) :: ktop   ( ncols )
    INTEGER, INTENT(in   ) :: kuo    ( ncols )  
    REAL,    INTENT(inout) :: tin    ( ncols , kmax )
    REAL,    INTENT(inout) :: qin    ( ncols , kmax )
    LOGICAL, INTENT(in   ) :: newr   
    INTEGER, INTENT(inout) :: kcbot1 (ncols) 
    INTEGER, INTENT(out  ) :: kctop1 (ncols)  
    INTEGER, INTENT(out  ) :: noshal1(ncols)

    REAL    :: dk(    ncols,kmax-1)
    REAL    :: terp  (ncols,kmax-1)
    REAL    :: f     (ncols,kmax  )
    REAL    :: ff    (ncols,kmax  )
    REAL    :: g     (ncols,kmax  )
    REAL    :: gg    (ncols,kmax  )
    REAL    :: tnew  (ncols,kmax  )
    REAL    :: qnew  (ncols,kmax  )
    REAL    :: a     (ncols,kmax  )
    REAL    :: b     (ncols,kmax  )
    REAL    :: c     (ncols,kmax  )
    REAL    :: ud    (ncols,kmax-1)
    REAL    :: rec   (ncols)
    INTEGER :: icheck(ncols)
    INTEGER :: kctop (ncols)
    INTEGER :: kcbot (ncols)
    INTEGER :: noshal(ncols)
    LOGICAL :: searching(ncols)

    INTEGER ::  n
    INTEGER ::  i
    INTEGER ::  l
    INTEGER ::  k

    REAL          :: dt2
    INTEGER       :: kmaxm
    INTEGER       :: levhm1
    INTEGER       :: loleh1
    INTEGER       :: lolem1
    INTEGER       :: lonlev
    INTEGER       :: lonleh

    dt2 = deltim* 2.0e0
    kmaxm = kmax  -1
    levhm1= kmax -1
    loleh1= ncols *levhm1
    lolem1 =  ncols * kmaxm
    lonlev =  ncols * kmax
    lonleh =  ncols * kmax

    !     
    !     zero  arrays
    !     
    DO i=1,ncols
       noshal(i)=0
       icheck(i)=0
    END DO

    dk=0.0
    DO i=1,ncols
       IF(kuo(i) .EQ. 1) THEN
          noshal(i)=1
       END IF
    END DO
    !     
    !     get cloud base .. overwrite plcl with normalized value
    !     
    DO i=1,ncols
       plcl(i)=plcl(i)/ps(i)
    END DO

    !    DO  l=1, ncols
    !       DO n=2, kmax
    !          kcbot(l)=n-1
    !          IF ( plcl(l) .GE. sl(n) ) THEN
    !             EXIT
    !          END IF
    !       END DO
    !    END DO

    searching = .TRUE.
    kcbot = kmax-1
    DO n=2, kmax
       DO  l=1, ncols
          IF (searching(l) .AND. plcl(l) >= sl(n)) THEN
             searching(l) = .FALSE.
             kcbot(l)=n-1
          END IF
       END DO
    END DO
    IF (newr) THEN
       kcbot1(1:ncols) = kcbot(1:ncols)
    END IF
    !     
    !     set cloud tops
    !     
    DO i=1,ncols
       kctop(i) = MIN(kcr,ktop(i))
       IF(newr)kctop1(i)=kctop(i)
    END DO
    !     
    !     check for too high cloud base
    !     remem. that plcl has been divided by ps
    !     
    DO i=1,ncols
       IF(plcl(i).LT.sl(kbase)) THEN
          noshal(i) = 1
       END IF
    END DO

    DO i=1,ncols
       IF(kcbot(i).GE.kctop(i)) THEN
          noshal(i) = 1
          kcbot (i) = kctop(i)
       END IF
    END DO
    !     
    !     test for moist convective instability
    !     
    DO k=1,kmax-1
       DO i=1,ncols
          a(i,k+1) =  tin(i,k+1) - tin(i,k)
          b(i,k+1) =  qin(i,k+1) - qin(i,k)
          c(i,k+1) =  tin(i,k+1) + tin(i,k)
          a(i,k  ) =  &
               (cp/(sl(k+1)-sl(k)))*a(i,k+1)-&
               (0.5e0*gasr/si(k+1))*c(i,k+1)+&
               (hl/(sl(k+1)-sl(k)))*b(i,k+1)
       END DO
    END DO

    DO k = 1, kmax
       DO i=1, ncols
          IF (k >= kcbot(i) .AND. k <= kctop(i) .AND. a(i,k) > 0.0e0) THEN
             icheck(i)=1
          END IF
       END DO
    END DO

    DO i=1,ncols
       IF(icheck(i).EQ.0) THEN
          noshal(i)=1
       END IF
    END DO
    !     
    !     set mixing coefficient dk (m**2/s) profile
    !     dk(n) is value at top of layer n
    !     n.b.  dk(kctop) .ne. 0
    !
    !DO l=1, ncols
    !   IF (noshal(l) .NE. 1) THEN
    !      dk(l,kctop(l)) = 1.0e0
    !      dk(l,kcbot(l)) = 1.5e0
    !      kbetw= kctop(l)-(kcbot(l)+1)
    !      IF(kbetw .GE. 1) dk(l,kctop(l)-1)=3.0e0
    !      IF(kbetw .GT. 1) THEN
    !         DO k=kcbot(l)+1, kctop(l)-2
    !            dk(l,k)=5.0e0
    !         END DO
    !      END IF
    !   END IF
    !END DO
    DO l=1, ncols
       IF (noshal(l) /= 1) THEN
          dk(l,kctop(l)) = 1.0e0
          dk(l,kcbot(l)) = 1.5e0
       END IF
    END DO
    DO l=1, ncols
       IF (noshal(l) /= 1 .AND. kctop(l) >= kcbot(l)+2) THEN
          dk(l,kctop(l)-1)=3.0e0
       END IF
    END DO
    DO k = 1, kmax
       DO l = 1, ncols
          IF (&
               noshal(l) /= 1         .AND. &
               kctop(l) >  kcbot(l)+2 .AND. &
               k >= kcbot(l)+1        .AND. &
               k <= kctop(l)-2               ) THEN
             dk(l,k)=5.0e0
          END IF
       END DO
    END DO
    !     
    !     compute adiabatic lapse rate terms for temperature eq.
    !     
    DO k=1,kmaxm
       DO i=1,ncols
          terp(i,k) = 1.0e0/(tin(i,k)+tin(i,k+1))
       END DO
    END DO
    !     
    !     n.b. terp(i,n) is valid at top of layer n
    !     
    DO k=1, kmaxm
       DO i=1,ncols
          ff(i,k) = (dt2*delmod(k))*terp(i,k)*dk(i,k)
          gg(i,k) = (dt2*gammod(k))/(dt2*delmod(k))*ff(i,k)
       END DO

    END DO
    !     
    !     compute elements of tridiagonal matrix a,b,c
    !     
    a=0.0 ! call reset(a,lonlev)
    b=0.0 ! call reset(b,lonlev)
    c=0.0 ! call reset(c,lonlev)

    DO i=1,lolem1
       terp(i,1) = terp(i,1)*terp(i,1)*dk(i,1)
    END DO

    DO n=1, kmaxm
       DO i=1,ncols
          c(i,n) = (-dels(n) *dt2)*terp(i,n)
       END DO
    END DO

    DO n=2, kmax
       DO i=1,ncols
          a(i,n) = (-gams(n-1)*dt2) *terp(i,n-1)
       END DO
    END DO

    DO i=1,lonlev
       b(i,1)=  1.0e0-a(i,1)-c(i,1)
    END DO
    !
    !     compute forcing terms f for temperature and
    !     g for water vapor
    !     
    DO i=1,ncols
       f(i,   1)=tin(i,   1)+ff(i,    1)
       f(i,kmax)=tin(i,kmax)-gg(i,kmaxm)
    END DO

    DO n=2, kmaxm
       DO i=1,ncols
          f(i,n)=tin(i,n)+ff(i,n)-gg(i,n-1)
       END DO
    END DO

    DO k=1,kmax
       DO i=1,ncols
          g(i,k)=qin(i,k)
       END DO
    END DO
    !
    !     solution of tridiagonal problems
    !     forward part
    !     
    DO i=1,ncols
       rec (i)=1.0e0/b(i,1)
       ud(i,1)=c(i,1)*rec(i)
       ff(i,1)=f(i,1)*rec(i)
       gg(i,1)=g(i,1)*rec(i)
    END DO

    DO k=2, kmaxm
       DO i=1,ncols
          rec(i)=1.0e0/( b(i,k)-a(i,k)*ud(i,k-1))
          ud(i,k)=c(i,k)*rec(i)
          ff(i,k)=rec(i)*(f(i,k)-a(i,k)*ff(i,k-1))
       END DO

       IF(k .LE. levhm1)THEN
          DO i=1,ncols
             gg(i,k)=rec(i)*(g(i,k)-a(i,k)*gg(i,k-1))
          END DO
       END IF

    END DO
    !     
    !     now determine solutions
    !     
    DO i=1,ncols
       rec(i)=1.0e0/(b(i,kmax)-a(i,kmax)*ud(i,kmaxm))
       tnew(i,kmax)=rec(i)*(f(i,kmax)-a(i,kmax)*ff(i,kmaxm))
    END DO

    DO n= kmaxm ,1,-1
       DO i=1,ncols
          tnew(i,n)=ff(i,n)-tnew(i,n+1)*ud(i,n)
       END DO
    END DO
    !     
    !     water vapor solution
    !     
    DO i=1,ncols
       rec(i)=1.0e0/(b(i,kmax)-a(i,kmax)*ud(i,levhm1))
       qnew(i,kmax)=rec(i)*(g(i,kmax)-a(i,kmax)*gg(i,levhm1))
       IF(newr)noshal1(i)=noshal(i)
    END DO

    DO k=kmax-1,1,-1
       DO i=1,ncols
          qnew(i,k)=gg(i,k)-ud(i,k)*qnew(i,k+1)
       END DO
    END DO
    !     
    !     store solutions
    !     
    DO k=1,kmax
       DO i=1,ncols
          IF(noshal(i).EQ.0)THEN
             tin(i,k)=tnew(i,k)
          END IF
       END DO
    END DO

    DO k=1,kmax
       DO i=1,ncols
          IF(noshal(i).EQ.0)THEN
             qin(i,k)=qnew(i,k)
          END IF
       END DO
    END DO
    !     
    !     end of computation of shallow convection
    !     
999 FORMAT(' PARAMETER SETTING IS WRONG IN SUBR.SHALV2')
  END SUBROUTINE shalv2



  ! qnegat : routine for dealing with negative values of specific humidity
  !          for data on latitude circle.



  SUBROUTINE  qnegat(fq, fdq, fft, rdt, ndnhcr, &
       del, imax, kmax)
    !
    ! input: fq  specific humidity (dimensionless mixing ratio)
    !        fp  surface pressure (cb)
    ! ouput: fq  adjusted specific humidity
    !        fp  unchanged
    !        fdq distribution of moisture modification
    !
    ! imax......Number of grid points on a gaussian latitude circle   
    ! kmax......Number of sigma levels  
    ! imx.......=imax+1 or imax+2   :this dimension instead of imax
    !              is used in order to avoid bank conflict of memory
    !              access in fft computation and make it efficient. the
    !              choice of 1 or 2 depends on the number of banks and
    !              the declared type of grid variable (real*4,real*8)
    !              to be fourier transformed.
    !              cyber machine has the symptom.
    !              cray machine has no bank conflict, but the argument
    !              'imx' in subr. fft991 cannot be replaced by imax    
    ! del.......sigma spacing for each layer computed in routine "setsig".  
    ! dfact.....del(k+1)/del(k)
    !
    INTEGER, INTENT(in   ) :: imax  
    INTEGER, INTENT(in   ) :: kmax
    INTEGER, INTENT(in   ) :: ndnhcr
    REAL   , INTENT(in   ) :: rdt

    REAL,    INTENT(inout) :: fq   (imax,kmax)
    REAL,    INTENT(inout) :: fdq  (imax,kmax)  
    REAL,    INTENT(inout) :: fft  (imax,kmax)   
    REAL,    INTENT(in   ) :: del  (kmax)

    REAL   :: dfact(kmax)

    INTEGER :: klev
    INTEGER :: kblw
    INTEGER :: i
    INTEGER :: k  

    DO k=1,kmax-1
       dfact(k+1) = del(k+1)/del(k)
    END DO
    !     
    !     ecmwf vertical borrowing scheme
    !     fdq contains compensated borrowing above first level, uncompensated
    !     borrowing in first level
    !     
    DO k=1,kmax-1
       klev = kmax-k+1
       kblw = klev - 1
       DO i=1,imax
          fdq(i,klev) = fq(i,klev)
          IF(fq(i,klev).LT.0.0e0) fq(i,klev) = 1.0e-12
          fdq(i,klev) = fq(i,klev) - fdq(i,klev)
          fq(i,kblw) = fq(i,kblw) - fdq(i,klev)*dfact(klev)
       END DO
    END DO

    DO i=1,imax
       fdq(i,1) = fq(i,1)
       IF(fq(i,1).LT.0.0e0) fq(i,1) = 1.0e-12
       fdq(i,1) = fq(i,1) - fdq(i,1)
    END DO

    DO k=1,kmax
       DO i=1,imax
          fft(i,k)=fft(i,k)/(1.0+delq*fq(i,k))
       END DO
    END DO

    IF(dodia(ndnhcr))THEN
       DO k=1,kmax
          DO i=1,imax
             fdq(i,k)=fdq(i,k)*rdt
          END DO
       END DO
    END IF

  END SUBROUTINE qnegat





  SUBROUTINE InitLrgscl
    REAL    :: cpol
    REAL    :: const
    rlocp = hl/cp
    cpol  = cp/hl
    rgrav = 1.0e0/grav
    rlrv  = -hl/(rmwmdi*gasr)
    const = 0.1e0*e0c*EXP(-rlrv/tbase)
    const1    = const*rmwmd
    const2    = const*(rmwmd-1.0e0)
    xx1    = rlrv/cpol
  END SUBROUTINE InitLrgscl





  ! lrgscl :calculates the precipitation resulting from large-scale
  !         processes as well as the adjusted temperature and specific
  !         humidity.



  SUBROUTINE lrgscl(geshem, tf, qs, qf, ps, del, sl, dt, &
       mlrg, latco, ncols, kmax)
    !
    !
    !***********************************************************************
    !
    ! lrgscl is called by the subroutine gwater.
    !
    ! lrgscl calls no subroutines.
    !
    !***********************************************************************
    !
    ! argument(dimensions)                       description
    !
    !        geshem(ncols)             input : accumulated precipitation (m)
    !                                         before adding large-scale
    !                                         precipitation for current
    !                                         time step (gaussian).
    !                                output : accumulated precipitation (m)
    !                                         after adding large-scale
    !                                         precipitation for current
    !                                         time step (gaussian).
    !        tf    (imx,kmax)         input : temperature prediction for the
    !                                         current time step before
    !                                         performing large-scale moist
    !                                         processes (gaussian).
    !                                output : temperature prediction for the
    !                                         current time step after
    !                                         performing large-scale moist
    !                                         processes (gaussian).
    !        qs    (imx,kmax)       output : saturation specific humidity
    !                                         (gaussian).
    !        qf    (imx,kmax)        input : specific humidity for the
    !                                         current time step before
    !                                         performing large-scale moist
    !                                         processes (gaussian).
    !                                output : specific humidity for the
    !                                         current time step after
    !                                         performing large-scale moist
    !                                         processes (gaussian).
    !        ps    (imx)              input : predicted surface pressure (cb
    !                                         (gaussian).
    !        del   (kmax)             input : sigma spacing for each layer
    !                                         computed in routine "setsig".
    !        sl    (kmax)             input : sigma values at center of each
    !                                         layer. computed in routine
    !                                         "setsig".
    !        prec  (imx,2)                  : falling precipitation for
    !                                         this time step (gaussian).
    !        super (imx)                    : quantity of precipitable water
    !                                         water in a layer (gaussian).
    !        dpovg (imx,kmax)              : a temporary storage array used
    !                                         in computing saturation
    !                                         specific humidity. also used
    !                                         in computing the precipitable
    !                                         from the specific humidity
    !                                         (gaussian).
    !        evap  (imx)                    : a temporary storage array for
    !                                         evaporation
    !        amtevp(imx,2)                  : the same as above
    !
    !***********************************************************************
    !
    !  ncols......Number of grid points on a gaussian latitude circle  
    !  kmax......Number of sigma levels  
    !  imx.......ncols+1 or ncols+2   :this dimension instead of ncols
    !              is used in order to avoid bank conflict of memory
    !              access in fft computation and make it efficient. the
    !              choice of 1 or 2 depends on the number of banks and
    !              the declared type of grid variable (real*4,real*8)
    !              to be fourier transformed.
    !              cyber machine has the symptom.
    !              cray machine has no bank conflict, but the argument
    !              'imx' in subr. fft991 cannot be replaced by ncols     
    !  dt........time interval,usually =delt,but changes
    !            in nlnmi (dt=1.) and at dead start(delt/4,delt/2)
    !  mlrg......mlrg=1 ;output of pre-adjusted & post adjusted temp. &
    !            s.h. in lrgscl
    !  latco.....latitude 
    !
    !***  qf=q(n+1),qs=sat. q at t(n+1)=tf. ps=surf. press(cb)
    !***********************************************************************  
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    REAL,    INTENT(inout) :: geshem(ncols)
    REAL,    INTENT(inout) :: tf    (ncols,kmax) 
    REAL,    INTENT(inout) :: qf    (ncols,kmax)
    REAL,    INTENT(inout) :: qs    (ncols,kmax)
    REAL,    INTENT(in   ) :: ps    (ncols)
    REAL,    INTENT(in   ) :: del   (kmax)
    REAL,    INTENT(in   ) :: sl    (kmax)
    REAL,    INTENT(in   ) :: dt
    INTEGER, INTENT(in   ) :: mlrg
    INTEGER, INTENT(in   ) :: latco
    !
    REAL    :: prec  (ncols,2)
    REAL    :: dpovg (ncols,kmax)
    REAL    :: super (ncols)
    REAL    :: fact  (ncols)
    REAL    :: evap  (ncols)
    REAL    :: amtevp(ncols,2)
    REAL    :: q     (ncols,kmax)
    REAL    :: t     (ncols,kmax)
    REAL    :: dtsqrt
    REAL    :: rhsat
    REAL    :: qcond
    REAL    :: qevap
    INTEGER :: k
    INTEGER :: i
    INTEGER :: l
    !

    dtsqrt=SQRT(dt)

    DO k=1,kmax
       DO i=1,ncols
          q (i,k)   = qf(i,k)
          t (i,k)   = tf(i,k)
          qs(i,k)   = EXP(rlrv/tf(i,k))
          qs(i,k)   = const1*qs(i,k)/(sl(k)*ps(i) +const2*qs(i,k))
          dpovg(i,k)= (del(k)*rgrav)*ps(i)
       END DO
    END DO

    prec=0.0
    !
    !     pcpn process.....top lyr downward
    !
    rhsat=0.80e0

    DO k=1, kmax
       l= kmax +1-k
       IF(l.EQ.1)rhsat=0.90e0
       DO i=1, ncols
          super(i)=qf(i,l)-qs(i,l)
          !
          !     compute wet-bulb adjustment to t and q, and augment
          !     precipitation falling through column.
          !
          qcond=MAX(0.0e0,super(i))/(1.0e0-xx1*qs(i,l)/(tf(i,l)*tf(i,l)))
          qcond=MAX(0.0e0,qcond)
          tf(i,l)=tf(i,l)+rlocp*qcond
          qf(i,l)=qf(i,l)-qcond
          prec(i,1)=prec(i,1)+qcond*dpovg(i,l)
       END DO
       !
       !     finished with super-saturated point
       !
       evap=0.0
       DO i=1,ncols
          IF(super(i).LE.0.0e0.AND.prec(i,1).GT.0.0e0) THEN
             evap(i)=rhsat*qs(i,l)-qf(i,l)
          END IF
          fact(i)=0.32e0*dtsqrt*SQRT(prec(i,1))
       END DO

       amtevp =0.0

       DO i=1,ncols
          IF(evap(i).GT.0.0e0) THEN
             amtevp(i,2)=fact(i)*evap(i)
             prec  (i,2)=prec(i,1)/dpovg(i,l)
          END IF
          amtevp(i,1)=MIN(amtevp(i,2),prec(i,2))
          !
          !     monitor
          !
          qevap     = MAX(0.0e0,amtevp(i,1))/ &
               (1.0e0-xx1*qs(i,l)/(tf(i,l)*tf(i,l)))
          tf(i,l)   = tf(i,l)-rlocp*qevap
          qf(i,l)   = qf(i,l)+qevap
          prec(i,1) = prec(i,1)-qevap*dpovg(i,l)
          prec(i,1) = MAX(0.0e0,prec(i,1))
       END DO

    END DO
    !
    !     pcpn reaches ground level....factor of .5 since pcpn is for
    !     two (leapfrog) time-steps....accum pcpn (geshem) is not leapfrogg
    IF(mlrg.EQ.1) THEN
       !
       !     monitor
       !
       DO i=1,ncols
          IF(prec(i,1).GT.0.0e0) THEN
             WRITE(nfprt,999)i,latco,(t (i,k),k=1,kmax)
             WRITE(nfprt,888)        (tf(i,k),k=1,kmax)
             WRITE(nfprt,777)        (q (i,k),k=1,kmax)
             WRITE(nfprt,666)        (qf(i,k),k=1,kmax)
          END IF
       END DO

    END IF

    DO i=1,ncols
       geshem(i)=geshem(i)+prec(i,1)*0.5e0
    END DO
666 FORMAT(' QLN '         ,8X,10E12.5)
777 FORMAT(' QLO '         ,8X,10E12.5)
888 FORMAT(' TLN '         ,8X,10E12.5)
999 FORMAT(' TLO ',I3,1X,I3,1X,10E12.5)
  END SUBROUTINE lrgscl



  ! qstar9:  vector computation of saturation mixing ratio



  SUBROUTINE qstar9 (t     ,p     ,n     ,qstt  ,layr  ,kmax  )
    !
    !
    ! function vqsat
    !     purpose
    !     vector computation of saturation mixing ratio
    !     usage
    !     qsatt(1;n) = vqsat(t(1;n),p(1;n),n;qsatt(1;n))
    !     description of parameters
    !     t        temperature vector (deg k)
    !     p        pressure vector (mb)
    !     vqsat    saturation mixing ratio vector
    !     kmax.....Number of sigma levels  
    !     n........Number of grid points on a gaussian latitude circle
    !     qstt.....saturation mixing ratio
    !     layr.....=2,3,4,5,...,kmax
    INTEGER, INTENT(in   ) :: kmax  
    INTEGER, INTENT(in   ) :: n
    REAL   , INTENT(in   ) :: t(n)
    REAL   , INTENT(in   ) :: p(n)
    REAL   , INTENT(inout) :: qstt(n)
    INTEGER, INTENT(in   ) :: layr

    REAL                   :: e1(n)
    REAL                   :: e2(n)
    REAL                   :: tq(n)
    INTEGER                :: i1(n)
    INTEGER                :: i2(n)
    REAL   , PARAMETER     :: zp622  = 0.622  
    REAL   , PARAMETER     :: z1p0s1 = 1.00001 
    REAL   , PARAMETER     :: z1p622 = 1.622  
    REAL   , PARAMETER     :: z138p9 = 138.90001 
    REAL   , PARAMETER     :: z198p9 = 198.99999 
    REAL   , PARAMETER     :: z200   = 200.0    
    REAL   , PARAMETER     :: z337p9 = 337.9    
    REAL   , PARAMETER     :: est(139) = (/ &
         0.31195e-02, 0.36135e-02, 0.41800e-02, &
         0.48227e-02, 0.55571e-02, 0.63934e-02, 0.73433e-02, &
         0.84286e-02, 0.96407e-02, 0.11014e-01, 0.12582e-01, &
         0.14353e-01, 0.16341e-01, 0.18574e-01, 0.21095e-01, &
         0.23926e-01, 0.27096e-01, 0.30652e-01, 0.34629e-01, &
         0.39073e-01, 0.44028e-01, 0.49546e-01, 0.55691e-01, &
         0.62508e-01, 0.70077e-01, 0.78700e-01, 0.88128e-01, &
         0.98477e-01, 0.10983e+00, 0.12233e+00, 0.13608e+00, &
         0.15121e+00, 0.16784e+00, 0.18615e+00, 0.20627e+00, &
         0.22837e+00, 0.25263e+00, 0.27923e+00, 0.30838e+00, &
         0.34030e+00, 0.37520e+00, 0.41334e+00, 0.45497e+00, &
         0.50037e+00, 0.54984e+00, 0.60369e+00, 0.66225e+00, &
         0.72589e+00, 0.79497e+00, 0.86991e+00, 0.95113e+00, &
         0.10391e+01, 0.11343e+01, 0.12372e+01, 0.13484e+01, &
         0.14684e+01, 0.15979e+01, 0.17375e+01, 0.18879e+01, &
         0.20499e+01, 0.22241e+01, 0.24113e+01, 0.26126e+01, &
         0.28286e+01, 0.30604e+01, 0.33091e+01, 0.35755e+01, &
         0.38608e+01, 0.41663e+01, 0.44930e+01, 0.48423e+01, &
         0.52155e+01, 0.56140e+01, 0.60394e+01, 0.64930e+01, &
         0.69767e+01, 0.74919e+01, 0.80406e+01, 0.86246e+01, &
         0.92457e+01, 0.99061e+01, 0.10608e+02, 0.11353e+02, &
         0.12144e+02, 0.12983e+02, 0.13873e+02, 0.14816e+02, &
         0.15815e+02, 0.16872e+02, 0.17992e+02, 0.19176e+02, &
         0.20428e+02, 0.21750e+02, 0.23148e+02, 0.24623e+02, &
         0.26180e+02, 0.27822e+02, 0.29553e+02, 0.31378e+02, &
         0.33300e+02, 0.35324e+02, 0.37454e+02, 0.39696e+02, &
         0.42053e+02, 0.44531e+02, 0.47134e+02, 0.49869e+02, &
         0.52741e+02, 0.55754e+02, 0.58916e+02, 0.62232e+02, &
         0.65708e+02, 0.69351e+02, 0.73168e+02, 0.77164e+02, &
         0.81348e+02, 0.85725e+02, 0.90305e+02, 0.95094e+02, &
         0.10010e+03, 0.10533e+03, 0.11080e+03, 0.11650e+03, &
         0.12246e+03, 0.12868e+03, 0.13517e+03, 0.14193e+03, &
         0.14899e+03, 0.15634e+03, 0.16400e+03, 0.17199e+03, &
         0.18030e+03, 0.18895e+03, 0.19796e+03, 0.20733e+03, &
         0.21708e+03, 0.22722e+03, 0.23776e+03, 0.24871e+03/)
    REAL                   :: qfac(kmax)
    INTEGER                :: i
    REAL                   :: a1622

    a1622   = 1.0e0  / z1p622
    qfac    = 1.0e0

    DO i = 1,n
       tq(i) = t(i) - z198p9
       IF(t(i).LT.z200   ) tq(i) = z1p0s1
       IF(t(i).GT.z337p9 ) tq(i) = z138p9
       i1(i) = tq(i)
       i2(i) = i1(i) + 1
    END DO

    DO i = 1,n
       e1(i) =  est(  i1(i) )
       e2(i) =  est(  i2(i) )
    END DO

    DO i = 1,n
       qstt(i)   = tq(i) - i1(i)
       qstt(i)   = qstt(i) * ( e2(i)-e1(i) )
       qstt(i)   = qstt(i) +   e1(i)
       e1(i) = p(i) * a1622
       IF( e1(i).LT.qstt(i) ) qstt(i) = e1(i)
       qstt(i)   = zp622 *  qstt(i) / p(i)
       qstt(i) = qstt(i)*qfac(layr)
    END DO

  END SUBROUTINE qstar9



  ! rnevp  :relaxed arakawa-schubert



  SUBROUTINE rnevp(ft    ,fq    ,pain  ,fp    ,sig   ,dsig  ,clfric, &
       rcon  ,rlar  ,dtc3  ,ncols ,kmax  ,nlst)
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    !
    !     default parameter statements for dimensioning resolution
    !     
    INTEGER, INTENT(in   ) :: nlst
    REAL,    INTENT(in   ) :: dtc3
    !     
    !     input model fields
    !
    REAL,    INTENT(in   ) :: fp  (ncols) 
    REAL,    INTENT(inout) :: fq  (ncols,kmax) 
    REAL,    INTENT(inout) :: ft  (ncols,kmax)
    REAL,    INTENT(in   ) :: dsig(kmax) 
    REAL,    INTENT(in   ) :: sig (kmax)
    !     
    !     integers and logicals are listed first, then half precisions
    !
    REAL,    INTENT(in   ) :: clfric
    REAL,    INTENT(in   ) :: pain (ncols,kmax ) 
    !
    !     this is need to use the dao type re-evaporation
    ! 
    REAL,    INTENT(inout) :: rcon(ncols)
    REAL,    INTENT(out  ) :: rlar(ncols)   
    !     
    !     integers and logicals are listed first, then half precisions
    !
    LOGICAL :: detprc
    LOGICAL :: timeit
    REAL    :: shsat    (ncols,kmax )
    !
    !     this is need to use the dao type re-evaporation
    !
    REAL    :: temp2 (ncols,kmax)
    REAL    :: tl    (ncols,kmax)
    REAL    :: ql    (ncols,kmax)
    REAL    :: rain  (ncols,kmax)
    REAL    :: pl    (ncols,kmax)
    REAL    :: amass (ncols,kmax)
    REAL    :: clfrac(ncols,kmax)
    REAL    :: tempc (ncols,kmax)
    REAL    :: cvt   (ncols,kmax)
    REAL    :: cvq   (ncols,kmax)
    REAL    :: evp9  (ncols,kmax)
    REAL    :: plk   (ncols,kmax)
    REAL    :: art   (ncols)
    INTEGER :: im 
    INTEGER :: nlay  
    INTEGER :: nlayp1
    INTEGER :: nlaym1
    INTEGER :: nlaym2
    INTEGER :: ls 
    INTEGER :: ls1 
    REAL    :: grav  
    REAL    :: grav2 
    REAL    :: pi 
    REAL    :: pi2 
    REAL    :: gamfac
    REAL    :: cp 
    REAL    :: velta 
    REAL    :: ptop  
    REAL    :: hltm  
    REAL    :: pcon  
    REAL    :: critl 
    REAL    :: flim  
    REAL    :: dhtop 
    INTEGER :: l 
    INTEGER :: i 
    REAL    :: rphf  
    REAL    :: elocp 
    INTEGER :: n 
    REAL    :: relax 
    REAL    :: rnfrac
    REAL    :: rpow  
    REAL    :: exparg

    !
    !     to save storage space use the temp array from comp3 for local store
    !                                                                     
    !     temp level 1 has accumulated evaporation of convective rain
    !     temp level 2 has saturation mixing ratio                   
    !     temp level 3 has local temperature                         
    !     temp level 4 has local mixing ratio                        
    !     temp level 5 has change in mixing ratio (at each iteration)
    !     temp level 6 has dqdt, one of the terms for the iteration  
    !     temp level 7 is  a temporary for actual evaporation of rain
    !     
    !     
    !     flip arrays and transfer to local variables
    !     
    im=ncols
    nlay=kmax
    nlayp1=nlay+1
    nlaym1=nlay-1
    nlaym2=nlay-2
    ls=nlst
    ls1=nlst+1
    detprc=.TRUE.
    timeit=.TRUE.
    grav=9.81
    grav2=0.01*grav
    pi=4.0*ATAN(1.0)
    pi2=2.*pi
    gamfac=1.348e7
    cp=1003.
    velta=.608
    ptop=0.01
    hltm=2.56e6
    pcon=2.e-3
    critl=0.
    flim=765.
    dhtop=0.95
    DO l = 1,kmax
       DO i = 1,ncols
          tl(i,l) = ft(i,kmax -l+1)
          ql(i,l) = fq(i,kmax-l+1)
          pl(i,l) = sig(kmax-l+1)*fp(i)*10.
          plk(i,l) = EXP(xkapa* LOG(pl(i,l)))
          amass(i,l) = 0.01*grav/(10.*fp(i)*dsig(kmax-l+1))
          temp2(i,l)= pl(i,l) *.001
          temp2(i,l)=SQRT(temp2(i,l))
       END DO
    END DO
    DO l=1,kmax
       DO i=1,ncols
          rain(i,l)=pain(i,l)
       END DO
    END DO
    rphf=3600.0/dtc3
    elocp=hltm/cp
    DO i = 1,im
       rcon(i) = 0.0e0
       rlar(i) = 0.0e0
    END DO
    DO i = 1,im*nlay
       evp9(i,1)  = 0.0e0
       tempc(i,1) = 0.0e0
       cvt(i,1)   = 0.0e0
       cvq(i,1)   = 0.0e0
    END DO
    ! 
    !     do loop for moisture evaporation ability and convec evaporation
    !
    DO l = 2,nlay
       DO i = 1,im
          tempc(i,3) = tl(i,l)
          tempc(i,4) = ql(i,l)
       END DO
       DO n = 1,3
          CALL qstar9(tempc(1,3),pl(1,l),im   ,tempc(1,2),l    ,kmax  )
          IF(n .EQ. 1) THEN
             relax = 0.5
          ELSE
             relax = 1.0
          END IF
          DO i = 1,im
             tempc(i,5) = tempc(i,2) - tempc(i,4)
             tempc(i,6) = tempc(i,2)*gamfac/tempc(i,3)**2
             tempc(i,5) = tempc(i,5)/(1.0e0+tempc(i,6))
             tempc(i,4) = tempc(i,4) + tempc(i,5)*relax
             tempc(i,3) = tempc(i,3) - tempc(i,5)*elocp*relax
          END DO
       END DO
       DO i = 1,im
          shsat(i,l) = tempc(i,4)
          evp9(i,l) = (tempc(i,4) - ql(i,l))/amass(i,l)
          rcon(i) = rcon(i) + rain(i,l)
          clfrac(i,l) = (1.-(ql(i,l)/tempc(i,4)))*clfric
          tempc(i,8) = MAX(tempc(i,8),clfrac(i,l))
          art(i) = 0.0e0
          IF(rcon(i).GT.0.0e0 .AND. evp9(i,l).GT.0.0e0) THEN
             rnfrac = tempc(i,8)
             rnfrac = MIN(rnfrac,1.0e0)
             rpow = EXP(.578* LOG(rcon(i)*rphf*temp2(i,l)))
             exparg = -1.04e-4*dtc3*rpow
             art(i) = 1.0 - (EXP(exparg))
             tempc(i,7) = evp9(i,l)*art(i)*rnfrac
             IF(tempc(i,7) .GE. rcon(i)) tempc(i,7) = rcon(i)
             rcon(i)   = rcon(i) - tempc(i,7)
             evp9(i,l) = evp9(i,l) - tempc(i,7)
             cvq(i,l) = cvq(i,l) + tempc(i,7)*amass(i,l)
             cvt(i,l) = cvt(i,l) - tempc(i,7)*amass(i,l)*elocp
             !
             !     update as rain falls and reevaporates
             !
             tl(i,l) = tl(i,l) + cvt(i,l)
             ql(i,l) = ql(i,l) + cvq(i,l)
          ELSE
             tempc(i,7) = 0.0e0
          ENDIF
       END DO
    END DO
    !
    !     flip arrays for return (only flip t and q)
    !
    DO l = 1,nlay
       DO i = 1,im
          ft(i,l) = tl(i,kmax -l+1)
          fq(i,l) = ql(i,kmax-l+1)
       END DO
    END DO
  END SUBROUTINE rnevp



  ! cloud  :relaxed arakawa-schubert.



  SUBROUTINE cloud(  &
       len   ,lenc  ,k     ,ic    ,rasalf,setras,frac  ,alhl  ,rkap  , &
       poi   ,qoi   ,uoi   ,voi   ,prs   ,prj   ,pcu   ,cln   ,tcu   , &
       qcu   ,alf   ,bet   ,gam   ,prh   ,pri   ,hol   ,eta   ,hst   , &
       qol   ,gmh   ,tx1   ,tx2   ,tx3   ,tx4   ,tx5   ,tx6   ,tx7   , &
       tx8   ,alm   ,wfn   ,akm   ,qs1   ,clf   ,uht   ,vht   ,wlq   , &
       ia    ,i1    ,i2    ,cmb2pa,rhmax )
    ! cmb2pa....Parameter cmb2pa = 100.0
    ! rhmax.....Parameter rhmax  = 0.9999    
    ! ncols......Number of grid points on a gaussian latitude circle    
    ! actp......Parameter actp   = 1.7   
    ! facm......Parameter facm   = 1.0      
    ! p5........Parameter p5     = 500.0  
    ! p8........Parameter p8     = 800.0  
    ! pt8.......Parameter pt8    = 0.8 
    ! pt2.......Parameter pt2    = 0.2 
    ! pfac......Parameter pfac   = pt2/(p8-p5)    
    ! cucld.....Parameter cucld  = 0.5   
    ! len.......ncols
    ! lenc......ncols
    ! k.........kmax
    ! ic
    ! rasalf
    ! setras
    ! frac......frac=1. 
    ! alhl......alhl=2.52e6
    ! rkap......rkap=r/cp=287.05e0/1004.6e0
    ! poi.......array pot. temp. 
    ! qoi.......array humidity.
    ! prs
    ! uoi
    ! voi
    ! prj
    ! tcu
    ! qcu
    ! cln
    ! alf
    ! bet
    ! gam
    ! prh
    ! pri
    ! akm
    ! wfn
    ! hol
    ! qol
    ! eta
    ! hst
    ! gmh
    ! alm
    ! wlq
    ! qs1
    ! tx1
    ! tx2
    ! tx3
    ! tx4
    ! tx5
    ! tx6
    ! tx7
    ! tx8
    ! uht
    ! vht
    ! clf
    ! pcu
    ! ia
    ! i1
    ! i2
    !
    !==========================================================================
    REAL,    INTENT(in   ) :: cmb2pa
    REAL,    INTENT(in   ) :: rhmax  

    INTEGER, INTENT(in   ) :: len
    INTEGER, INTENT(in   ) :: lenc
    INTEGER, INTENT(in   ) :: k
    INTEGER, INTENT(in   ) :: ic
    REAL,    INTENT(in   ) :: rasalf
    LOGICAL, INTENT(in   ) :: setras
    REAL,    INTENT(in   ) :: frac 
    REAL,    INTENT(in   ) :: alhl
    REAL,    INTENT(in   ) :: rkap

    REAL,    INTENT(in   ) :: poi(len,k)
    REAL,    INTENT(in   ) :: qoi(len,k) 
    REAL,    INTENT(in   ) :: prs(len,k+1)
    REAL,    INTENT(in   ) :: uoi(len,k)
    REAL,    INTENT(in   ) :: voi(len,k)
    REAL,    INTENT(in   ) :: prj(len,k+1)
    REAL,    INTENT(out  ) :: tcu(len,k) 
    REAL,    INTENT(out  ) :: qcu(len,k)
    REAL,    INTENT(out  ) :: cln(len)

    REAL,    INTENT(inout) :: alf(len,k)
    REAL,    INTENT(inout) :: bet(len,k) 
    REAL,    INTENT(inout) :: gam(len,k)
    REAL,    INTENT(inout) :: prh(len,k)
    REAL,    INTENT(inout) :: pri(len,k)
    REAL,    INTENT(inout) :: akm(lenc)  
    REAL,    INTENT(inout) :: wfn(lenc)

    REAL,    INTENT(inout) :: hol(lenc,k) 
    REAL,    INTENT(inout) :: qol(lenc,k)  
    REAL,    INTENT(inout) :: eta(lenc,k) 
    REAL,    INTENT(inout) :: hst(lenc,k)
    REAL,    INTENT(inout) :: gmh(lenc,k) 
    REAL,    INTENT(inout) :: alm(lenc) 
    REAL,    INTENT(inout) :: wlq(lenc)   
    REAL,    INTENT(inout) :: qs1(lenc)
    REAL,    INTENT(inout) :: tx1(lenc)   
    REAL,    INTENT(inout) :: tx2(lenc) 
    REAL,    INTENT(inout) :: tx3(lenc)   
    REAL,    INTENT(inout) :: tx4(lenc)
    REAL,    INTENT(inout) :: tx5(lenc)   
    REAL,    INTENT(inout) :: tx6(lenc) 
    REAL,    INTENT(inout) :: tx7(lenc)   
    REAL,    INTENT(inout) :: tx8(lenc)
    REAL,    INTENT(inout) :: uht(lenc)   
    REAL,    INTENT(inout) :: vht(lenc) 
    REAL,    INTENT(out  ) :: clf(lenc)   
    REAL,    INTENT(inout) :: pcu(lenc)

    INTEGER, INTENT(inout) :: ia(lenc)
    INTEGER, INTENT(inout) :: i1(lenc)
    INTEGER, INTENT(inout) :: i2(lenc)

    REAL    :: rkapp1
    REAL    :: albcp
    REAL    :: onebcp
    REAL    :: onebg
    REAL    :: cpbg
    REAL    :: twobal
    REAL    :: etamn
    INTEGER :: km1 
    INTEGER :: ic1 
    INTEGER :: l   
    INTEGER :: i   
    REAL    :: tem1 
    INTEGER :: len1 
    INTEGER :: len2 
    INTEGER :: isav 
    INTEGER :: len11 
    INTEGER :: ii    
    REAL    :: tem   
    INTEGER :: lena  
    INTEGER :: lenb  
    INTEGER :: lena1 
    INTEGER :: ksl     

    rkapp1 = 1.0  + rkap
    onebcp = 1.0  / cp
    albcp  = alhl * onebcp
    onebg  = 1.0  / grav
    cpbg   = cp   * onebg
    twobal = 2.0 / alhl
    km1 = k  - 1
    ic1 = ic + 1
    !     
    !     settiing alf, bet, gam, prh, and pri : done only when setras=.t.
    !     
    IF (setras) THEN
       DO l=1,k
          DO i=1,lenc
             prh(i,l) = (prj(i,l+1)*prs(i,l+1) - prj(i,l)*prs(i,l)) &
                  / ((prs(i,l+1)-prs(i,l)) * rkapp1)
          END DO
       END DO

       DO l=1,k
          DO i=1,lenc
             tx5(i) = poi(i,l) * prh(i,l)
             tx1(i) = (prs(i,l) + prs(i,l+1)) * 0.5
             tx3(i) = tx5(i)
             CALL qsat(tx3(i), tx1(i), tx2(i), tx4(i), .TRUE.)
             alf(i,l) = tx2(i) - tx4(i) * tx5(i)
             bet(i,l) = tx4(i) * prh(i,l)
             gam(i,l) = 1.0 / ((1.0 + tx4(i)*albcp) * prh(i,l))
             pri(i,l) = (cp/cmb2pa) / (prs(i,l+1) - prs(i,l))
          END DO
       END DO
    ENDIF

    DO l=1,k
       DO i=1,len
          tcu(i,l) = 0.0
          qcu(i,l) = 0.0
       END DO
    END DO

    DO i=1,lenc
       tx1(i)   = prj(i,k+1) * poi(i,k)
       qs1(i)   = alf(i,k) + bet(i,k)*poi(i,k)
       qol(i,k) = amin1(qs1(i)*rhmax,qoi(i,k))
       hol(i,k) = tx1(i)*cp + qol(i,k)*alhl
       eta(i,k) = 0.0e0
       tx2(i)   = (prj(i,k+1) - prj(i,k)) * poi(i,k) * cp
    END DO

    IF (ic .LT. km1) THEN
       DO l=km1,ic1,-1
          DO i=1,lenc
             qs1(i)   = alf(i,l) + bet(i,l)*poi(i,l)
             qol(i,l) = amin1(qs1(i)*rhmax,qoi(i,l))
             tem1     = tx2(i) + prj(i,l+1) * poi(i,l) * cp 
             hol(i,l) = tem1 + qol(i,l )* alhl
             hst(i,l) = tem1 + qs1(i)   * alhl
             tx1(i)   = (prj(i,l+1) - prj(i,l)) * poi(i,l)
             eta(i,l) = eta(i,l+1) + tx1(i)*cpbg
             tx2(i)   = tx2(i)     + tx1(i)*cp
          END DO
       END DO
    ENDIF

    DO i=1,lenc
       hol(i,ic) = tx2(i)
       qs1(i)    = alf(i,ic) + bet(i,ic)*poi(i,ic)
       qol(i,ic) = amin1(qs1(i)*rhmax,qoi(i,ic)) 
       tem1      = tx2(i) + prj(i,ic1) * poi(i,ic) * cp 
       hol(i,ic) = tem1 + qol(i,ic) * alhl
       hst(i,ic) = tem1 + qs1(i)    * alhl
       tx3(i   ) = (prj(i,ic1) - prh(i,ic)) * poi(i,ic)
       eta(i,ic) = eta(i,ic1) + cpbg * tx3(i)
    END DO

    DO i=1,lenc
       tx2(i) = hol(i,k)  - hst(i,ic)
       tx1(i) = 0.0e0
    END DO
    !     
    !     entrainment parameter
    !     
    DO l=ic,km1
       DO i=1,lenc
          tx1(i) = tx1(i) + (hst(i,ic) - hol(i,l)) *  &
               (eta(i,l) - eta(i,l+1))
       END DO
    END DO

    len1 = 0
    len2 = 0
    isav = 0

    DO i=1,lenc
       IF (tx1(i) .GT. 0.0e0 .AND. tx2(i) .GT. 0.0e0) THEN
          len1      = len1 + 1
          ia(len1)  = i
          alm(len1) = tx2(i) / tx1(i)
       ENDIF
    END DO

    len2 = len1

    IF (ic1 .LT. k) THEN
       DO i=1,lenc
          IF (tx2(i) .LE. 0.0 .AND. (hol(i,k) .GT. hst(i,ic1))) THEN
             len2      = len2 + 1
             ia(len2)  = i
             alm(len2) = 0.0
          ENDIF
       END DO
    ENDIF

    IF (len2 .EQ. 0) THEN 
       DO i=1,lenc*k
          hst(i,1) = 0.0
          qol(i,1) = 0.0
       END DO
       DO i=1,lenc
          pcu(i) = 0.0
       END DO
       RETURN
    ENDIF
    len11 = len1 + 1
    !     
    !     normalized massflux
    !     
    DO i=1,len2
       eta(i,k) = 1.0
       ii       = ia(i)
       tx2(i)   = 0.5 * (prs(ii,ic) + prs(ii,ic1))
       tx4(i)   = prs(ii,k)
    END DO

    DO i=len11,len2
       wfn(i)   = 0.0
       ii       = ia(i)
       IF (hst(ii,ic1) .LT. hst(ii,ic)) THEN
          tx6(i) = (hst(ii,ic1)-hol(ii,k))/(hst(ii,ic1)-hst(ii,ic))
       ELSE
          tx6(i) = 0.0
       ENDIF
       tx2(i) = 0.5 * (prs(ii,ic1)+prs(ii,ic1+1)) * (1.0-tx6(i)) &
            + tx2(i)      * tx6(i)
    END DO

    CALL acritn(len2, tx2, tx4, tx3)

    DO l=km1,ic,-1
       DO i=1,len2
          tx1(i) = eta(ia(i),l)
       END DO

       DO i=1,len2
          eta(i,l) = 1.0 + alm(i) * tx1(i)
       END DO
    END DO
    !     
    !     cloud workfunction
    !     
    IF (len1 .GT. 0) THEN
       DO i=1,len1
          ii = ia(i)
          wfn(i) = - gam(ii,ic) * (prj(ii,ic1) - prh(ii,ic)) &
               *  hst(ii,ic) * eta(i,ic1)
       END DO
    ENDIF

    DO i=1,len2
       ii = ia(i)
       tx1(i) = hol(ii,k)
    END DO

    IF (ic1 .LE. km1) THEN
       DO l=km1,ic1,-1
          DO i=1,len2
             ii = ia(i)
             tem = tx1(i) + (eta(i,l) - eta(i,l+1)) * hol(ii,l)
             pcu(i) = prj(ii,l+1) - prh(ii,l)
             tem1   = eta(i,l+1) * pcu(i)
             tx1(i) = tx1(i)*pcu(i)
             pcu(i) = prh(ii,l) - prj(ii,l)
             tem1   = (tem1 + eta(i,l) * pcu(i)) * hst(ii,l)
             tx1(i) = tx1(i) + tem*pcu(i)
             wfn(i) = wfn(i) + (tx1(i) - tem1) * gam(ii,l)
             tx1(i) = tem
          END DO
       END DO
    ENDIF
    lena = 0
    IF (len1 .GT. 0) THEN
       !cdir nodep
       DO i=1,len1
          ii = ia(i)
          wfn(i) = wfn(i) + tx1(i) * gam(ii,ic) *  &
               (prj(ii,ic1)-prh(ii,ic)) - tx3(i)
          IF (wfn(i) .GT. 0.0) THEN
             lena = lena + 1
             i1(lena) = ia(i)
             i2(lena) = i
             tx1(lena) = wfn(i)
             tx2(lena) = qs1(ia(i))
             tx6(lena) = 1.0
          ENDIF
       END DO
    ENDIF
    lenb = lena
    DO i=len11,len2
       wfn(i) = wfn(i) - tx3(i)
       IF (wfn(i) .GT. 0.0 .AND. tx6(i) .GT. 0.0) THEN
          lenb = lenb + 1
          i1(lenb)  = ia(i)
          i2(lenb)  = i
          tx1(lenb) = wfn(i)
          tx2(lenb) = qs1(ia(i))
          tx4(lenb) = tx6(i)
       ENDIF
    END DO
    IF (lenb .LE. 0) THEN
       DO i=1,lenc*k
          hst(i,1) = 0.0
          qol(i,1) = 0.0
       END DO
       DO i=1,lenc
          pcu(i) = 0.0
       END DO
       RETURN
    ENDIF
    DO i=1,lenb
       wfn(i) = tx1(i)
       qs1(i) = tx2(i)
    END DO
    DO l=ic,k
       DO i=1,lenb
          tx1(i) = eta(i2(i),l)
       END DO
       DO i=1,lenb
          eta(i,l) = tx1(i)
       END DO
    END DO
    lena1 = lena + 1
    DO i=1,lena
       ii = i1(i)
       tx8(i) = hst(ii,ic) - hol(ii,ic)
    END DO
    DO i=lena1,lenb
       ii = i1(i)
       tx6(i) = tx4(i)
       tem    = tx6(i) * (hol(ii,ic)-hol(ii,ic1)) + hol(ii,ic1)
       tx8(i) = hol(ii,k) - tem
       tem1   = tx6(i) * (qol(ii,ic)-qol(ii,ic1)) + qol(ii,ic1)
       tx5(i) = tem    - tem1 * alhl
       qs1(i) = tem1   + tx8(i)*(1.0e0/alhl)
       tx3(i) = hol(ii,ic)
    END DO
    DO i=1,lenb
       ii = i1(i)
       wlq(i) = qol(ii,k) - qs1(i)     * eta(i,ic)
       uht(i) = uoi(ii,k) - uoi(ii,ic) * eta(i,ic)
       vht(i) = voi(ii,k) - voi(ii,ic) * eta(i,ic)
       tx7(i) = hol(ii,k)
    END DO
    DO l=km1,ic,-1
       DO i=1,lenb
          ii = i1(i)
          tem    = eta(i,l) - eta(i,l+1)
          wlq(i) = wlq(i) + tem * qol(ii,l)
          uht(i) = uht(i) + tem * uoi(ii,l)
          vht(i) = vht(i) + tem * voi(ii,l)
       END DO
    END DO
    !     
    !     calculate gs and part of akm (that requires eta)
    !     
    DO i=1,lenb
       ii = i1(i)
       tem        = (poi(ii,km1) - poi(ii,k)) /  &
            (prh(ii,k) - prh(ii,km1))
       hol(i,k)   = tem *(prj(ii,k)-prh(ii,km1))*prh(ii,k)*pri(ii,k)
       hol(i,km1) = tem *(prh(ii,k)-prj(ii,k))*prh(ii,km1)*pri(ii,km1)
       akm(i)     = 0.0e0
       tx2(i)     = 0.5 * (prs(ii,ic) + prs(ii,ic1))
    END DO

    IF (ic1 .LE. km1) THEN
       DO l=km1,ic1,-1
          DO i=1,lenb
             ii = i1(i)
             tem      = (poi(ii,l-1) - poi(ii,l)) * eta(i,l) &
                  / (prh(ii,l) - prh(ii,l-1))
             hol(i,l)   = tem * (prj(ii,l)-prh(ii,l-1)) * prh(ii,l) &
                  *  pri(ii,l)  + hol(i,l)
             hol(i,l-1) = tem * (prh(ii,l)-prj(ii,l)) * prh(ii,l-1) &
                  * pri(ii,l-1)
             akm(i)   = akm(i) - hol(i,l) &
                  * (eta(i,l)   * (prh(ii,l)-prj(ii,l)) + &
                  eta(i,l+1) * (prj(ii,l+1)-prh(ii,l))) / prh(ii,l)
          END DO
       END DO
    ENDIF

    CALL rncl(lenb, tx2, tx1, clf)

    DO i=1,lenb
       tx2(i) = (1.0e0 - tx1(i)) * wlq(i)
       wlq(i) = tx1(i) * wlq(i)
       tx1(i) = hol(i,ic)
    END DO

    DO i=lena1, lenb
       ii = i1(i)
       tx1(i) = tx1(i) + (tx5(i)-tx3(i)+qol(ii,ic)*alhl)* &
            (pri(ii,ic)/cp)
    END DO

    DO i=1,lenb
       hol(i,ic) = tx1(i) - tx2(i) * albcp * pri(i1(i),ic)
    END DO

    IF (lena .GT. 0) THEN
       DO i=1,lena
          ii = i1(i)
          akm(i) = akm(i) - eta(i,ic1) * (prj(ii,ic1) - prh(ii,ic))  &
               * tx1(i) / prh(ii,ic)
       END DO
    ENDIF
    !     
    !     calculate gh
    !     
    DO i=1,lenb
       ii = i1(i)
       tx3(i)   =  qol(ii,km1) - qol(ii,k)
       gmh(i,k) = hol(i,k) + tx3(i) * pri(ii,k) * (albcp*0.5e0)
       akm(i)   = akm(i) + gam(ii,km1)*(prj(ii,k)-prh(ii,km1))  &
            * gmh(i,k)
    END DO

    IF (ic1 .LE. km1) THEN
       DO l=km1,ic1,-1
          DO i=1,lenb
             ii = i1(i)
             tx2(i) = tx3(i)
             tx3(i) = (qol(ii,l-1) - qol(ii,l)) * eta(i,l)
             tx2(i) = tx2(i) + tx3(i)
             gmh(i,l) = hol(i,l) + tx2(i)   * pri(ii,l) * (albcp*0.5e0)
          END DO
       END DO
    ENDIF

    DO i=lena1,lenb
       tx3(i) = tx3(i) + twobal &
            * (tx7(i) - tx8(i) - tx5(i) - qol(i1(i),ic)*alhl)
    END DO

    DO i=1,lenb
       gmh(i,ic) = tx1(i) + pri(i1(i),ic) * onebcp &
            * (tx3(i)*(alhl*0.5e0) + eta(i,ic) * tx8(i))
    END DO
    !     
    !     calculate hc part of akm
    !     
    IF (ic1 .LE. km1) THEN
       DO i=1,lenb
          tx1(i) = gmh(i,k)
       END DO
       DO l=km1,ic1,-1
          DO i=1,lenb
             ii = i1(i)
             tx1(i) = tx1(i) + (eta(i,l) - eta(i,l+1)) * gmh(i,l)
             tx2(i) = gam(ii,l-1) * (prj(ii,l) - prh(ii,l-1))
          END DO
          IF (l .EQ. ic1) THEN
             DO i=lena1,lenb
                tx2(i) = 0.0e0
             END DO
          ENDIF
          DO i=1,lenb
             ii = i1(i)
             akm(i) = akm(i) + tx1(i) *  &
                  (tx2(i) + gam(ii,l)*(prh(ii,l)-prj(ii,l)))
          END DO
       END DO
    ENDIF

    DO i=lena1,lenb
       ii = i1(i)
       tx2(i) = 0.5 * (prs(ii,ic) + prs(ii,ic1)) &
            + 0.5*(prs(ii,ic+2) - prs(ii,ic)) * (1.0e0-tx6(i))
       tx1(i) = prs(ii,ic1)
       tx5(i) = 0.5 * (prs(ii,ic1) + prs(ii,ic+2))
       IF ((tx2(i) .GE. tx1(i)) .AND. (tx2(i) .LT. tx5(i))) THEN
          tx6(i)     = 1.0e0 - (tx2(i) - tx1(i)) / (tx5(i) - tx1(i))
          tem        = pri(ii,ic1) / pri(ii,ic)
          hol(i,ic1) = hol(i,ic1) + hol(i,ic) * tem
          hol(i,ic)  = 0.0e0
          gmh(i,ic1) = gmh(i,ic1) + gmh(i,ic) * tem
          gmh(i,ic)  = 0.0e0
       ELSEIF (tx2(i) .LT. tx1(i)) THEN
          tx6(i) = 1.0
       ELSE
          tx6(i) = 0.0
       ENDIF
    END DO

    DO i=1,lenc
       pcu(i) = 0.0
    ENDDO

    DO i=1,lenb
       ii = i1(i)
       IF (akm(i) .LT. 0.0e0 .AND. wlq(i) .GE. 0.0) THEN
          wfn(i) = - tx6(i) * wfn(i) * rasalf / akm(i)
       ELSE
          wfn(i) = 0.0e0
       ENDIF
       tem       = (prs(ii,k+1)-prs(ii,k))*(cmb2pa*frac)
       wfn(i)    = amin1(wfn(i), tem)
       !
       !     compute cloud amount
       !
       etamn=0.0
       IF(km1.GT.ic) THEN
          DO ksl= km1,ic,-1
             etamn=etamn+eta(i,ksl)
          END DO
          etamn=etamn/float(km1-ic)
          tx1(i)=wfn(i)*864.*etamn
       ELSE
          tx1(i)=wfn(i)*864.
       ENDIF
       !
       !     precipitation
       !
       pcu(ii) =  wlq(i) * wfn(i) * onebg
       !     
       !     cumulus friction at the bottom layer
       !     
       tx4(i)   = wfn(i) * (1.0/alhl)
       tx5(i)   = wfn(i) * onebcp
    END DO

    DO i=1,lenb
       ii = i1(i)
       cln(ii) = tx1(i)
    END DO
    !     
    !     theta and q change due to cloud type ic
    !     
    DO l=ic,k
       DO i=1,lenb
          ii = i1(i)
          tem       = (gmh(i,l) - hol(i,l)) * tx4(i)
          tem1      =  hol(i,l) * tx5(i)
          tcu(ii,l) = tem1 / prh(ii,l)
          qcu(ii,l) = tem
       END DO
    END DO

    DO l=1,k
       DO i=1,lenc
          hst(i,l) = 0.0
          qol(i,l) = 0.0
       END DO
    END DO
  END SUBROUTINE cloud



  ! ras     :relaxed arakawa-schubert.



  SUBROUTINE ras( &
       len   ,lenc  ,k     ,dt    ,ncrnd ,krmax ,frac  , &
       rasal , botop,alhl  ,rkap  ,poi   , qoi  ,uoi   ,voi   , &
       prs   ,prj   ,cup   ,cln   ,q1    ,q2    ,alf   ,bet   ,gam   , &
       prh   ,pri   ,hoi   ,eta   ,tcu   ,qcu   ,hst   ,qol   ,gmh   , &
       tx1   ,tx2   ,tx3   ,tx4   ,tx5   ,tx6   ,tx7   ,tx8   ,tx9   , &
       wfn   ,akm   ,qs1   ,clf   ,uht   ,vht   ,wlq   ,pcu   ,ia    , &
       i1    ,i2    ,kmax  ,kmaxm1,kmaxp1)

    !==========================================================================
    !
    ! ras     :relaxed arakawa-schubert.
    !==========================================================================
    ! kmax......Number of sigma levels       
    ! kmaxm1....Parameter kmaxm1 = kmax-1
    ! kmaxp1....Parameter kmaxp1 = kmax+1
    ! icm.......Parameter icm    = 100      
    ! daylen....Parameter daylen = 86400.0
    ! cp........specific heat of air           (j/kg/k) 
    ! grav......gas constant of dry air        (j/kg/k)
    ! p00.......Pressao de referencia ao nivel do mar (mb)
    ! cmb2pa....Parameter cmb2pa = 100.0
    ! rhmax.....Parameter rhmax  = 0.9999  
    ! ncols......Number of grid points on a gaussian latitude circle   
    ! actp......Parameter actp   = 1.7 
    ! facm......Parameter facm   = 1.0   
    ! p5........Parameter p5     = 500.0     
    ! p8........Parameter p8     = 800.0         
    ! pt8.......Parameter pt8    = 0.8  
    ! pt2.......Parameter pt2    = 0.2
    ! pfac......Parameter pfac   = pt2/(p8-p5)    
    ! cucld.....Parameter cucld  = 0.5  
    ! len.......ncols
    ! lenc......ncols
    ! k.........kmax
    ! rkap......rkap=r/cp=287.05e0/1004.6e0
    ! alhl......alhl=2.52e6
    ! dt........time interval,usually =delt,but changes
    !           in nlnmi (dt=1.) and at dead start(delt/4,delt/2) 
    ! frac......frac=1.
    ! krmax.....=nls
    ! nls.......Number of layers in the stratosphere.    
    ! ncrnd.....ncrnd=0
    ! poi.......array pot. temp.  
    ! qoi.......array humidity.
    ! prs  
    ! prj  
    ! uoi  
    ! voi  
    ! rasal
    ! q1.......heating diagnostics 
    ! q2.......moitening diagnostics       
    ! cln  
    ! cup......rainfall diagnostic (mm/day)
    ! tcu  
    ! qcu  
    ! alf  
    ! bet  
    ! gam  
    ! gmh  
    ! eta  
    ! hoi  
    ! hst  
    ! qol  
    ! prh  
    ! pri  
    ! tx1  
    ! tx2  
    ! tx3  
    ! tx4  
    ! tx5  
    ! tx6  
    ! tx7  
    ! tx8  
    ! tx9  
    ! wfn  
    ! akm  
    ! qs1  
    ! wlq  
    ! pcu  
    ! uht  
    ! vht  
    ! clf  
    ! ia   
    ! i1   
    ! i2   
    ! botop.......botop=.true.
    !==========================================================================
    INTEGER, INTENT(in   ) :: kmax  
    INTEGER, INTENT(in   ) :: kmaxm1
    INTEGER, INTENT(in   ) :: kmaxp1

    INTEGER, INTENT(in   ) :: len
    INTEGER, INTENT(in   ) :: lenc
    INTEGER, INTENT(inout) :: k
    REAL,    INTENT(in   ) :: rkap
    REAL,    INTENT(in   ) :: alhl
    REAL,    INTENT(in   ) :: dt
    REAL,    INTENT(in   ) :: frac
    INTEGER, INTENT(in   ) :: krmax
    INTEGER, INTENT(in   ) :: ncrnd

    REAL,    INTENT(inout) :: poi  (len,k)
    REAL,    INTENT(inout) :: qoi  (len,k) 
    REAL,    INTENT(inout) :: prs  (len,k+1) 
    REAL,    INTENT(inout) :: prj  (len,k+1)
    REAL,    INTENT(in   ) :: uoi  (len,k)
    REAL,    INTENT(in   ) :: voi  (len,k)
    REAL,    INTENT(in   ) :: rasal(k-1)

    REAL,    INTENT(inout) :: q1   (len,k) 
    REAL,    INTENT(inout) :: q2   (len,k) 
    REAL,    INTENT(out  ) :: cln  (len,k)
    REAL,    INTENT(inout) :: cup  (len,k)
    REAL,    INTENT(inout) :: tcu  (len,k) 
    REAL,    INTENT(inout) :: qcu  (len,k)

    REAL,    INTENT(inout) :: alf  (len,k) 
    REAL,    INTENT(inout) :: bet  (len,k) 
    REAL,    INTENT(inout) :: gam  (len,k) 
    REAL,    INTENT(inout) :: gmh  (lenc,k)
    REAL,    INTENT(inout) :: eta  (lenc,k) 
    REAL,    INTENT(inout) :: hoi  (lenc,k) 
    REAL,    INTENT(inout) :: hst  (lenc,k) 
    REAL,    INTENT(inout) :: qol  (lenc,k)
    REAL,    INTENT(inout) :: prh  (len,k) 
    REAL,    INTENT(inout) :: pri  (len,k)

    REAL,    INTENT(inout) :: tx1  (lenc) 
    REAL,    INTENT(inout) :: tx2  (lenc) 
    REAL,    INTENT(inout) :: tx3  (lenc) 
    REAL,    INTENT(inout) :: tx4  (lenc) 
    REAL,    INTENT(inout) :: tx5  (lenc)
    REAL,    INTENT(inout) :: tx6  (lenc) 
    REAL,    INTENT(inout) :: tx7  (lenc) 
    REAL,    INTENT(inout) :: tx8  (lenc) 
    REAL,    INTENT(inout) :: tx9  (lenc) 
    REAL,    INTENT(inout) :: wfn  (lenc) 
    REAL,    INTENT(inout) :: akm  (lenc) 
    REAL,    INTENT(inout) :: qs1  (lenc) 
    REAL,    INTENT(inout) :: wlq  (lenc) 
    REAL,    INTENT(inout) :: pcu  (lenc)
    REAL,    INTENT(inout) :: uht  (lenc) 
    REAL,    INTENT(inout) :: vht  (lenc) 
    REAL,    INTENT(out  ) :: clf  (lenc)
    INTEGER, INTENT(inout) :: ia   (lenc)   
    INTEGER, INTENT(inout) :: i1   (lenc) 
    INTEGER, INTENT(inout) :: i2   (lenc)
    LOGICAL, INTENT(in   ) :: botop    

    REAL   , PARAMETER :: cmb2pa=100.0
    REAL   , PARAMETER :: rhmax =0.9999
    INTEGER, PARAMETER :: icm   =100 
    REAL   , PARAMETER :: daylen=86400.0

    REAL                   :: rkapp1
    REAL                   :: onebcp
    REAL                   :: albcp
    INTEGER                :: km1
    REAL                   :: onbdt
    REAL                   :: fracs
    INTEGER                :: kcr
    INTEGER                :: kfx
    INTEGER                :: ncmx
    INTEGER                :: nc
    INTEGER                :: ib
    REAL                   :: rasalf
    INTEGER                :: ic(icm)
    LOGICAL                :: setras
    INTEGER                :: i
    INTEGER                :: l

    setras = .FALSE.
    rkapp1=1.0 + rkap
    onebcp = 1.0 / cp
    albcp = alhl * onebcp
    !
    !     dgd modify k value
    !
    k=kmaxm1
    DO i=1,lenc
       poi(i,kmaxm1)=poi(i,kmax)*(prs(i,kmaxp1)-prs(i,kmax))+ &
            poi(i,kmaxm1)*(prs(i,kmax)-prs(i,kmaxm1))
       poi(i,kmaxm1)=poi(i,kmaxm1)/(prs(i,kmaxp1)-prs(i,kmaxm1))
       qoi(i,kmaxm1)=qoi(i,kmax)*(prs(i,kmaxp1)-prs(i,kmax))+ &
            qoi(i,kmaxm1)*(prs(i,kmax)-prs(i,kmaxm1))
       qoi(i,kmaxm1)=qoi(i,kmaxm1)/(prs(i,kmaxp1)-prs(i,kmaxm1))
    END DO

    DO i=1,lenc
       prj(i,kmaxp1)=prj(i,kmaxp1)
       prj(i,kmax)=prj(i,kmaxp1)
       prs(i,kmaxp1)=prs(i,kmaxp1)
       prs(i,kmax)=prs(i,kmaxp1)
    END DO
    DO l=1,k
       DO i=1,lenc
          prh(i,l) = (prj(i,l+1)*prs(i,l+1) - prj(i,l)*prs(i,l)) &
               / ((prs(i,l+1)-prs(i,l)) * rkapp1)
       END DO
    END DO
    DO l=1,k
       DO i=1,lenc
          tx5(i) = poi(i,l) * prh(i,l)
          tx1(i) = (prs(i,l) + prs(i,l+1)) * 0.5
          tx3(i) = tx5(i)
          CALL qsat(tx3(i), tx1(i), tx2(i), tx4(i), .TRUE.)
          alf(i,l) = tx2(i) - tx4(i) * tx5(i)
          bet(i,l) = tx4(i) * prh(i,l)
          gam(i,l) = 1.0 / ((1.0 + tx4(i)*albcp) * prh(i,l))
          pri(i,l) = (cp/cmb2pa) / (prs(i,l+1) - prs(i,l))
       END DO
    END DO
    !
    !     done modification
    !
    km1    = k  - 1
    onbdt  = 1.0 / dt
    fracs  = frac  * onbdt
    !
    !     set number of clouds to adjust during this call to ras, ncmx,
    !     and the cloud calling sequence, ic.  this allows various
    !     combinations of randomly and sequentially called clouds.
    !
    kcr   = MIN(km1,krmax)
    kfx   = km1 - kcr
    ncmx  = kfx + ncrnd
    IF (kfx .GT. 0) THEN
       IF (botop) THEN
          DO nc=1,kfx
             ic(nc) = k - nc
          END DO
       ELSE
          DO nc=kfx,1,-1
             ic(nc) = k - nc
          END DO
       END IF
    END IF
    !
    !     this area commented until machine independent random number
    !     generator can be found.  parameters set in arprep to use 
    !     non random clouds
    !     
    !     IF (ncrnd .gt. 0) THEN
    !     cray rng setup
    !     call ranset(iseed)
    !     dec rng setup
    !     call srand(iseed)
    !     do 30 i=1,ncrnd
    !     cray rng
    !     irnd = (ranf()-0.0005)*(kcr-krmin+1)
    !     dec rng
    !     irnd = (rand()-0.0005)*(kcr-krmin+1)
    !     ic(kfx+i) = irnd + krmin
    !     30    continue
    !     END IF
    !     
    !     loop over clouds to be adjusted during this call
    !
    DO nc=1,ncmx
       ib = ic(nc)
       rasalf = rasal(ib) * onbdt
       CALL cloud( &
            len   ,lenc  ,k     ,ib    ,rasalf,setras,fracs ,alhl  ,rkap  , &
            poi   ,qoi   ,uoi   ,voi   ,prs   ,prj   ,pcu   ,cln(1,ib),tcu, &
            qcu   ,alf   ,bet   ,gam   ,prh   ,pri   ,hoi   ,eta   ,hst   , &
            qol   ,gmh   ,tx1   ,tx2   ,tx3   ,tx4   ,tx5   ,tx6   ,tx7   , &
            tx8   ,tx9   ,wfn   ,akm   ,qs1   ,clf   ,uht   ,vht   ,wlq   , &
            ia    ,i1    ,i2    ,cmb2pa,rhmax )
       DO l=ib,k
          DO i=1,lenc
             !
             !     update pot. temp. and humidity.
             !
             poi(i,l) = poi(i,l) + tcu(i,l) * dt
             qoi(i,l) = qoi(i,l) + qcu(i,l) * dt
             !
             !     heating and moitening diagnostics
             !
             q1(i,l)  = q1(i,l)  + tcu(i,l) * prh(i,l) * daylen
             q2(i,l)  = q2(i,l)  + qcu(i,l) * daylen
          END DO
       END DO
       !
       !     rainfall diagnostic (mm/day)
       !
       DO i=1,lenc
          cup(i,ib) = cup(i,ib) + pcu(i) * daylen
       END DO
    END DO
  END SUBROUTINE ras




  ! arprep :used to interface with the relaxed arakawa schubert code of 
  !         moorthi and suarez.



  SUBROUTINE arprep (dtwrk ,dqwrk ,sl    ,si    ,fpn   ,ktop  ,kbot  ,rrr   , &
       hrar  ,qrar  ,dt    ,ftn   ,fqn   ,del   ,kuo   ,cldm  , &
       cflric,kmaxp1,kmaxm1,ncols  ,kmax  ,nls   )
    !
    !==========================================================================
    ! ncols......Number of grid points on a gaussian latitude circle     
    ! kmax......Number of sigma levels     
    ! nls.......Number of layers in the stratosphere.    
    ! nlst......Parameter nlst   = 01    
    ! kmaxm1....Parameter kmaxm1 = kmax-1
    ! kmaxp1....Parameter kmaxp1 = kmax+1
    ! icm.......Parameter icm    = 100 
    ! daylen....Parameter daylen = 86400.0 
    ! cmb2pa....Parameter cmb2pa = 100.0
    ! rhmax.....Parameter rhmax  = 0.9999 
    ! dt........time interval,usually =delt,but changes
    !           in nlnmi (dt=1.) and at dead start(delt/4,delt/2)   
    ! cflric....parameter used by relaxed arakawa-schubert
    ! fpn.......sfc pres (cb)
    ! sl........sigma value at midpoint of
    !                                         each layer : (k=287/1005)
    !
    !                                                                     1
    !                                             +-                   + ---
    !                                             !     k+1         k+1!  k
    !                                             !si(l)   - si(l+1)   !
    !                                     sl(l) = !--------------------!
    !                                             !(k+1) (si(l)-si(l+1)!
    !                                             +-                  -+      
    ! si........si(l)=1.0-ci(l).  
    ! ci........sigma value at each level.        
    ! dtwrk
    ! dqwrk
    ! ktop......ktop (g.t.e 1 and l.t.e. km) is highest lvl for which
    !           tin is colder than moist adiabat given by the
    !           (ktop=1 denotes tin(k=2) is already g.t.e. moist adb.)
    !           allowance is made for perhaps one level below ktop
    !           at which tin was warmer than tmst. 
    ! kbot......is the first regular level above the lcl    
    ! ftn.......temperature field in the spectral grid
    ! fqn.......specific humidit field in the spectral grid  
    ! hrar......this array is needed for the heating from ras scheme
    ! qrar......this array is needed for the mostening from ras scheme 
    ! rrr  
    ! del ......sigma spacing for each layer computed in routine "setsig". 
    ! kuo.......flag to indicate that deep convection was done
    !           kuo, ktop and plcl are longitude arrays       
    ! cldm 
    !==========================================================================
    INTEGER, INTENT(in   ) :: kmaxp1
    INTEGER, INTENT(in   ) :: kmaxm1
    INTEGER, INTENT(in   ) :: ncols    
    INTEGER, INTENT(in   ) :: kmax    
    INTEGER, INTENT(in   ) :: nls     
    !
    !     default parameter statements for dimensioning resolution
    !             
    REAL,    INTENT(in   ) :: dt
    REAL,    INTENT(in   ) :: cflric
    REAL,    INTENT(in   ) :: fpn  (ncols)
    REAL,    INTENT(in   ) :: sl   (kmax)
    REAL,    INTENT(in   ) :: si   (kmaxp1)
    REAL,    INTENT(in   ) :: dtwrk(ncols,kmax)
    REAL,    INTENT(in   ) :: dqwrk(ncols,kmax)
    INTEGER, INTENT(inout) :: ktop (ncols)
    INTEGER, INTENT(out  ) :: kbot (ncols)
    REAL,    INTENT(out  ) :: ftn  (ncols,kmax)
    REAL,    INTENT(inout) :: fqn  (ncols,kmax)
    REAL,    INTENT(inout) :: hrar (ncols,kmax)
    REAL,    INTENT(inout) :: qrar (ncols,kmax)
    REAL,    INTENT(out  ) :: rrr  (ncols)
    REAL,    INTENT(in   ) :: del  (kmax)
    INTEGER, INTENT(out  ) :: kuo  (ncols)
    REAL,    INTENT(inout) :: cldm (ncols)
    !     
    !     set up variables
    !
    REAL                   :: prs  (ncols,kmaxp1)
    REAL                   :: prj  (ncols,kmaxp1)
    REAL                   :: rasal(kmaxm1)
    REAL                   :: poi  (ncols,kmax)
    REAL                   :: qoi  (ncols,kmax)
    REAL                   :: uoi  (ncols,kmax)
    REAL                   :: voi  (ncols,kmax)
    REAL                   :: rrr1 (ncols)  
    REAL                   :: cln  (ncols,kmax)
    REAL                   :: q1   (ncols,kmax)
    REAL                   :: q2   (ncols,kmax)
    REAL                   :: tcu  (ncols,kmax)
    REAL                   :: qcu  (ncols,kmax)
    REAL                   :: rnew (ncols)
    REAL                   :: rdum (ncols)
    REAL                   :: rvd  (ncols,kmax)
    !     
    !     work arrays needed for relaxed arakawa-schubert
    !        
    REAL                 :: alf(ncols,kmax) 
    REAL                 :: bet(ncols,kmax) 
    REAL                 :: gam(ncols,kmax)
    REAL                 :: gmh(ncols,kmax)
    REAL                 :: eta(ncols,kmax) 
    REAL                 :: hoi(ncols,kmax) 
    REAL                 :: hst(ncols,kmax)
    REAL                 :: qol(ncols,kmax)
    REAL                 :: prh(ncols,kmax) 
    REAL                 :: pri(ncols,kmax)

    REAL                 :: tx1(ncols) 
    REAL                 :: tx2(ncols)
    REAL                 :: tx3(ncols) 
    REAL                 :: tx4(ncols) 
    REAL                 :: tx5(ncols)
    REAL                 :: tx6(ncols) 
    REAL                 :: tx7(ncols) 
    REAL                 :: tx8(ncols) 
    REAL                 :: tx9(ncols)
    REAL                 :: wfn(ncols) 
    REAL                 :: akm(ncols) 
    REAL                 :: qs1(ncols) 
    REAL                 :: wlq(ncols) 
    REAL                 :: pcu(ncols)
    REAL                 :: uht(ncols) 
    REAL                 :: vht(ncols) 
    REAL                 :: clf(ncols)
    INTEGER              :: ia (ncols)   
    INTEGER              :: i1 (ncols) 
    INTEGER              :: i2 (ncols) 
    LOGICAL              :: botop
    INTEGER              :: len  
    INTEGER              :: lenc   
    INTEGER              :: k1  
    REAL                 :: twodt  
    INTEGER              :: krmin  
    INTEGER              :: krmax  
    INTEGER              :: ncrnd  
    REAL                 :: alhl   
    REAL                 :: rkap   
    REAL                 :: frac   
    INTEGER              :: k  
    REAL                 :: cnst   
    INTEGER              :: i  

    INTEGER, PARAMETER     ::  nlst   =01

    !
    !     define constants
    !
    botop=.TRUE.
    len=ncols
    lenc=ncols
    k1=kmax
    twodt=2.*dt
    krmin=nls
    krmax=krmin
    ncrnd=0
    alhl=2.52e6
    rkap=gasr/cp
    frac=1.
    !     
    !     define variables 
    !     
    !     now need to set up arrays for call to arakawa -schubert and 
    !     flip over since the subroutine uses an inverted vertical
    !     coordinate compared to that in the rest of the model
    !     
    !     set up arrays
    !
    DO k=1,kmaxp1
       cnst=si(k)**rkap
       DO i=1,ncols
          prj(i,kmax+2-k)=cnst
          prs(i,kmax+2-k)=si(k)*fpn(i)*10.
       END DO
    END DO
    DO  k=1,kmax
       cnst=((1./sl(k))**rkap)
       DO i=1,ncols
          poi(i,kmax+1-k)=dtwrk(i,k)*cnst
          qoi(i,kmax+1-k)=dqwrk(i,k)
          hrar(i,k)=0.0
          qrar(i,k)=0.0
          uoi(i,k)=0.0
          voi(i,k)=0.0
          cln(i,k)=0.0
          q1(i,k)=0.0
          q2(i,k)=0.0
          tcu(i,k)=0.0
          qcu(i,k)=0.0
          alf(i,k)=0.0
          bet(i,k)=0.0
          gam(i,k)=0.0
          gmh(i,k)=0.0
          eta(i,k)=0.0
          hoi(i,k)=0.0
          hst(i,k)=0.0
          qol(i,k)=0.0 
          prh(i,k)=0.0
          pri(i,k)=0.0
          rvd(i,k)=0.0
       END DO
    END DO
    DO k=1,kmaxm1
       rasal(k)=twodt/7200.
    END DO
    DO i=1,ncols
       rrr(i)=0.0
       rrr1(i)=0.0
       ktop(i)=3
       kbot(i)=3
       tx9(i)=0.0
       !
       !     needed for cloud fraction based on mass flux
       !
       cldm(i)=0.0
    END DO
    !     
    !     call relaxed arakawa-schubert
    !     
    CALL ras( &
         len   ,lenc  ,k1    ,twodt ,ncrnd ,krmax ,frac  , &
         rasal ,botop ,alhl  ,rkap  ,poi   ,qoi   ,uoi   ,voi   , &
         prs   ,prj   ,rvd   ,cln   ,q1  ,q2 ,alf   ,bet   ,gam   , &
         prh   ,pri   ,hoi   ,eta   ,tcu   ,qcu ,hst   ,qol   ,gmh   , &
         tx1   ,tx2   ,tx3   ,tx4   ,tx5   ,tx6 ,tx7   ,tx8   ,tx9   , &
         wfn   ,akm   ,qs1   ,clf   ,uht   ,vht ,wlq   ,pcu   ,ia    , &
         i1    ,i2    ,kmax  ,kmaxm1,kmaxp1)
    !     
    !     now need to assign and flip output arrays
    !     
    DO k=1,kmax
       DO i=1,ncols
          IF(cln(i,kmaxp1-k).GT.0.) THEN
             ktop(i)=k
             !
             !     needed for cloud fraction based on mass flux
             !
             cldm(i)=cldm(i)+cln(i,kmaxp1-k)
          ELSE
             ktop(i)=ktop(i)
             cldm(i)=cldm(i)
          END IF
       END DO
    END DO
    DO k=1,kmax
       DO i=1,ncols
          hrar(i,k)=q1(i,kmaxp1-k)/86400.
          qrar(i,k)=q2(i,kmaxp1-k)/86400.
          ftn(i,k)=dtwrk(i,k)+twodt*q1(i,kmaxp1-k)/86400.
          fqn(i,k)=dqwrk(i,k)+twodt*q2(i,kmaxp1-k)/86400.
          rvd(i,k)=twodt*rvd(i,k)/86400.
       END DO
    END DO
    DO i=1,ncols
       cldm(i)=0.035*LOG(1.+cldm(i))
    END DO
    !
    !     now for subcloud layer
    !
    DO i=1,ncols
       hrar(i,1)=hrar(i,2)
       qrar(i,1)=qrar(i,2)
       ftn(i,1)=dtwrk(i,1)+twodt*hrar(i,2)
       fqn(i,1)=dqwrk(i,1)+twodt*qrar(i,2)
    END DO
    !
    !     now for re-evaporation of fallingrain
    !
    CALL rnevp(ftn   ,fqn   ,rvd   ,fpn   ,sl    ,del   ,cflric, &
         rnew  ,rdum  ,twodt ,ncols ,kmax  ,nlst)
    !
    !        now need to modify values for re-evaporation
    !
    DO i=1,ncols
       rrr(i)=0.5*1./1000.*rnew(i)
       IF(rrr(i).GT.0.) kuo(i)=1
    END DO

    DO k=1,kmax
       DO i=1,ncols
          hrar(i,k)=(ftn(i,k)-dtwrk(i,k))/twodt
          qrar(i,k)=(fqn(i,k)-dqwrk(i,k))/twodt
       END DO
    END DO
  END SUBROUTINE arprep




  SUBROUTINE shllcl(dt, ps, sl, si, qn1, tn1, msta, kuo, &
       plcl, kktop, kkbot, ncols, kmax)
    !
    !==========================================================================
    ! ncols......Number of grid points on a gaussian latitude circle  
    ! kmax......Number of sigma levels  
    ! msta
    ! dt........time interval,usually =delt,but changes
    !           in nlnmi (dt=1.) and at dead start(delt/4,delt/2)   
    ! ps........surface pressure      
    ! sl........sigma value at midpoint of
    !                                         each layer : (k=287/1005)
    !
    !                                                                     1
    !                                             +-                   + ---
    !                                             !     k+1         k+1!  k
    !                                             !si(l)   - si(l+1)   !
    !                                     sl(l) = !--------------------!
    !                                             !(k+1) (si(l)-si(l+1)!
    !                                             +-                  -+   
    ! si........si(l)=1.0-ci(l).  
    ! ci........sigma value at each level.     
    ! qn1.......qn1 is q (specific humidit) at the n+1 time level 
    ! tn1.......tn1 is tmp (temperature) at the n+1 time level 
    ! kktop......ktop (g.t.e 1 and l.t.e. km) is highest lvl for which
    !            tin is colder than moist adiabat given by the
    !            (ktop=1 denotes tin(k=2) is already g.t.e. moist adb.)
    !            allowance is made for perhaps one level below ktop
    !            at which tin was warmer than tmst.
    ! kuo........flag to indicate that deep convection was done
    !            kuo, ktop and plcl are longitude arrays     
    ! plcl.......pressure at the lcl 
    ! kkbot......is the first regular level above the lcl   
    ! cp........Specific heat of air      (j/kg/k) 
    ! hl........heat of evaporation of water     (j/kg) 
    ! gasr......gas constant of dry air        (j/kg/k)  
    ! rmwmd.....fracao molar entre a agua e o ar       
    ! sthick....upper limit for originating air for lcl.  replaces kthick.      
    ! ki........lowest level from which parcels can be lifted to find lcl            
    !==========================================================================
    IMPLICIT NONE
    INTEGER, INTENT(IN   ) :: ncols
    INTEGER, INTENT(IN   ) :: kmax
    INTEGER, INTENT(INOUT) :: msta
    REAL,    INTENT(IN   ) :: dt
    REAL,    INTENT(IN   ) :: ps   (ncols)
    REAL,    INTENT(IN   ) :: sl   (kmax)
    REAL,    INTENT(IN   ) :: si   (kmax+1)
    REAL,    INTENT(IN   ) :: qn1  (ncols,kmax)
    REAL,    INTENT(IN   ) :: tn1  (ncols,kmax)
    INTEGER, INTENT(OUT  ) :: kktop(ncols)
    INTEGER, INTENT(OUT  ) :: kuo  (ncols)
    REAL,    INTENT(OUT  ) :: plcl (ncols)
    INTEGER, INTENT(OUT  ) :: kkbot(ncols)

    REAL    :: press (ncols,kmax)
    REAL    :: tin   (ncols,kmax)
    REAL    :: qin   (ncols,kmax)         
    REAL    :: tmst  (ncols,kmax)
    REAL    :: qmst  (ncols,kmax)
    REAL    :: tpar  (ncols)
    REAL    :: espar (ncols)
    REAL    :: qspar (ncols)
    REAL    :: qpar  (ncols)
    REAL    :: qex1  (ncols)
    REAL    :: tlcl  (ncols)
    REAL    :: qexces(ncols)
    REAL    :: dqdp  (ncols)
    REAL    :: deltap(ncols)
    REAL    :: hnew  (ncols)
    REAL    :: slcl  (ncols)
    INTEGER :: ier   (ncols)         
    LOGICAL :: llift (ncols)
    LOGICAL :: lconv (ncols)
    INTEGER :: ll    (ncols)       

    REAL    :: cappa
    REAL    :: rdt
    REAL    :: cpovl
    REAL    :: kk
    INTEGER :: i
    INTEGER :: k
    INTEGER :: kthick

    cappa=gasr/cp

    IF(dt.EQ.0.0e0) RETURN
    !
    !     set default values
    !
    DO i=1,ncols
       kktop(i)=1
       kkbot(i)=1
       llift(i)=.FALSE.
       lconv(i)=.FALSE.
    END DO
    !     
    !     define kthick in terms of sigma values
    !     
    DO k=1,kmax
       IF(si(k).GE.sthick.AND.si(k+1).LT.sthick) THEN
          kthick = k
          EXIT
       ENDIF
    END DO
    rdt=1.0e0/dt
    cpovl= cp/hl
    kk = kmax
    !     
    !     qn is q at the n-1 time level
    !     del=del sigma (del p ovr psfc)
    !     ps=sfc pres (cb)
    !     
    DO i=1, ncols
       hnew(i) = 1.0e-2*ps(i)
    END DO

    DO  k=1,kmax
       DO  i=1,ncols
          press(i,k)=sl(k)*ps(i)
       END DO
    END DO
    !     
    !     tin, qin are prelim tmp and q at n+1
    !     zero q if it is  negative
    !
    DO k=1,kmax
       DO i=1,ncols
          qin(i,k) = qn1(i,k)
          IF (qn1(i,k).LE.0.0e0) qin(i,k) = 1.0e-12
       END DO
    END DO

    DO k=1,kmax
       DO i=1,ncols
          tin(i,k) = tn1(i,k)
       END DO
    END DO

    DO i=1,ncols
       qex1(i) = 0.0e0
       qpar(i) = qin(i,ki)
    END DO
    !     
    !     lift parcel from k=ki until it becomes saturated
    !
    DO k =ki,kthick
       DO i=1,ncols
          IF(.NOT.llift(i)) THEN
             tpar(i) = tin(i,ki)* &
                  EXP(cappa*LOG(press(i,k)/press(i,ki)))
             espar(i) = es(tpar(i))
             qspar(i) =rmwmd*espar(i)/ &
                  (press(i,k)-(1.0e0-rmwmd)*espar(i))
             qexces(i) = qpar(i) - qspar(i)
             !
             !     if parcel not saturated,  try next level
             !
             IF (qexces(i).LT.0.0e0) THEN
                qex1(i) = qexces(i)
                !
                !     saturated - go down and find p,t, sl at the lcl;
                !     if sat exists in first layer (k=ki), use this as lcl
                !
             ELSE IF (k.EQ.ki) THEN
                plcl(i) = press(i,k)
                tlcl(i) = tpar(i)
                tlcl(i) = tlcl(i) + 1.0e0
                slcl(i) = plcl(i)/ps(i)
                ll(i)   = k
                kkbot(i) = ll(i)
                llift(i) = .TRUE.
             ELSE
                dqdp(i) = (qexces(i)-qex1(i))/ &
                     (press(i,k-1)-press(i,k))
                deltap(i)= qexces(i)/dqdp(i)
                plcl(i)=press(i,k) + deltap(i)
                tlcl(i)=  tpar(i) * (1.0e0+2.0e0*cappa*deltap(i) &
                     /(press(i,k)+press(i,k-1)))
                tlcl(i) = tlcl(i) + 1.0e0
                slcl(i) = plcl(i)/ps(i)
                ll(i)   = k
                kkbot(i) = ll(i)
                llift(i) = .TRUE.
                !
                !     give parcel a one-degree goose
                !
             ENDIF
             !
             !     lifting cond level found - get out of loop
             !
          ENDIF
       END DO
    END DO
    !     
    !     quit if parcel still unsat at k = kthick
    !     in this case,set low value of plcl as signal to shalmano
    !
    DO i=1,ncols
       IF(.NOT.llift(i)) THEN
          plcl(i) = 1.0e0
          kuo  (i) = 5
       ENDIF
    END DO
    CALL mstad2(hnew, sl, tin, tmst, qmst,  kktop, ier, &
         slcl, ll, qin, tlcl, llift, ncols, kmax)
    !
    !     tmst and qmst,k=1...ktop contain cloud temps and specific humid.
    !     store values of plcl and ktop to pass to shalmano
    !
    DO i=1,ncols
       IF(llift(i)) THEN
          IF(ier(i).NE.0) THEN
             msta = msta+1
             kuo(i) =6
             kktop(i) = 1
             plcl(i) = 10.e0
             lconv(i)=.FALSE.
          ELSE
             lconv(i)=.TRUE.
          ENDIF
       ENDIF
    END DO
  END SUBROUTINE shllcl


  !     gwater : performs (1) moist convective adjustment, calculating
  !              the resulting precipitation, (2) calculation of
  !              precipitation resulting from large-scale phenomena and
  !              (3) dry convective adjustment.



  SUBROUTINE gwater2( &
       ncols ,&
       kmax  ,&
       geshem,&
       ppli  ,&
       ppci  ,&
       convc ,&
       convt ,&
       convb ,&
       prcp1 ,&
       prcp2 ,&
       prcp3 ,&
       prcpt ,&
       toplv ,&
       botlv ,&
       convts,&
       convcs,&
       convbs,&
       prct  ,&
       prcc  ,&
       si    ,&
       sl    ,&
       del   ,&
       latco ,&
       rqn   ,&
       ftn   ,&
       fqn   ,&
       fpn   ,&
       wtn   ,&
       wqn   ,&
       uun   ,&
       vvn   ,&
       omgn) 

    !
    !**************************************************************************
    !
    !     gwater is called by the main routine smf.
    !
    !     gwater calls the following subroutines :  pln2    sumpls  ffs54
    !                                               conkuo  lrgscl  tetfix
    !                                               ffa38   symasy  fl22
    !
    !**************************************************************************
    !
    !    argument(dimensions)                       description
    !
    !          filta                  input : weight used on central time
    !                                         step of robert time filter.
    !                                         set in main routine "smf".
    !            dt                   input : time step (sec). set in main
    !                                         routine "smf".
    !          iflag                  input : if iflag=0, robert time filter
    !                                         is skipped. otherwise, robert
    !                                         time filter is performed. set
    !                                         in main routine "smf".
    !           nun                  output : index used by routine "tetfix"
    !                                         to accumulate number of
    !                                         points for which a dry
    !                                         adiabatic adjustment was
    !                                         necessary.
    !           nkuo                 output : index used by routine "conkuo"
    !                                         to accumulate number of
    !                                         points for which trouble was
    !                                         encountered while computing
    !                                         a moist adiabat.
    !
    !***********************************************************************
    !
    !            member(dimensions)        input/output
    !
    !      colrad(jmaxhf)                  input(glats)
    !      wgt   (jmaxhf)                  input(glats)
    !      eps   (mnwv1)                   input(epslon)
    !      pln   (mnwv1)                   output
    !      qln   (mnwv3)                   output
    !      del   (kmax)                    input(setsig)
    !      rpi   (kmax)                    input(setsig)
    !      rpirec(kmax)                    input(gsstcd)
    !      sl    (kmax)                    input(setsig)
    !      geshem(ncols,jmax)              input/output
    !      qlnp  (mnwv2)                   input
    !      qqm   (mnwv2,kmax)              input/output
    !      qqp   (mnwv2,kmax)              input/output
    !      qqt   (mnwv2,kmax)              input/output
    !      qtmpp (mnwv2,kmax)              input/output
    !      qtmpt (mnwv2,kmax)              output
    !      qup   (mnwv3,kmax)              output
    !      qdivt (mnwv2,kmax)              output
    !      fqn(imx,kmax)                   output
    !      fqs(imx,kmax)                   output
    !      rqn(imx,kmax)                   output
    !      rqs(imx,kmax)                   output
    !      ftn(imx,kmax)                   output
    !      fts(imx,kmax)                   output
    !      fpn(imx)                        output
    !      fps(imx)                        output
    !
    !
    !
    !     locations for available diagnostics in this subroutine
    !     
    !     total precipiation                                  ndto
    !     convective precipitation                            ndkuo
    !     large scale precipitation                           ndls
    !     snowfall                                            ndsf
    !     convective latent heating                           ndclh
    !     convective moisture change                          ndcmc
    !     large scale latent heating                          ndlslh
    !     large scale moisture change                         ndlsmc
    !     shallow convective latent heating                   ndsclh
    !     shallow convective moisture change                  ndscmc
    !     negative specific humidity correction moisture src. ndnhcr
    !
    !     
    !     qqt contains spectral qlnp ...dencies from dynamics computed in
    !     gloop and gfidi. sea surf evaporation and heating is in gfidi
    ! 
    !==========================================================================
    ! ncols......Number of grid points on a gaussian latitude circle        
    ! kmax......Number of sigma levels        
    ! nls.......Number of layers in the stratosphere.     
    ! ngaus.....ngaus=   21
    ! nkuo......index used by routine "conkuo"
    ! imx.......=ncols+1 or ncols+2   :this dimension instead of ncols
    !              is used in order to avoid bank conflict of memory
    !              access in fft computation and make it efficient. the
    !              choice of 1 or 2 depends on the number of banks and
    !              the declared type of grid variable (real*4,real*8)
    !              to be fourier transformed.
    !              cyber machine has the symptom.
    !              cray machine has no bank conflict, but the argument
    !              'imx' in subr. fft991 cannot be replaced by ncols       
    ! kmaxp.....kmaxp=kmax+1
    ! nlst......Parameter nlst   = 01    
    ! kmax-1....Parameter kmax-1 = kmax-1
    ! kmax+1....Parameter kmax+1 = kmax+1
    ! icm.......Parameter icm    = 100    
    ! daylen....Parameter daylen = 86400.0 
    ! cmb2pa....Parameter cmb2pa = 100.0
    ! rhmax.....Parameter rhmax  = 0.9999  
    ! actp......Parameter actp   = 1.7   
    ! facm......Parameter facm   = 1.0    
    ! p5........Parameter p5     = 500.0     
    ! p8........Parameter p8     = 800.0         
    ! pt8.......Parameter pt8    = 0.8  
    ! pt2.......Parameter pt2    = 0.2
    ! pfac......Parameter pfac   = pt2/(p8-p5)   
    ! cucld.....Parameter cucld  = 0.5   
    ! geshem....set aside convective precip in separate array for diagnostics 
    ! ppli......Precipitation rate ( large scale )       (mm/s)    
    ! ppci......Precipitation rate ( cumulus )           (mm/s)    
    ! rccmbl....Radiative convective cloud minimum base layer index 
    ! convc.....ncols*jmax convective cloud cover in 3 hr. avrage
    ! convt.....ncols*jmax convective cloud top  (sigma layer)  
    ! convb.....ncols*jmax convective cloud base (sigma layer) 
    ! prcp1.....  
    ! prcp2.....  
    ! prcp3.....  
    ! prcpt.....  
    ! toplv.....  
    ! botlv..... 
    ! iccon.....iccon=yes:cumulus convection(kuo)
    !                =yes  the physical process included
    !                =no   the physical process excluded
    !
    ! ilcon.....ilcon=yes:large scale condensation
    !                =yes  the physical process included
    !                =no   the physical process excluded
    ! iscon.....iscon=yes:shallow convection
    !                =yes  the physical process included
    !                =no   the physical process excluded
    ! dt........input : time step (sec). set in main
    !                    routine "smf".   
    ! kt........hour of present  time step   
    ! ktp   
    ! convts.... 
    ! convcs.... 
    ! convbs.... 
    ! doprec.... 
    ! prct......instantaneous total precipitation
    ! prcc......convective precipitation 
    ! cflric....parameter used by relaxed arakawa-schubert..data cflric /0.15/ 
    ! cp........specific heat of air           (j/kg/k)    
    ! hl........heat of evaporation of water     (j/kg)   
    ! gasr......gas constant of dry air        (j/kg/k)   
    ! g.........grav   gravity constant        (m/s**2) 
    ! rmwmd.....fracao molar entre a agua e o ar  
    ! si........si(l)=1.0-ci(l).      
    ! ci........sigma value at each level.    
    ! sl........sigma value at midpoint of
    !                                         each layer : (k=287/1005)
    !
    !                                                                     1
    !                                             +-                   + ---
    !                                             !     k+1         k+1!  k
    !                                             !si(l)   - si(l+1)   !
    !                                     sl(l) = !--------------------!
    !                                             !(k+1) (si(l)-si(l+1)!
    !                                             +-                  -+       
    ! del......sigma spacing for each layer computed in routine "setsig"   
    ! sthick...upper limit for originating air for lcl. 
    !          replaces kthick.
    ! sacum....top level for integrated moisture convergence 
    !          test. replaceskacum
    ! acum0....threshold moisture convergence such that 
    !          integrated moisture 
    ! tbase....constant tbase =  273.15e00 (K)
    ! mlrg 
    ! is.......start    i-poin
    ! latco....latitude  
    ! ki.......lowest level from which parcels can be lifted to find lcl    
    ! rqn......qn is q (specific humidit) at the n-1 time level
    ! ftn......temperature field in the spectral grid  
    ! fqn......specific humidit field in the spectral grid  
    ! fpn......surface pressure field in the spectral grid    
    ! fdqn.....distribution of moisture modification
    ! fp2457...Constant fp2457  = 0.2457
    ! fp1253...Constant fp1253  = 0.1253
    ! f0p8.....Constant f0p8    = 0.8
    ! f8p0e3...Constant f8p0e3  = 8.0e3
    ! ndkuo....convective precipitation    
    ! ndclh....snowfall  
    ! ndcmc....convective moisture change      
    ! ndsclh...shallow convective latent heating   
    ! ndscmc...shallow convective moisture change      
    ! ndto.....total precipiation        
    ! ndsf.....snowfall  
    ! ndlslh...large scale latent heating    
    ! ndlsmc...large scale moisture change
    ! ndls.....large scale precipitation         
    ! ndnhcr...negative specific humidity correction moisture src.      
    ! ngkuo....Constant ngkuo = 18
    ! ngclh....Constant ngclh = 65   
    ! ngcmc....Constant ngcmc = 69   
    ! ngsclh...Constant ngsclh= 66  
    ! ngscmc...Constant ngscmc= 70  
    ! ngto.....Constant ngto  = 17 
    ! ngsf.....Constant ngsf  = 11 
    ! nglslh...Constant nglslh= 64 
    ! nglsmc...Constant nglsmc= 68     
    ! msta
    ! dt  .....deltim=dt where dt time interval,usually =delt,but changes
    !          in nlnmi (dt=1.) and at dead start(delt/4,delt/2)
    ! rdt......rdt=one/dt
    !////////////////////////////
    !/ fac      fac=0.5e0       /
    !/ fac2     fac=0.0e0       /
    !/ fac2x    fac2=2.0e0*fac  /
    !/       fac2x=2.0e0*fac   /
    !/       fac2x=2.0e0       /
    !////////////////////////////
    !==========================================================================
    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    REAL,    INTENT(inout) :: geshem (ncols)
    REAL,    INTENT(out  ) :: ppli   (ncols)
    REAL,    INTENT(inout) :: ppci   (ncols)
    REAL,    INTENT(inout) :: convc  (ncols)
    REAL,    INTENT(inout) :: convt  (ncols)
    REAL,    INTENT(inout) :: convb  (ncols)
    REAL,    INTENT(inout) :: prcp1  (ncols)
    REAL,    INTENT(inout) :: prcp2  (ncols)
    REAL,    INTENT(inout) :: prcp3  (ncols)
    REAL,    INTENT(inout) :: prcpt  (ncols)
    REAL,    INTENT(inout) :: toplv  (ncols)
    REAL,    INTENT(inout) :: botlv  (ncols)
    REAL,    INTENT(out  ) :: convts (ncols)
    REAL,    INTENT(out  ) :: convcs (ncols)
    REAL,    INTENT(out  ) :: convbs (ncols)
    REAL,    INTENT(out  ) :: prct   (ncols)
    REAL,    INTENT(out  ) :: prcc   (ncols)

    REAL,    INTENT(in   ) :: si     (kmax+1)
    REAL,    INTENT(in   ) :: sl     (kmax)
    REAL,    INTENT(in   ) :: del    (kmax)
    INTEGER, INTENT(in   ) :: latco
    REAL,    INTENT(in   ) :: rqn    (ncols,kmax)
    REAL,    INTENT(inout) :: ftn    (ncols,kmax)
    REAL,    INTENT(inout) :: fqn    (ncols,kmax)
    REAL,    INTENT(in   ) :: fpn    (ncols)
    REAL,    INTENT(in   ) :: wtn    (ncols)
    REAL,    INTENT(in   ) :: wqn    (ncols)
    REAL,    INTENT(in   ) :: uun    (ncols,kmax)
    REAL,    INTENT(in   ) :: vvn    (ncols,kmax)
    REAL,    INTENT(in   ) :: omgn   (ncols,kmax) 

    REAL    :: fdqn (ncols,kmax)
    INTEGER :: msta
    REAL    :: rdt
    REAL    :: fac
    REAL    :: fac2
    REAL    :: fac2x  

    REAL    :: gwork(ncols,kmax)
    REAL    :: dtwrk(ncols,kmax)
    REAL    :: dqwrk(ncols,kmax)
    REAL    :: rrr  (ncols)
    REAL    :: plcl (ncols)
    INTEGER :: kbot (ncols)
    INTEGER :: ktop (ncols)
    INTEGER :: kuo  (ncols)
    !
    !     these arrays are needed for the heating and mostening from
    !     ras  scheme
    !
    REAL    :: hrem(ncols,kmax)
    REAL    :: qrem(ncols,kmax)
    !
    !     these arrays are needed for the new shallow convection scheme
    !

    INTEGER :: ktops(ncols)
    INTEGER :: kbots(ncols)
    !
    !     needed for cloud fraction based on mass flux
    !
    REAL    :: cldm(ncols)
    !     
    !     needed for cloud fraction based on mass flux
    !     new arrays needed for shallow convective clouds
    !     
    INTEGER :: noshal1(ncols)
    INTEGER :: kctop1 (ncols)
    INTEGER :: kcbot1 (ncols)
    REAL    :: topo   (ncols)

    REAL    :: bfrg   (ncols)
    REAL    :: bfr1   (ncols)
    REAL    :: bfr2   (ncols)
    REAL    :: bfr3   (ncols)

    REAL   , PARAMETER :: fp2457 = 0.2457
    REAL   , PARAMETER :: fp1253 = 0.1253
    REAL   , PARAMETER :: f0p8   = 0.8
    REAL   , PARAMETER :: f8p0e3 = 8.0e3

    !    INTEGER, PARAMETER :: ndkuo  = 10! convective precipitation
    !    INTEGER, PARAMETER :: ndclh  = 33! convective latent heating
    !    INTEGER, PARAMETER :: ndcmc  = 34! convective moisture change
    !    INTEGER, PARAMETER :: ndsclh = 37! shallow convective latent heating
    !    INTEGER, PARAMETER :: ndscmc = 38! shallow convective moisture change
    !    INTEGER, PARAMETER :: ndto   =  9! total precipiation
    !    INTEGER, PARAMETER :: ndsf   = 12! snowfall
    !    INTEGER, PARAMETER :: ndlslh = 35! large scale latent heating
    !    INTEGER, PARAMETER :: ndlsmc = 36! large scale moisture change
    !    INTEGER, PARAMETER :: ndls   = 11! large scale precipitation
    !    INTEGER, PARAMETER :: ndnhcr = 70! negative specific humidity correction moisture src.


    INTEGER, PARAMETER :: nlst  = 01    
    INTEGER, PARAMETER :: icm   = 100     
    REAL,    PARAMETER :: daylen= 86400.0  
    REAL,    PARAMETER :: cmb2pa= 100.0
    REAL,    PARAMETER :: rhmax = 0.9999  
    REAL,    PARAMETER :: actp  = 1.7
    REAL,    PARAMETER :: facm  = 1.0  
    REAL,    PARAMETER :: p5    = 500.0  
    REAL,    PARAMETER :: p8    = 800.0 
    REAL,    PARAMETER :: pt8   = 0.8 
    REAL,    PARAMETER :: pt2   = 0.2
    REAL,    PARAMETER :: pfac  = pt2/(p8-p5)
    REAL,    PARAMETER :: cucld = 0.5

    INTEGER :: i
    INTEGER :: IJK
    INTEGER :: k 
    INTEGER :: nkuo = 0  
    INTEGER :: imx 
    LOGICAL :: ghl_local

    ghl_local = IsGridHistoryOn()
    rdt=1.0/dt
    msta=0
    fac=0.5
    imx  = ncols + 2
    IF(ifilt.EQ.0.AND.kt.EQ.0.AND.jdt.EQ.1) fac=0.0
    fac2=2.0*fac
    fac2x=2.0*fac
    IF(ifilt.EQ.0.AND.kt.EQ.0.AND.jdt.EQ.2) fac2x=2.0

    CALL qnegat(fqn,fdqn ,ftn,&
         rdt,ndnhcr,del   ,ncols  ,kmax  )


    IF(dodia(ndnhcr))CALL updia(fdqn,ndnhcr,latco)

    rrr=0.0 !  call reset(rrr,ncols)

    IF(iccon.EQ.'ARA '.OR.iccon.EQ.'YES '.OR.iccon.EQ.'KUO '& 
         .OR.iccon.EQ.'GRE')THEN
       DO k=1,kmax
          DO i=1,ncols
             dtwrk(i,k)=ftn(i,k)
          END DO
       END DO

       DO k=1,kmax
          DO i=1,ncols
             dqwrk(i,k)=fqn(i,k)
          END DO
       END DO

       DO i=1,ncols
          ktop(i)=-1000
          ktops(i)=-1000
          kuo (i)=-1000
          plcl(i)=-1.0e3
       END DO

       IF(iccon.EQ.'ARA ') THEN

          CALL arprep( &
               dtwrk ,dqwrk ,sl    ,si    ,fpn   ,ktop  ,kbot  ,rrr   , &
               hrem  ,qrem  ,dt    ,ftn   ,fqn   ,del   ,kuo   ,cldm  , &
               cflric,kmax+1,kmax-1,ncols ,kmax  ,nls)

          CALL shllcl(dt    ,fpn   ,sl    ,si    ,fqn  ,ftn   ,msta  ,kuo   , &
               plcl  ,ktops ,kbots ,ncols ,kmax  )

       END IF

       IF(iccon.EQ.'YES '.OR.iccon.EQ.'KUO ') THEN

          CALL kuolcl(dt    ,nkuo  ,fpn   ,del   ,sl    ,si    ,rqn   ,fqn   ,ftn   ,&
               gwork ,rrr   ,msta  ,kuo   ,plcl  ,ktop  ,kbot  ,ncols  ,&
               kmax  )

       END IF
       !eps
       !grell
       !          
       IF(iccon.EQ.'GRE' )THEN
          DO i=1,ncols
             !
             !eps topo is supposed to be the topography. However, I couldn't find it.
             !    This must be set out in order to the scheme can work alright
             !
             topo(i)=10.
          END DO
          CALL cupgrell(fpn,sl,dt,uun,vvn,omgn,ftn,fqn,rqn,rrr,ktop,kbot,ncols,kmax)
       END IF
       !     
       !     calculate convective cloud cover from convective precipitation
       !     
       IF(iccon.EQ.'YES '.OR.iccon.EQ.'KUO '.OR.iccon.EQ.'GRE') THEN
          IF (kt .NE. ktp) THEN
             DO IJK=1,ncols
                convc(IJK) = 0.0   ! call reset(convc(1),ncols)
                convt(IJK) = 0.0   ! call reset(convt(1),ncols)
                convb(IJK) = 0.0   ! call reset(convb(1),ncols)
             ENDDO
             DO i = 1, ncols
                prcpt(i) = prcpt(i) - prcp1(i) &
                     + prcp3(i)
             END DO
             DO i = 1, ncols
                IF (prcpt(i) .GT. 0.0e0) THEN
                   convc(i) = fp2457 + fp1253 * &
                        LOG(prcpt(i) * f8p0e3)
                   convc(i) = MAX(convc(i), 0.0e0)
                   convc(i) = MIN(convc(i), f0p8)
                END IF
             END DO
             DO i = 1, ncols
                IF (prcp3(i) .GT. 0.0e0) THEN
                   convt(i)=toplv(i) / prcp3(i)
                   convb(i)=botlv(i) / prcp3(i)
                END IF
             END DO
             DO i = 1, ncols
                convb(i) = MAX(convb(i),rccmbl)
                IF (convb(i) .GT. convt(i)) &
                     convb(i) = convt(i)
             END DO
             DO i = 1, ncols
                prcp1(i) = prcp2(i)
                prcp2(i) = prcp3(i)
             END DO
             DO IJK=1,ncols
                prcp3(IJK) = 0.0   !call reset(prcp3(1),ncols)
                toplv(IJK) = 0.0   !call reset(toplv(1),ncols)
                botlv(IJK) = 0.0   !call reset(botlv(1),ncols)
             ENDDO
          END IF
          DO i = 1, ncols
             IF (rrr(i) .GT. 0.) THEN
                prcp3(i) = prcp3(i) + fac2x * rrr(i)
                toplv(i) = toplv(i) +  &
                     fac2x * rrr(i) * ktop(i)
                botlv(i) = botlv(i) +  &
                     fac2x * rrr(i) * kbot(i)
             END IF
          END DO
       END IF
       IF(iccon.EQ.'ARA ')THEN
          IF (kt .NE. ktp) THEN
             DO IJK=1,ncols
                convc(IJK) = 0.0   ! call reset(convc(1),ncols)
                convt(IJK) = 0.0   ! call reset(convt(1),ncols)
                convb(IJK) = 0.0   ! call reset(convb(1),ncols)
             ENDDO
             DO i = 1, ncols
                prcpt(i) = prcpt(i) - prcp1(i) &
                     + prcp3(i)
             END DO
             DO i = 1, ncols
                IF (prcpt(i) .GT. 0.0e0) THEN
                   convc(i) = 0.2+.038*prcpt(i)*23000.
                   convc(i) = MAX(convc(i), 0.0e0)
                   convc(i) = MIN(convc(i), f0p8)
                END IF
             END DO
             DO i = 1, ncols
                IF (prcp3(i) .GT. 0.0e0) THEN
                   convt(i) = toplv(i)/prcp3(i)
                   convb(i) = botlv(i)/prcp3(i)
                END IF
             END DO
             DO i = 1, ncols
                convb(i) = MAX(convb(i),rccmbl)
                IF (convb(i) .GT. convt(i)) &
                     convb(i) = convt(i)
             END DO
             DO i = 1, ncols
                prcp1(i) = prcp2(i)
                prcp2(i) = prcp3(i)
             END DO

             DO IJK=1,ncols
                prcp3(IJK) = 0.0   !   call reset(prcp3(1),ncols)
                toplv(IJK) = 0.0   !   call reset(toplv(1),ncols)
                botlv(IJK) = 0.0   !   call reset(botlv(1),ncols)
             ENDDO

          END IF
          DO i = 1, ncols
             IF (rrr(i) .GT. 0.) THEN
                prcp3(i) = prcp3(i) + fac2x * rrr(i)
                toplv(i) = toplv(i) + fac2x * rrr(i)  &
                     * ktop(i)
                botlv(i) = botlv(i) + fac2x * rrr(i)  &
                     * kbot(i)
             END IF
          END DO
       ENDIF
       !     
       !     end of convective cloud computation
       !     
       DO i=1,ncols
          ppci  (i)=2.0e0*1.0e3*rrr(i)
          IF(dodia(ndkuo))bfr1(i)=fac2*rdt*1.0e3*rrr(i)
       END DO
       IF (ghl_local) THEN
          IF (dogrh(ngkuo,latco)) CALL StoreGridHistory (rrr, ngkuo, latco, fac2*rdt*1.0e3)
       END IF

       IF(dodia(ndkuo))CALL updia(bfr1,ndkuo,latco)


       DO k=1,kmax
          DO i=1,ncols
             dtwrk(i,k)=fac*rdt*(ftn(i,k)-dtwrk(i,k))
          END DO
       END DO

       DO k=1,kmax
          DO i=1,ncols
             dqwrk(i,k)=fac*rdt*(fqn(i,k)-dqwrk(i,k))
          END DO
       END DO

       IF(ghl_local)THEN
          IF(dogrh(ngclh,latco))CALL StoreGridHistory(dtwrk, ngclh, latco)
          IF(dogrh(ngcmc,latco))CALL StoreGridHistory(dqwrk, ngcmc, latco)
       END IF
       IF(dodia(ndclh))CALL updia(dtwrk,ndclh,latco)


       IF(dodia(ndcmc))CALL updia(dqwrk,ndcmc,latco)


    END IF
    IF (doprec) THEN
       DO i=1,ncols
          prcc(i)=fac2*rdt*1.0e3*rrr(i)
       END DO
    END IF
    !
    !eps
    !
    IF(iscon.EQ.'SOUZ')THEN
       !dtca=9.0D2
       !jdte=jdt
       !
       DO i=1,ncols*kmax
          dtwrk(i,1)=ftn(i,1)
          dqwrk(i,1)=fqn(i,1)
       END DO
       CALL shcuso(ftn,fqn,fpn,sl,wtn,wqn,dt,ncols,kmax)
    END IF
    !
    IF(iscon.EQ.'YES '.OR.iscon.EQ.'TIED')THEN
       DO k=1,kmax
          DO i=1,ncols
             dtwrk(i,k)=ftn(i,k)
             dqwrk(i,k)=fqn(i,k)
          END DO
       END DO

       IF(iccon.EQ.'KUO '.OR.iccon.EQ.'YES ') THEN

          CALL shalv2( &
               si    ,sl    ,ftn   ,fqn   ,fpn   ,dt     , &
               ktop  ,plcl  ,kuo   ,kmax+1 ,kctop1,kcbot1,noshal1, &
               .FALSE.,ncols  ,kmax   )

       ELSE IF(iccon.EQ.'ARA ') THEN

          CALL shalv2( &
               si    ,sl    ,ftn   ,fqn   ,fpn   ,dt     , &
               ktops ,plcl  ,kuo   ,kmax+1 ,kctop1,kcbot1,noshal1, &
               .TRUE.,ncols  ,kmax)

          DO IJK=1,ncols
             convts(IJK) = 0.0   ! call reset(convts(1),ncols)
             convbs(IJK) = 0.0   ! call reset(convbs(1),ncols)
             convcs(IJK) = 0.0   ! call reset(convcs(1),ncols)
          ENDDO

          DO is=1,ncols
             IF(noshal1(is).EQ.0) THEN
                convts(is)=kctop1(is)
                convbs(is)=kcbot1(is)
                !     
                !     for mass flux   convcs(is)=0.5
                !     
                convcs(is)= 0.3
             ENDIF
          END DO
       ENDIF

       DO k=1,kmax
          DO i=1,ncols
             dtwrk(i,k)=fac*rdt*(ftn(i,k)-dtwrk(i,k))
             dqwrk(i,k)=fac*rdt*(fqn(i,k)-dqwrk(i,k))
          END DO
       END DO
       IF(ghl_local)THEN
          IF(dogrh(ngsclh,latco))CALL StoreGridHistory(dtwrk, ngsclh, latco)
          IF(dogrh(ngscmc,latco))CALL StoreGridHistory(dqwrk, ngscmc, latco)
       END IF
       IF(dodia(ndsclh))CALL updia(dtwrk,ndsclh,latco)


       IF(dodia(ndscmc))CALL updia(dqwrk,ndscmc,latco)


    END IF
    !     
    !     gdivn,gtmpn,grotn,gun,gvn are temporary working space
    !     
    IF(ilcon.EQ.'YES ') THEN
       DO k=1,kmax
          DO i=1,ncols
             dtwrk(i,k)=ftn(i,k)
             dqwrk(i,k)=fqn(i,k)
          END DO
       END DO

       IF(dodia(ndls))THEN
          DO i=1,ncols
             bfr3(i)=rrr(i)
          END DO
       END IF
       CALL lrgscl(rrr, ftn, gwork, fqn, fpn, del, sl, dt, &
            mlrg, latco, ncols, kmax)
       DO i=1,ncols
          ppli  (i)=2.0e0*1.0e3*rrr(i)-ppci(i)
          geshem(i)=geshem(i)+fac2x*rrr(i)
          IF(dodia(ndto))bfr1(i)=fac2*rdt*1.0e3*rrr(i)
          IF(dodia(ndls))bfr3(i)=fac2*rdt*1.0e3*(rrr(i)-bfr3(i))
          IF(ghl_local.AND.dogrh(ngsf,latco))bfrg(i)=0.0e0
          IF(dodia(ndsf))bfr2(i)=0.0e0
          IF(0.35*ftn(i,1)+0.65*ftn(i,2).LE.273.2)THEN
             IF(ghl_local.AND.dogrh(ngsf,latco))bfrg(i)=rrr(i)
             IF(dodia(ndsf))bfr2(i)=fac2*rdt*1.0e3*rrr(i)
          ENDIF
       END DO
       IF(ghl_local)THEN
          IF(dogrh(ngto,latco))CALL StoreGridHistory(rrr,  ngto, latco, fac2*rdt*1.0e3)    
          IF(dogrh(ngsf,latco))CALL StoreGridHistory(bfrg, ngsf, latco, fac2*rdt*1.0e3)
       ENDIF
       IF(dodia(ndto))CALL updia(bfr1,ndto,latco)


       IF(dodia(ndsf))CALL updia(bfr2,ndsf,latco)


       IF(dodia(ndls))CALL updia(bfr3,ndls,latco)


       DO k=1,kmax
          DO i=1,ncols
             dtwrk(i,k)=fac*rdt*(ftn(i,k)-dtwrk(i,k))
             dqwrk(i,k)=fac*rdt*(fqn(i,k)-dqwrk(i,k))
          END DO
       END DO
       IF(ghl_local)THEN
          IF(dogrh(nglslh,latco))CALL StoreGridHistory(dtwrk, nglslh, latco)
          IF(dogrh(nglsmc,latco))CALL StoreGridHistory(dqwrk, nglsmc, latco)
       END IF
       IF(dodia(ndlslh))CALL updia(dtwrk,ndlslh,latco)


       IF(dodia(ndlsmc))CALL updia(dqwrk,ndlsmc,latco)


    END IF
    IF (doprec) THEN
       DO i=1,ncols
          prct(i)=fac2*rdt*1.0e3*rrr(i)
       END DO
    END IF
    DO k=1,kmax
       DO i=1,ncols
          ftn(i,k) = ftn(i,k) * (1.0 + 0.608e0*fqn(i,k))
       END DO
    END DO
  END SUBROUTINE gwater2





  SUBROUTINE cupgrell(ps,sl,dtime,ua,va,omega,te,qe,rtt,conprr,kctop,kcbot,&
       imax,kmax)
    !
    !
    INTEGER, INTENT(IN   ) :: imax
    INTEGER, INTENT(IN   ) :: kmax
    INTEGER, PARAMETER     :: jmax=1
    INTEGER maxiens,maxens,maxens2,maxens3,ensdim
    PARAMETER (maxiens=1) !  outer outer one
    PARAMETER (maxens =3) !3  ensemble one on mbdt
    PARAMETER (maxens2=3) !3 ensemble two on precip efficiency
    PARAMETER (maxens3=12) !12  ensemble three done in cup_forcing
    !  this is the lowest level, then comes ensemble 1!
    PARAMETER (ensdim=maxiens*maxens*maxens2*maxens3)
    !     
    REAL, PARAMETER ::                     &
         rgas     =  287.             & 
         ,   cp       = 1004.             &
         ,   rm       =  461.              &   
         ,   p00      = 1.e5                & 
         ,   tcrit    =  273.15             &
         ,   g        =    9.806          & 
         ,   cpor     = cp / rgas       &
         ,   pkdcut   =   75.             
    !------------------------------------ vectors: 
    INTEGER j1,j2
    INTEGER, SAVE :: ialloc=0
    !
    REAL,    INTENT(IN   ) :: ps(imax)          ! surface pressure (cb)
    REAL,    INTENT(IN   ) :: sl(kmax)         ! sigma layers
    REAL,    INTENT(IN   ) :: dtime            ! time step (s)
    REAL,    INTENT(IN   ) :: ua(imax,kmax)    ! zonal wind (m/s)
    REAL,    INTENT(IN   ) :: va(imax,kmax)    ! meridional wind (m/s)
    REAL,    INTENT(IN   ) :: omega(imax,kmax) ! vertical p velocity (kPa/s)
    REAL,    INTENT(INOUT) :: te(imax,kmax)     ! air temperature (K)
    REAL,    INTENT(INOUT) :: qe(imax,kmax)     ! specific humidity (kg/kg)
    REAL,    INTENT(IN   ) :: rtt(imax,kmax)    ! specific humidity of previous t-step (kg/kg)
    REAL,    INTENT(  OUT) :: conprr(imax)     ! convective precipitation (mm)
    INTEGER, INTENT(  OUT) :: kctop(imax)      ! level of convective cloud top
    INTEGER, INTENT(  OUT) :: kcbot(imax)      ! level of convective cloud base
    !
    !----------------------------------- local variables :
    !
    INTEGER istart,iend,i,j,k,mix,mjx,mkx
    REAL vspeed,dp,dq,cpdtdt
    INTEGER, SAVE, ALLOCATABLE :: kdet(:),kdt(:)                    &
         ,iact_gr(:),iact_old_gr(:),iktop(:),ikbot(:)
    !
    REAL, SAVE, ALLOCATABLE :: mconv(:),direction(:)
    REAL, SAVE, ALLOCATABLE :: umean(:),vmean(:),pmean(:)
    REAL, SAVE, ALLOCATABLE :: t(:,:),q(:,:),omeg(:,:),p(:,:),po(:,:)        &
         ,tn(:,:),qo(:,:),aa0(:)                                 &
         ,outt(:,:),outq(:,:),outqc(:,:)                              &
         ,pret(:),psur(:),ter11(:)                                   &
         ,us(:,:), vs(:,:),xland(:)
    REAL, SAVE, ALLOCATABLE :: precip(:),massflx(:),xierr(:,:)
    REAL, SAVE, ALLOCATABLE :: massfln(:,:)
    !
    !--- allocating memory
    !
    IF(ialloc == 0)  THEN
       !
       ALLOCATE(massfln(imax,ensdim))
       !      
       ALLOCATE(                                     &
            mconv(imax),umean(imax),vmean(imax)                    &
            ,pmean(imax),direction(imax)                       &
            )
       !     
       ALLOCATE(                                      &
            t(imax,kmax),q(imax,kmax),p(imax,kmax)               &
            ,po(imax,kmax),tn(imax,kmax),qo(imax,kmax)                &
            ,outt(imax,kmax),outq(imax,kmax),outqc(imax,kmax)       &
            ,pret(imax),psur(imax),ter11(imax)                   &
            ,us(imax,kmax), vs(imax,kmax),omeg(imax,kmax)       &
            ,aa0(imax),iktop(imax),ikbot(imax)                   &
            )
       !
       ALLOCATE( kdet(imax),kdt(imax) )
       !
       ALLOCATE(                                     &
            xland(imax),precip(imax)                           &
            ,massflx(imax),xierr(imax,jmax)                      &
            ,iact_gr(imax),iact_old_gr(imax)                       &
            )
       !
    END IF
    !
    !----------------------------------------------------------------------
    istart = 1
    iend   = imax
    j1     = 1
    j2     = 1
    mkx    = kmax         
    mix    = imax          
    mjx    = jmax          
    !
    ! loop in - j - (begining) 
    !
    DO j=j1,j2
       !
       DO  i = istart,iend
          aa0(i)=0.
          xland(i) = 0. ! land/water flag - not in use
       END DO
       !
       DO i = istart,iend
          iact_gr(i)      = 0
          iact_old_gr(i)  = 0
          kdt(i)        = 0
          precip(i)        = 0.
          massflx(i)      = 0.
       END DO
       !
       !--- prepare input, erase output
       !
       DO i = istart,iend        
          kdet(i)  =2
          pret(i)  =0.
          mconv(i) =0.
          umean(i) =0.
          vmean(i) =0.
          pmean(i) =0.
          ter11(i)=29.25*te(i,1)*(101.3-ps(i))/ps(i)
          ter11(i)=MAX(0.,ter11(i))
       END DO
       !
       !     prepare values from the GCM to be used by the scheme 
       !
       DO k = 1, mkx
          DO i = istart,iend
             !
             !ter11(i)= ht(i)
             psur(i) = ps(i)*10.
             po(i,k) = ps(i)*sl(k)*10.               ! pressure in mbar
             us(i,k) = ua(i,k) 
             vs(i,k) = va(i,k) 
             omeg(i,k)   = omega(i,k)*1000.          ! omeg in Pa/s
             !
             t(i,k)  = te(i,k)
             q(i,k)  = qe(i,k)
             !
             cpdtdt=0.
             !
             !        temperature and moisture if there is no convection
             !
             tn(i,k) = t(i,k) + ( cpdtdt/cp )*dtime
             !         
             qo(i,k) = q(i,k) + (qe(i,k)-rtt(i,k))*dtime
             !
             p(i,k)  = po(i,k)
             IF((psur(i)-p(i,k)).GT.150.AND.p(i,k).GT.300.)THEN   
                dp       = -.5*(p(i,k+1)-p(i,k-1))        
                umean(i) = umean(i)+us(i,k)*dp 
                vmean(i) = vmean(i)+vs(i,k)*dp
                pmean(i) = pmean(i)+dp
             END IF
             !IF(i.EQ.25)THEN
             !  PRINT*,'p,u,v,w',p(i,k),us(i,k),vs(i,k),omeg(i,k)
             !END IF
             !
             IF(tn(i,k).LT.200.)    tn(i,k) = t(i,k)
             IF(qo(i,k).LT.1.e-08)  qo(i,k) = 1.e-08
             !
             outt(i,k) = 0.   !tendency of temperature associated with cumulus
             outq(i,k) = 0.   !tendency of water vapor associated with cumulus
             outqc(i,k) = 0.  !tendency of liquid water or ice associated with cumulus
          END DO
       END DO
       !
       DO i = istart,iend
          umean(i)=umean(i)/pmean(i)
          vmean(i)=vmean(i)/pmean(i)
          !IF(umean(i).EQ.0.)umean(i)=1.
          !IF(vmean(i).EQ.0.)vmean(i)=1.
          vspeed=SQRT(umean(i)*umean(i)+vmean(i)*vmean(i))
          direction(i)=(ATAN2(umean(i),vmean(i))+3.1415926)*57.29578
          IF(direction(i).GT.360.)direction(i)=direction(i)-360.
          IF(vspeed.LT.5.)direction(i)=9999.
       END DO
       !
       DO k=2,mkx-1
          DO i = istart,iend
             dq=.5*(q(i,k+1)-q(i,k-1))
             mconv(i)=mconv(i) + omeg(i,k)*dq/g   !moisture convergence (omeg in Pa/s)
          END DO
       END DO
       !
       !---  cumulus parameterization
       !
       CALL cup_enss(imax,1,kmax                              &
            ,ialloc,maxens,maxens2,maxens3,ensdim            &
            ,1,istart,iend,mix,1,mkx,massfln,massflx                   &
            ,iact_old_gr,xierr,ter11                  &
            ,aa0,t,q,tn,qo,po,pret,p,outt                        &
            ,outq,outqc,dtime,psur,us,vs,kdet,tcrit,1                  &
            ,mconv,omeg,direction,iktop,ikbot)
       !
       !--- output
       !
       DO k=1,mkx
          DO i = istart,iend
             te(i,k) = t(i,k) + outt(i,k)*dtime 
             qe(i,k) = q(i,k) + (outq(i,k) + outqc(i,k))*dtime
          END DO
       END DO
       !
       DO i = istart,iend
          precip(i) = pret(i)
          conprr(i) = precip(i)/2
          kctop(i)  = iktop(i)
          kcbot(i)  = ikbot(i)
       END DO
       !
       DO i = istart,iend
          IF(precip(i).LE.0.)THEN
             iact_gr(i) =0
             precip(i)  =0.
             conprr(i)=0.
             DO k=1,mkx
                te(i,k) = t(i,k)
                qe(i,k) = q(i,k)
             END DO
          END IF
       END DO
       !
    END DO
    !
    ! loop - j - (end)
    !
    !
    RETURN
  END SUBROUTINE cupgrell





  SUBROUTINE cup_enss(imax,jmax,kmax                  &
       ,ialloc,maxens,maxens2,maxens3,ensdim         &
       ,j,istart,iend,mix,mjx,mkx                     &
       ,massfln,massflx,iact_old_gr,xierr         &
       ,z1                                 &
       ,aaeq,t,q,tn,qo,po,pre,p,outt,outq,outqc,dtime      &
       ,psur,us,vs,kdet,tcrit,iens                       &
       ,mconv,omeg,direction,ktop,kbcon)
    !
    INTEGER maxens,maxens2,maxens3,ensdim
    INTEGER mix,mjx,mkx,imax, jmax, kmax
    INTEGER nall,nens,iens,iedt
    INTEGER nens3
    INTEGER ialloc,izero
    !
    !--- input variables -----------------------------
    !
    REAL mconv(mix),z1(mix),direction(mix),aaeq(mix)               &
         ,pre(mix),psur(mix)
    REAL t(mix,mkx),q(mix,mkx),tn(mix,mkx),qo(mix,mkx)         &
         ,p(mix,mkx),po(mix,mkx)                         &
         ,us(mix,mkx),vs(mix,mkx),omeg(mix,mkx)
    !
    REAL massfln(mix,ensdim)
    REAL massflx(mix)
    INTEGER iact_old_gr(mix),                      &
         kdet(mix),ktop(mix),kbcon(mix)
    !
    !--- work variables - allocatable in this point --
    !
    INTEGER fquasi,fstab,fmconv,iresult
    INTEGER ki
    INTEGER i,j,k,istart,iend
    !
    INTEGER, SAVE, ALLOCATABLE :: kzdown(:)      &
         ,kbmax(:),ierr(:)                      &
         ,k22(:),kb(:),jmin(:)                      &
         ,kstabi(:),kstabm(:)                    &
         ,k22x(:),kbconx(:),kbx(:),ktopx(:)
    !
    REAL, SAVE, ALLOCATABLE :: he(:,:),hes(:,:),qes(:,:)       &
         ,z(:,:),tv(:,:),dby(:,:)                           &
         ,qc(:,:),qrcd(:,:),pwd(:,:)                            &
         ,pw(:,:),edt(:),edto(:),edtx(:)                   &
         ,aa1(:),aa0(:),xaa0(:),hkb(:)                          &
         ,hkbo(:),aad(:)                                 &
         ,xhkb(:),qkb(:),qkbo(:),xmb(:)
    !
    REAL, SAVE, ALLOCATABLE :: heo(:,:),heso(:,:),qeso(:,:)       &
         ,zo(:,:),tvo(:,:),dbyo(:,:)                        &
         ,qco(:,:),qrcdo(:,:),pwdo(:,:)                            &
         ,pwo(:,:)                                    &
         ,xpwav(:),xpwev(:),pwav(:),pwev(:)                  &
         ,pwavo(:),pwevo(:),bu(:),buo(:)
    !
    REAL, SAVE, ALLOCATABLE :: xhe(:,:),xhes(:,:),xqes(:,:)      &
         ,xz(:,:),xtv(:,:),xt(:,:)                         &
         ,xq(:,:),xdby(:,:),xqc(:,:)                          &
         ,xqrcd(:,:),xpwd(:,:),xpw(:,:)
    REAL, SAVE, ALLOCATABLE :: hcd(:,:),hcdo(:,:),xhcd(:,:)
    REAL, SAVE, ALLOCATABLE :: qcd(:,:),qcdo(:,:),xqcd(:,:)
    REAL, SAVE, ALLOCATABLE :: dbyd(:,:),dbydo(:,:)
    REAL, SAVE, ALLOCATABLE :: hc(:,:),hco(:,:),xhc(:,:)        &
         ,qrc(:,:),qrco(:,:),xqrc(:,:)                    &
         ,zu(:,:),zuo(:,:),xzu(:,:)                        &
         ,zd(:,:),zdo(:,:),xzd(:,:)             
    REAL, SAVE, ALLOCATABLE :: dellah(:,:),dellaq(:,:),dellat(:,:) &
         ,dellaqc(:,:)
    REAL mbdt
    !
    !--- output variables ----------------------------
    !
    REAL outqc(mix,mkx),outt(mix,mkx),outq(mix,mkx)                   &
         ,xierr(mix,mjx)
    !
    REAL, SAVE, ALLOCATABLE :: dellat_ens(:,:,:),dellaq_ens(:,:,:)        &
         ,dellaqc_ens(:,:,:),pwo_ens(:,:,:)
    !
    !--- variables on cloud levels ------------------------------------
    !
    REAL, SAVE, ALLOCATABLE :: qes_cup(:,:),q_cup(:,:),he_cup(:,:)        &
         ,hes_cup(:,:),z_cup(:,:),p_cup(:,:)                         &
         ,gamma_cup(:,:),t_cup(:,:)
    !
    REAL, SAVE, ALLOCATABLE :: qeso_cup(:,:),qo_cup(:,:)             &
         ,heo_cup(:,:),heso_cup(:,:)                               &
         ,zo_cup(:,:),po_cup(:,:)                               &
         ,gammao_cup(:,:),tn_cup(:,:)
    !
    REAL, SAVE, ALLOCATABLE :: xqes_cup(:,:),xq_cup(:,:),xhe_cup(:,:)      &
         ,xhes_cup(:,:),xz_cup(:,:),xt_cup(:,:)
    !
    REAL day,dz,tcrit,dtime
    !srf_tmp
    !srf_tmp
    !
    !--- new entrainment/detrainment related stuff --------------------
    !
    REAL mentr_rate,mentrd_rate,entr_rate,radius,             &
         massfld,zcutdown,edtmax,edtmin,depth_min,                  &
         zkbmax,z_detr,zktop,dh,cap_maxs
    !
    REAL, SAVE, ALLOCATABLE :: cd(:,:),cdd(:,:),scr1(:,:)               &
         ,cap_max(:),mbdt_ens(:)                                 &
         ,xaa0_ens(:,:),edt_ens(:),edtc(:,:)
    !
    REAL, SAVE, ALLOCATABLE :: cwf(:,:),pwf(:,:)                   &
         ,pwdf(:,:),eddt(:,:),predb(:,:)                          &
         ,xktop(:,:),xkbas(:,:),xmass(:,:)                          &
         ,xf(:,:)
    !
    REAL, SAVE, ALLOCATABLE :: xf_ens(:,:),pr_ens(:,:)               &
         ,outt_ens(:,:)
    !
    !srf variaveis para a rotine "cup_dd_edt"
    REAL, SAVE, ALLOCATABLE :: vshear(:),sdp(:),vws(:)
    !
    !----------------------------allocate memory -----
    IF(ialloc == 0)  THEN              
       !     
       ialloc = 1
       !
       ALLOCATE(                                      &
            kzdown(imax),kbmax(imax),ierr(imax)               &
            ,k22(imax),kb(imax),jmin(imax)                         &
            ,kstabi(imax),kstabm(imax)                          &
            ,k22x(imax),kbconx(imax),kbx(imax),ktopx(imax)        &
            )
       !
       ALLOCATE(                                      &
            he(imax,kmax),hes(imax,kmax),qes(imax,kmax)           &
            ,z(imax,kmax),tv(imax,kmax),dby(imax,kmax)              &
            ,qc(imax,kmax),qrcd(imax,kmax),pwd(imax,kmax)        &
            ,pw(imax,kmax),edt(imax),edto(imax),edtx(imax)        &
            ,aa1(imax),aa0(imax),xaa0(imax),hkb(imax)              &
            ,hkbo(imax),aad(imax)                          &
            ,xhkb(imax),qkb(imax),qkbo(imax),xmb(imax)              &
            ,heo(imax,kmax),heso(imax,kmax),qeso(imax,kmax)        &
            ,zo(imax,kmax),tvo(imax,kmax),dbyo(imax,kmax)        &
            ,qco(imax,kmax),qrcdo(imax,kmax),pwdo(imax,kmax)      &
            ,pwo(imax,kmax)                                &
            ,xpwav(imax),xpwev(imax),pwav(imax),pwev(imax)           &
            ,pwavo(imax),pwevo(imax)                          &
            ,bu(imax),buo(imax)                                  &
            )
       !
       ALLOCATE(                                    &
            xhe(imax,kmax),xhes(imax,kmax),xqes(imax,kmax)      &
            ,xz(imax,kmax),xtv(imax,kmax),xt(imax,kmax)             &
            ,xq(imax,kmax),xqc(imax,kmax),xqrcd(imax,kmax)      &
            ,xdby(imax,kmax),xpwd(imax,kmax),xpw(imax,kmax)      &
            ,hcd(imax,kmax),hcdo(imax,kmax),xhcd(imax,kmax)      &
            ,qcd(imax,kmax),qcdo(imax,kmax),xqcd(imax,kmax)      &
            ,dbyd(imax,kmax),dbydo(imax,kmax)                  &
            ,hc(imax,kmax),hco(imax,kmax),xhc(imax,kmax)      &
            ,qrc(imax,kmax),qrco(imax,kmax),xqrc(imax,kmax)      &
            ,zu(imax,kmax),zuo(imax,kmax),xzu(imax,kmax)      &
            ,zd(imax,kmax),zdo(imax,kmax),xzd(imax,kmax)      &
            )
       !
       ALLOCATE(                                    &
            dellah(imax,kmax),dellaq(imax,kmax)                   &
            ,dellat(imax,kmax),dellaqc(imax,kmax)            &
            ) 
       !
       ALLOCATE(                                    &
            dellat_ens(imax,kmax,maxens2)                  &
            ,dellaq_ens(imax,kmax,maxens2)                  &
            ,dellaqc_ens(imax,kmax,maxens2)                  &
            ,pwo_ens(imax,kmax,maxens2)                        &
            ) 
       !
       ALLOCATE(                                    &
            qes_cup(imax,kmax),q_cup(imax,kmax)                    &
            ,he_cup(imax,kmax),hes_cup(imax,kmax)               &
            ,z_cup(imax,kmax),p_cup(imax,kmax)                   &
            ,gamma_cup(imax,kmax),t_cup(imax,kmax)             &
            ,qeso_cup(imax,kmax),qo_cup(imax,kmax)              &
            ,heo_cup(imax,kmax),heso_cup(imax,kmax)               &
            ,zo_cup(imax,kmax),po_cup(imax,kmax)               &
            ,gammao_cup(imax,kmax),tn_cup(imax,kmax)            &
            ,xqes_cup(imax,kmax),xq_cup(imax,kmax)             &
            ,xhe_cup(imax,kmax),xhes_cup(imax,kmax)              &
            ,xz_cup(imax,kmax),xt_cup(imax,kmax)              &
            ) 
       !
       ALLOCATE(                                    &
            cd(imax,kmax),cdd(imax,kmax)                   &
            ,scr1(imax,kmax),cap_max(imax)                    &
            ,mbdt_ens(maxens),xaa0_ens(imax,maxens)             &
            ,edt_ens(maxens2),edtc(imax,maxens2)            &
            ) 
       !
       ALLOCATE(                                    &
            cwf(imax,jmax),pwf(imax,jmax)                     &
            ,pwdf(imax,jmax),eddt(imax,jmax),predb(imax,jmax)   &
            ,xktop(imax,jmax),xkbas(imax,jmax),xmass(imax,jmax)      &
            ,xf(imax,ensdim)                               &
            ) 

       ALLOCATE(                                       &
            xf_ens(imax,ensdim),pr_ens(imax,ensdim)             &
            ,outt_ens(imax,ensdim)                        &
            ) 
       !
       !srf variaveis para a rotina "cup_dd_edt"
       ALLOCATE( vshear(imax),sdp(imax),vws(imax) )
       !
    END IF
    !
    !----------------------end of memory allocation ----
    !
    !zzzzzzzzzzzzzzzzzzzzz zero out zzzzzzzzzzzzzzzzzzzz
    izero=1
    IF(izero.EQ.1) THEN
       !set zero all variables:
       CALL zero_out(imax,kmax,jmax,ensdim                    &
            ,maxens,maxens2                        &
            ,kzdown,kbmax,ierr                               &
            ,k22,kbcon,kb,jmin                               &
            ,ktop,kstabi,kstabm                               &
            ,k22x,kbconx,kbx,ktopx                         &
            ,he,hes,qes                                     &
            ,z,tv,dby                                     &
            ,qc,qrcd,pwd                               &
            ,pw,edt,edto,edtx                               &
            ,aa1,aa0,xaa0,hkb                              &
            ,hkbo,aad                                    &
            ,xhkb,qkb,qkbo,xmb                               &
            ,heo,heso,qeso                              &
            ,zo,tvo,dbyo                              &
            ,qco,qrcdo,pwdo                              &
            ,pwo                                    &
            ,xpwav,xpwev,pwav,pwev                         &
            ,pwavo,pwevo                              &
            ,bu,buo                                    &
            ,xhe,xhes,xqes                              &
            ,xz,xtv,xt                                    &
            ,xq,xqc,xqrcd                              &
            ,xdby,xpwd,xpw                              &
            ,hcd,hcdo,xhcd                              &
            ,qcd,qcdo,xqcd                              &
            ,dbyd,dbydo                                    &
            ,hc,hco,xhc                                    &
            ,qrc,qrco,xqrc                              &
            ,zu,zuo,xzu                                    &
            ,zd,zdo,xzd                                    &
            ,dellah,dellaq                              &
            ,dellat,dellaqc                              &
            ,dellat_ens                                    &
            ,dellaq_ens                                    &
            ,dellaqc_ens                              &
            ,pwo_ens                                    &
            ,qes_cup,q_cup                              &
            ,he_cup,hes_cup                              &
            ,z_cup,p_cup                              &
            ,gamma_cup,t_cup                              &
            ,qeso_cup,qo_cup                              &
            ,heo_cup,heso_cup                              &
            ,zo_cup,po_cup                              &
            ,gammao_cup,tn_cup                               &
            ,xqes_cup,xq_cup                              &
            ,xhe_cup,xhes_cup                              &
            ,xz_cup,xt_cup                              &
            ,cd,cdd                                    &
            ,scr1,cap_max                              &
            ,mbdt_ens,xaa0_ens                               &
            ,edt_ens,edtc                              &
            ,cwf,pwf                                    &
            ,pwdf,eddt,predb                              &
            ,xktop,xkbas,xmass                               &
            ,xf                                           &
            ,xf_ens,pr_ens                              &
            ,outt_ens                                    &
            ,vshear,sdp,vws )
       !
    END IF
    !
    day=86400.
    !
    !--- specify entrainmentrate and detrainmentrate
    !
    radius=12000.
    fquasi=1
    fstab=0  
    fmconv=0
    !
    !--- gross entrainment rate (these may be changed later on in the
    !--- program, depending what your detrainment is!!)
    !
    entr_rate=.2/radius
    !
    !--- entrainment of mass
    !
    mentrd_rate=entr_rate
    mentr_rate =entr_rate
    !
    !--- initial detrainmentrates
    !
    DO k=1,mkx
       DO i=istart,iend
          cd(i,k)=0.3*entr_rate
          cdd(i,k)=0.
       END DO
    END DO
    !
    !--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
    !    base mass flux
    !
    edtmax=.95
    edtmin=.2
    !
    !--- minimum depth (m), clouds must have
    !
    depth_min=500.
    !---- for kbcon_cin test
    cap_maxs=10.
    IF(iens.EQ.2)cap_maxs=20.
    IF(iens.EQ.3)cap_maxs=30.
    IF(iens.EQ.4)cap_maxs=40.
    cap_maxs=20.
    !
    DO i=istart,iend
       aa0(i)=0.
       aa1(i)=0.
       aad(i)=0.
       kstabm(i)=mkx-2
       IF(aaeq(i).LT.0.)THEN
          ierr(i)=20
       ELSE
          ierr(i)=0
          xierr(i,j)=0.
          cwf(i,j)=0.
          pwf(i,j)=0.
          pwdf(i,j)=0.
          eddt(i,j)=0.
          xktop(i,j)=0.
          xkbas(i,j)=0.
          xmass(i,j)=0.
          predb(i,j)=0.
       END IF
    END DO
    !
    !--- first check for upstream convection
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          iresult=0
          massfld=0.
          CALL cup_direction2(i,j,direction,iact_old_gr,mix,mjx,  &
               massflx,iresult,0,massfld)
          !
          cap_max(i)=cap_maxs
          IF(iresult.EQ.1)THEN
             cap_max(i)=cap_maxs+20.
          END IF
       END IF
    END DO
    !
    !--- max height(m) above ground where updraft air can originate
    !
    zkbmax=4000.
    !
    !--- height(m) above which no downdrafts are allowed to originate
    !
    zcutdown=3000.
    !
    !--- depth(m) over which downdraft detrains all its mass
    !
    z_detr=1250.
    !
    DO nens=1,maxens
       mbdt_ens(nens)=(float(nens)-3.)*dtime*1.e-3+dtime*5.e-03
       !        mbdt_ens(nens)=(float(nens)-1.5)*dtime*2.e-3+dtime*5.e-03
    END DO
    DO nens=1,maxens2
       !        edt_ens(nens)=.7-float(nens)*.1
       edt_ens(nens)=.95-float(nens)*.01
    END DO
    !
    !--- environmental conditions, first heights
    !
    DO i=istart,iend
       IF(ierr(i).NE.20)THEN
          DO k=1,maxens*maxens2*maxens3
             xf_ens(  i,(iens-1)*maxens*maxens2*maxens3+k)= 0.
             pr_ens(  i,(iens-1)*maxens*maxens2*maxens3+k)= 0.
             outt_ens(i,(iens-1)*maxens*maxens2*maxens3+k)= 0.
          END DO
       END IF
    END DO
    !
    !--- calculate moist static energy, heights, qes
    !
    CALL cup_env(z,qes,he,hes,t,q,p,z1,mix,mkx,       &
         istart,iend,psur,ierr,tcrit,0)
    CALL cup_env(zo,qeso,heo,heso,tn,qo,po,z1,mix,mkx, &
         istart,iend,psur,ierr,tcrit,0)
    !
    !--- environmental values on cloud levels
    !
    CALL cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,       &
         q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur,  &
         mix,mkx,istart,iend,ierr,z1)
    CALL cup_env_clev(tn,qeso,qo,heo,heso,zo,po,       &
         qeso_cup,qo_cup,heo_cup,heso_cup,zo_cup,po_cup,       &
         gammao_cup,tn_cup,psur,mix,mkx,istart,iend,ierr,z1)
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          !
          DO k=1,mkx
             IF(zo_cup(i,k).GT.zkbmax+z1(i))THEN
                kbmax(i)=k
                EXIT
             END IF
          END DO
          !
          !--- level where detrainment for downdraft starts
          !
          DO k=1,mkx
             IF(zo_cup(i,k).GT.z_detr+z1(i))THEN
                kdet(i)=k
                EXIT
             END IF
          END DO
          !
       END IF
    END DO
    !
    !--- determine level with highest moist static energy content - k22
    !
    CALL maximi(heo_cup,mix,mkx,3,kbmax,k22,istart,iend,ierr)
    DO i=istart,iend
       IF(ierr(i).EQ.0.)THEN
          IF(k22(i).GE.kbmax(i))ierr(i)=2
       END IF
    END DO
    !
    !--- determine the level of convective cloud base  - kbcon
    !grell sugested to test cup_kbcon -18dec2001
    !
    CALL cup_kbcon(1,k22,kbcon,heo_cup,heso_cup,              &
         mix,mkx,istart,iend,ierr,kbmax,po_cup,cap_max)
    !
    !--- increase detrainment in stable layers
    !
    CALL minimi(heso_cup,mix,mkx,kbcon,kstabm,                     &
         kstabi,istart,iend,ierr)
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0.)THEN
          IF(kstabm(i)-1.GT.kstabi(i))THEN
             DO k=kstabi(i),kstabm(i)-1
                cd(i,k)=cd(i,k-1)+1.5*entr_rate
                IF(iens.GT.4)THEN
                   cd(i,k)=cd(i,k-1)+float(iens-4)*entr_rate        &
                        /float(kstabm(i)-kstabi(i))
                ELSE
                   cd(i,k)=cd(i,k)
                END IF
                IF(cd(i,k).GT.10.0*entr_rate)cd(i,k)=10.0*entr_rate
             END DO
          END IF
       END IF
    END DO
    !
    !--- calculate incloud moist static energy
    !
    CALL cup_up_he(k22,hkb,z_cup,cd,mentr_rate,he_cup,hc,       &
         mix,mkx,kbcon,ierr,istart,iend,dby,he,hes_cup)
    CALL cup_up_he(k22,hkbo,zo_cup,cd,mentr_rate,heo_cup,hco,       &
         mix,mkx,kbcon,ierr,istart,iend,dbyo,heo,heso_cup)
    !
    !--- determine cloud top - ktop
    !
    CALL cup_ktop(1,dbyo,kbcon,ktop,mix,mkx,istart,iend,ierr)
    !
    DO i=istart,iend
       kzdown(i)=0
       IF(ierr(i).EQ.0)THEN
          zktop=(zo_cup(i,ktop(i))-z1(i))*.6
          zktop=MIN(zktop+z1(i),zcutdown+z1(i))
          DO k=1,mkx
             IF(zo_cup(i,k).GT.zktop)THEN
                kzdown(i)=k
                go to 37
             END IF
          END DO
       END IF
37     CONTINUE
    END DO
    !
    !--- downdraft originating level - jmin
    !
    CALL minimi(heso_cup,mix,mkx,k22,kzdown,jmin,istart,iend,ierr)
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0.)THEN
          !
          !--- check whether it would have buoyancy, if there where
          !--- no entrainment/detrainment
          !
101       CONTINUE
          IF(jmin(i)-1.LT.kdet(i))kdet(i)=jmin(i)-1
          IF(jmin(i).GE.ktop(i)-1)jmin(i)=ktop(i)-2
          ki=jmin(i)
          !
          hcdo(i,ki)=heso_cup(i,ki)
          dz=zo_cup(i,ki+1)-zo_cup(i,ki)
          dh=dz*(hcdo(i,ki)-heso_cup(i,ki))
          dh=0.
          !
          DO k=ki-1,1,-1
             !c        hcdo(i,k)=heo_cup(i,jmin(i))
             hcdo(i,k)=heso_cup(i,jmin(i))
             dz=zo_cup(i,k+1)-zo_cup(i,k)
             dh=dh+dz*(hcdo(i,k)-heso_cup(i,k))
             IF(dh.GT.0.)THEN
                jmin(i)=jmin(i)-1
                IF(jmin(i).GT.3)THEN
                   go to 101
                ELSE IF(jmin(i).LE.3)THEN
                   ierr(i)=9
                   go to 100
                END IF
             END IF
          END DO
          !
          IF(jmin(i).LE.3)THEN
             ierr(i)=4
          END IF
          !
       END IF
100    CONTINUE
    END DO
    !
    !--- must have at least depth_min m between cloud convective base
    !    and cloud top.
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0.)THEN
          IF(-zo_cup(i,kbcon(i))+zo_cup(i,ktop(i)).LT.depth_min)THEN
             ierr(i)=6
          END IF
       END IF
    END DO
    !
    !--- normalized updraft mass flux profile
    !
    CALL cup_up_nms(zu,z_cup,mentr_rate,cd,kbcon,ktop,mix,mkx,   &
         istart,iend,ierr,k22)
    CALL cup_up_nms(zuo,zo_cup,mentr_rate,cd,kbcon,ktop,mix,mkx, &
         istart,iend,ierr,k22)
    !
    !--- normalized downdraft mass flux profile,also work on bottom detrainment
    !--- in this routine
    !
    CALL cup_dd_nms(zd,z_cup,cdd,mentrd_rate,jmin,ierr,        &
         mix,mkx,istart,iend,0,kdet,z1)
    CALL cup_dd_nms(zdo,zo_cup,cdd,mentrd_rate,jmin,ierr,      &
         mix,mkx,istart,iend,1,kdet,z1)
    !
    !--- downdraft moist static energy
    !
    CALL cup_dd_he(hes_cup,hcd,z_cup,cdd,mentrd_rate,       &
         jmin,ierr,mix,mkx,istart,iend,he,dbyd)
    CALL cup_dd_he(heso_cup,hcdo,zo_cup,cdd,mentrd_rate,        &
         jmin,ierr,mix,mkx,istart,iend,heo,dbydo)
    !
    !--- calculate moisture properties of downdraft
    !
    CALL cup_dd_moisture(zd,hcd,hes_cup,qcd,qes_cup,                   &
         pwd,q_cup,z_cup,cdd,mentrd_rate,jmin,ierr,gamma_cup,            &
         pwev,mix,mkx,istart,iend,bu,qrcd,q,2)
    CALL cup_dd_moisture(zdo,hcdo,heso_cup,qcdo,qeso_cup,            &
         pwdo,qo_cup,zo_cup,cdd,mentrd_rate,jmin,ierr,gammao_cup,      &
         pwevo,mix,mkx,istart,iend,bu,qrcdo,qo,1)
    !
    !--- calculate moisture properties of updraft
    !
    CALL cup_up_moisture(ierr,z_cup,qc,qrc,pw,pwav,                    &
         kbcon,ktop,mix,mkx,istart,iend,cd,dby,mentr_rate,             &
         q,gamma_cup,zu,qes_cup,k22,q_cup)
    CALL cup_up_moisture(ierr,zo_cup,qco,qrco,pwo,pwavo,              &
         kbcon,ktop,mix,mkx,istart,iend,cd,dbyo,mentr_rate,             &
         q,gammao_cup,zuo,qeso_cup,k22,qo_cup)
    !
    !--- calculate workfunctions for updrafts
    !
    CALL cup_up_aa0(aa0,z,zu,dby,gamma_cup,t_cup,               &
         kbcon,ktop,mkx,mix,istart,iend,ierr)
    CALL cup_up_aa0(aa1,zo,zuo,dbyo,gammao_cup,tn_cup,        &
         kbcon,ktop,mkx,mix,istart,iend,ierr)
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          IF(aa1(i).EQ.0.)THEN
             ierr(i)=17
          END IF
       END IF
    END DO
    !
    !--- determine downdraft strength in terms of windshear
    !
    CALL cup_dd_edt(ierr,us,vs,zo,ktop,kbcon,edt,po,pwavo             &
         ,pwevo,mix,mkx,istart,iend,edtmax,edtmin,maxens2,edtc            &
         ,vshear,sdp,vws)
    !
    !- big loop starts here!
    !
    DO iedt=1,maxens2
       DO i=istart,iend
          IF(ierr(i).EQ.0)THEN
             edt(i)=edtc(i,iedt)
             edto(i)=edtc(i,iedt)
             edtx(i)=edtc(i,iedt)
          END IF
       END DO
       DO k=1,mkx
          DO i=istart,iend
             dellat_ens(i,k,iedt)=0.
             dellaq_ens(i,k,iedt)=0.
             dellaqc_ens(i,k,iedt)=0.
             pwo_ens(i,k,iedt)=0.
          END DO
       END DO
       !
       DO i=istart,iend
          aad(i)=0.
       END DO
       !
       !--- downdraft workfunctions
       CALL cup_dd_aa0(edto,ierr,aad,jmin,gammao_cup,tn_cup,            &
            hcdo,heso_cup,zo,mix,mkx,istart,iend,zdo)
       !
       !--- change per unit mass that a model cloud would modify the environment
       !
       !--- 1. in bottom layer
       !
       !trocando 'po'  por   'po_cup'
       !
       CALL cup_dellabot(heo_cup,ierr,zo_cup,po_cup,hcdo,edto,              &
            zdo,cdd,heo,mix,mkx,istart,iend,dellah,mentrd_rate)
       CALL cup_dellabot(qo_cup ,ierr,zo_cup,po_cup,qrcdo,edto,       &
            zdo,cdd,qo, mix,mkx,istart,iend,dellaq,mentrd_rate)
       !
       !--- 2. everywhere else
       !
       CALL cup_dellas(ierr,zo_cup,po_cup,hcdo,edto,zdo,cdd,              &
            heo,mix,mkx,istart,iend,dellah,j,mentrd_rate,zuo,       &
            cd,hco,ktop,k22,kbcon,mentr_rate,jmin,heo_cup,kdet,       &
            k22,'deep')
       !
       !-- take out cloud liquid water for detrainment
       !
       DO k=1,mkx
          DO i=istart,iend
             scr1(i,k)=0.
             dellaqc(i,k)=0.
             IF(ierr(i).EQ.0)THEN
                !       print *,'in vupnewg, after della ',ierr(i),aa0(i),i,j
                scr1(i,k)=qco(i,k)-qrco(i,k)
                !
                !modificacao sug. pelo grell
                !
                IF(k.EQ.ktop(i)-0)dellaqc(i,k)=                   &
                     .01*zuo(i,ktop(i))*qrco(i,ktop(i))*      &
                     9.81/(po_cup(i,k  )-po_cup(i,k+1))
                !
                IF(k.LT.ktop(i)  .AND.k.GT.kbcon(i))THEN
                   dz=zo_cup(i,k+1)-zo_cup(i,k)
                   dellaqc(i,k)=.01*9.81*cd(i,k)*dz*zuo(i,k)         &
                        *.5*(qrco(i,k)+qrco(i,k+1))/               &
                        (po_cup(i,k  )-po_cup(i,k+1))
                END IF
             END IF
          END DO
       END DO
       !
       CALL cup_dellas(ierr,zo_cup,po_cup,qrcdo,edto,zdo,cdd,             &
            qo,mix,mkx,istart,iend,dellaq,j,mentrd_rate,zuo,             &
            cd,scr1,ktop,k22,kbcon,mentr_rate,jmin,qo_cup,kdet,       &
            k22,'deep')
       !
       !--- using dellas, calculate changed environmental profiles
       !
       DO nens=1,maxens
          mbdt=mbdt_ens(nens)
          DO i=istart,iend
             xaa0_ens(i,nens)=0.
          END DO
          DO k=1,mkx-1
             DO i=istart,iend
                dellat(i,k)=0.
                IF(ierr(i).EQ.0)THEN
                   xhe(i,k)=dellah(i,k)*mbdt+heo(i,k)
                   xq(i,k)=dellaq(i,k)*mbdt+qo(i,k)
                   dellat(i,k)=(1./1004.)*(dellah(i,k)-2.5e06*dellaq(i,k))
                   xt(i,k)= dellat(i,k)*mbdt+tn(i,k)
                   IF(xq(i,k).LE.0.)xq(i,k)=1.e-08
                END IF
             END DO
          END DO
          !
          DO i=istart,iend
             IF(ierr(i).EQ.0)THEN
                xhe(i,mkx)=heo(i,mkx)
                xq(i,mkx)=qo(i,mkx)
                xt(i,mkx)=tn(i,mkx)
                IF(xq(i,mkx).LE.0.)xq(i,mkx)=1.e-08
             END IF
          END DO
          !
          !--- calculate moist static energy, heights, qes
          !
          CALL cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1,mix,mkx,            &
               istart,iend,psur,ierr,tcrit,2)
          !
          !--- environmental values on cloud levels
          !
          CALL cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,       &
               xq_cup,xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,            &
               psur,mix,mkx,istart,iend,ierr,z1)
          !
          !**************************** static control
          !
          !--- moist static energy inside cloud
          !
          DO i=istart,iend
             IF(ierr(i).EQ.0)THEN
                xhkb(i)=xhe(i,k22(i))
             END IF
          END DO
          CALL cup_up_he(k22,xhkb,xz_cup,cd,mentr_rate,xhe_cup,xhc,               &
               mix,mkx,kbcon,ierr,istart,iend,xdby,xhe,xhes_cup)
          !
          !--- normalized mass flux profile
          !
          CALL cup_up_nms(xzu,xz_cup,mentr_rate,cd,kbcon,ktop,mix,mkx,            &
               istart,iend,ierr,k22)
          CALL cup_dd_nms(xzd,xz_cup,cdd,mentrd_rate,jmin,ierr,                     &
               mix,mkx,istart,iend,1,kdet,z1)
          !
          !--- moisture downdraft
          !
          CALL cup_dd_he(xhes_cup,xhcd,xz_cup,cdd,mentrd_rate,             &
               jmin,ierr,mix,mkx,istart,iend,xhe,dbyd)
          CALL cup_dd_moisture(xzd,xhcd,xhes_cup,xqcd,xqes_cup,             &
               xpwd,xq_cup,xz_cup,cdd,mentrd_rate,jmin,ierr,gamma_cup,             &
               xpwev,mix,mkx,istart,iend,bu,xqrcd,xq,3)
          !
          !--- moisture updraft
          !
          CALL cup_up_moisture(ierr,xz_cup,xqc,xqrc,xpw,xpwav,                     &
               kbcon,ktop,mix,mkx,istart,iend,cd,xdby,mentr_rate,               &
               xq,gamma_cup,xzu,xqes_cup,k22,xq_cup)
          !
          !--- workfunctions for updraft
          !
          CALL cup_up_aa0(xaa0,xz,xzu,xdby,gamma_cup,xt_cup,                   &
               kbcon,ktop,mkx,mix,istart,iend,ierr)
          !
          !--- workfunctions for downdraft
          !
          !c     call cup_dd_aa0(edtx,ierr,xaa0,jmin,gamma_cup,xt_cup,
          !c    1      xhcd,xhes_cup,xz,mix,mkx,istart,iend,xzd)
          DO i=istart,iend 
             IF(ierr(i).EQ.0)THEN
                xaa0_ens(i,nens)=xaa0(i)
                nall=(iens-1)*maxens3*maxens*maxens2                        &
                     +(iedt-1)*maxens*maxens3                               &
                     +(nens-1)*maxens3
                DO k=1,mkx
                   IF(k.LE.ktop(i))THEN
                      DO nens3=1,maxens3
                         IF(nens3.EQ.7)THEN
                            !c--- b=0
                            pr_ens(i,nall+nens3)=pr_ens(i,nall+nens3)+            &
                                 pwo(i,k)+edto(i)*pwdo(i,k)
                            !c--- b=beta
                         ELSE IF(nens3.EQ.8)THEN
                            pr_ens(i,nall+nens3)=pr_ens(i,nall+nens3)+                &
                                 pwo(i,k)
                            !c--- b=beta/2
                         ELSE IF(nens3.EQ.9)THEN
                            pr_ens(i,nall+nens3)=pr_ens(i,nall+nens3)+               &
                                 pwo(i,k)+.5*edto(i)*pwdo(i,k)
                         ELSE
                            pr_ens(i,nall+nens3)=pr_ens(i,nall+nens3)+               &
                                 pwo(i,k)+edto(i)*pwdo(i,k)
                         END IF
                      END DO
                   END IF
                END DO
                !
                DO nens3=1,maxens3
                   outt_ens(i,nall+nens3)=dellat(i,1)
                END DO
             END IF
          END DO
       END DO
       !
       !--- large scale forcing
       !
       !------- check wether aa0 should have been zero
       !
       CALL maximi(he_cup,mix,mkx,3,kbmax,k22x,istart,iend,ierr)
       DO i=istart,iend
          IF(ierr(i).EQ.0)THEN
             IF(k22x(i).GE.kbmax(i))ierr(i)=998
          END IF
       END DO
       !
       !--- determine the level of convective cloud base  - kbcon
       !
       IF(ensdim.EQ.1)THEN
          CALL cup_forcing_ens1(aa0,aa1,xaa0_ens,mbdt_ens,dtime,              &
               xmb,ierr,mix,istart,iend,mjx,xf_ens,j,               &
               'deeps',maxens,iens,iedt,maxens2,            &
               maxens3,edto,             &
               massflx,iact_old_gr,direction,ensdim,                          &
               massfln)
       ELSE
          !changed by grell on 07-dez-2001
          !      call cup_forcing_ens(aa0,aa1,xaa0_ens,mbdt_ens,dtime,
          !     1   xmb,ierr,mix,mkx,istart,iend,mjx,xf_ens,j,fquasi,
          !     2   fstab,'deeps',xland,maxens,iens,iedt,maxens2,ipr,jpr,
          !     3   maxens3,mconv,omeg,zdo,kbcon,zuo,pr_ens,edto,aad,kdet,
          !     4   massflx,iact_old_gr,direction,ensdim,
          !     5   massfln)
          CALL cup_forcing_ens(aa0,aa1,xaa0_ens,mbdt_ens,dtime,                   &
               xmb,ierr,mix,mkx,istart,iend,mjx,xf_ens,j,            &
               'deeps',maxens,iens,iedt,maxens2,                   &
               maxens3,mconv,omeg,kbcon,pr_ens,edto,kdet,            &
               massflx,iact_old_gr,direction,ensdim,                         &
               massfln,massfld,iresult)
       END IF
       !
       DO i=istart,iend
          xierr(i,j)=float(ierr(i))
          IF(ierr(i).EQ.0)THEN
             cwf(i,j)=aa0(i)
             pwf(i,j)=pwav(i)
             pwdf(i,j)=pwev(i)
             xktop(i,j)=float(ktop(i))
             xkbas(i,j)=float(kbcon(i))
             xmass(i,j)=xmb(i)
          ELSEIF(ierr(i).NE.0.AND.ierr(i).NE.20)THEN
             cwf(i,j)=0.
             pwf(i,j)=0.
             pwdf(i,j)=0.
             xktop(i,j)=0.
             xkbas(i,j)=0.
             xmass(i,j)=0.
          END IF
       END DO
       DO k=1,mkx
          DO i=istart,iend
             IF(ierr(i).EQ.0)THEN
                dellat_ens(i,k,iedt)=dellat(i,k)
                dellaq_ens(i,k,iedt)=dellaq(i,k)
                dellaqc_ens(i,k,iedt)=dellaqc(i,k)
                pwo_ens(i,k,iedt)=pwo(i,k)+edt(i)*pwdo(i,k)
             ELSE 
                dellat_ens(i,k,iedt)=0.
                dellaq_ens(i,k,iedt)=0.
                dellaqc_ens(i,k,iedt)=0.
                pwo_ens(i,k,iedt)=0.
             END IF
          END DO
       END DO
    END DO
    !
    !--- feedback
    !
    CALL cup_output_ens(xf_ens,ierr,dellat_ens,dellaq_ens,dellaqc_ens,       &
         outt,outq,outqc,pre,pwo_ens,xmb,ktop,mix,mkx,             &
         istart,iend,maxens2,maxens,iens,              &
         pr_ens,outt_ens,maxens3,ensdim,massfln)            
    DO i=istart,iend
       pre(i)=MAX(pre(i),0.)
    END DO
    !
    !---------------------------done------------------------------
    !
    RETURN
    !
  END SUBROUTINE cup_enss




  SUBROUTINE cup_dellas(ierr,z_cup,p_cup,hcd,edt,zd,cdd,             &
       he,mix,mkx,istart,iend,della,j,mentrd_rate,zu,            &
       cd,hc,ktop,k22,kbcon,mentr_rate,jmin,he_cup,kdet,kpbl,             &
       name)
    CHARACTER *(*) name
    INTEGER mix,mkx,i,k,istart,iend,j
    REAL z_cup(mix,mkx),p_cup(mix,mkx),hcd(mix,mkx),zd(mix,mkx),       &
         cdd(mix,mkx),he(mix,mkx),della(mix,mkx),hc(mix,mkx),               &
         cd(mix,mkx),zu(mix,mkx),he_cup(mix,mkx)
    REAL edt(mix)
    INTEGER kbcon(mix),ktop(mix),k22(mix),jmin(mix)
    INTEGER ierr(mix),kdet(mix),kpbl(mix)
    REAL entdo,g,dp,dz,mentrd_rate,mentr_rate,             &
         subin,detdo,entup,detup,subdown,entdoj,entupk,                    &
         detupk,totmas
    g=9.81
    i=istart
    DO k=2,mkx
       DO i=istart,iend
          della(i,k)=0.
       END DO
    END DO
    !
    DO k=2,mkx-1
       DO i=istart,iend
          IF(ierr(i).NE.0)go to 100
          IF(k.GT.ktop(i))go to 100
          !
          !--- specify detrainment of downdraft, has to be consistent
          !--- with zd calculations in soundd.
          !
          dz    = z_cup(i,k+1)-z_cup(i,k)
          detdo = edt(i)*cdd(i,k)   *dz*zd(i,k+1)
          entdo = edt(i)*mentrd_rate*dz*zd(i,k+1)
          subin = zu(i,k+1)-zd(i,k+1)*edt(i)
          entup = 0.
          detup = 0.
          IF(k.GE.kbcon(i).AND.k.LT.ktop(i))THEN
             entup = mentr_rate*dz*zu(i,k)
             detup = cd(i,k+1) *dz*zu(i,k)
          END IF
          subdown = ( zu(i,k)-zd(i,k)*edt(i) )
          entdoj  = 0.
          entupk  = 0.
          detupk  = 0.
          !
          IF(k.EQ.jmin(i))THEN
             entdoj  = zd(i,k)*edt(i)
          END IF
          !
          IF(k.EQ.k22(i)-1)THEN
             !        if(k.eq.kpbl(i))then
             entupk  = zu(i,kpbl(i))
          END IF
          !
          IF(k.GT.kdet(i))THEN
             detdo   = 0.
          END IF
          !
          IF(k.EQ.ktop(i)-0)THEN
             detupk  = zu(i,ktop(i))
             subin   = 0.
          END IF
          IF(k.LT.kbcon(i))THEN
             detup   = 0.
          END IF
          !
          !--- changed due to subsidence and entrainment
          !
          totmas=subin-subdown+detup-entup-entdo+                   &
               detdo-entupk-entdoj+detupk
          IF(ABS(totmas).GT.1.e-6)THEN
             PRINT *,'*********************',i,j,k,totmas,name
             PRINT *,kpbl(i),k22(i),kbcon(i),ktop(i)
             STOP
          END IF
          !      
          dp =  100.*( p_cup(i,k)-p_cup(i,k+1) )
          della(i,k)=(                                              &
               subin  *he_cup(i,k+1)                              &
               - subdown*he_cup(i,k  )                                 &
               + detup*.5*( hc(i,k+1)+ hc(i,k))                     &
               + detdo*.5*(hcd(i,k+1)+hcd(i,k))                     &
               - entup*he(i,k)                                    &
               - entdo*he(i,k)                                    &
               - entupk*he_cup(i,k22(i))                          &
               - entdoj*he_cup(i,jmin(i))                           &
               + detupk*hc(i,ktop(i))                              &
               )*g/dp
          !     
100       CONTINUE
       END DO
    END DO
    !
    RETURN
  END SUBROUTINE cup_dellas




  SUBROUTINE cup_dellabot(he_cup,ierr,z_cup,p_cup,              &
       hcd,edt,zd,cdd,he,mix,mkx,istart,iend,della,              &
       mentrd_rate)
    INTEGER mix,mkx,i,istart,iend
    REAL z_cup(mix,mkx),p_cup(mix,mkx),hcd(mix,mkx),zd(mix,mkx),      &
         cdd(mix,mkx),he(mix,mkx),della(mix,mkx),he_cup(mix,mkx),      &
         edt(mix)
    INTEGER ierr(mix)
    REAL detdo1,detdo2,entdo,g,dp,dz,mentrd_rate,subin,detdo
    !
    g=9.81
    DO i=istart,iend
       della(i,1)=0.
       IF(ierr(i).NE.0)go to 100
       dz        =       z_cup(i,2)-z_cup(i,1)
       dp        = 100.*(p_cup(i,1)-p_cup(i,2))
       detdo1    = edt(i)*zd(i,2)*cdd(i,1)*dz
       detdo2    = edt(i)*zd(i,1)
       entdo     = edt(i)*zd(i,2)*mentrd_rate*dz
       subin     =-edt(i)*zd(i,2)
       detdo     = detdo1+detdo2-entdo+subin
       della(i,1)= (  detdo1*.5*(hcd(i,1)+hcd(i,2))       &
            + detdo2*    hcd(i,1)                   &
            + subin *    he_cup(i,2)                   &
            - entdo *    he(i,1)    )*g/dp
       !
100    CONTINUE
    END DO
    RETURN
  END SUBROUTINE cup_dellabot




  SUBROUTINE cup_forcing_ens1(aa0,aa1,xaa0,mbdt,dtime,xmb,ierr,             &
       mix,istart,iend,mjx,xf,j,name,                          &
       maxens,iens,iedt,maxens2,maxens3,                        &
       edt,massflx,                        &
       iact_old_gr,dir,ensdim,massfln)
    CHARACTER *(*) name
    !
    INTEGER i,istart,iend,mix,mjx,j,maxens,maxens3
    INTEGER ensdim,ierr(mix),iens,nall,iedt,maxens2
    INTEGER iact_old_gr(mix,mjx)
    REAL aa0(mix),aa1(mix),xaa0(mix,maxens),xmb(mix)
    REAL mbdt(maxens),dtime,edt(mix),dir(mix)
    REAL xf(mix,ensdim)
    REAL xff_ens3(maxens3),xk(maxens),xff0
    REAL massflx(mix)
    REAL massfln(mix,ensdim)
    REAL massfld
    INTEGER nens,ne,n,nens3,iresult,iresultd,  &
         iresulte
    nens=0
    !
    !--- large scale forcing
    !
    DO i=istart,iend
       xmb(i)=0.
       IF(name.EQ.'deeps'.AND.ierr(i).GT.995)THEN
          aa0(i)=0.
          ierr(i)=0
       END IF
       IF(ierr(i).EQ.0)THEN
          !
          !--- treatment different for this closure
          !
          IF(name.EQ.'deeps')THEN
             !
             xff0= (aa1(i)-aa0(i))/dtime
             xff_ens3(1)=(aa1(i)-aa0(i))/dtime
             !
             !   
             !--- more original arakawa-schubert (climatologic value of aa0)
             !
             !               xff_ens3(2)=max(0.,(aa1(i)-1500.)/dtime)
             !               xff_ens3(2)=xff_ens3(1)
             !
             !--- omeg is in bar/s, mconv done with omeg in pa/s
             !     more like brown (1979), or frank-cohen (199?)
             !
             !csrf           xff_ens3(3)=-1.e5*omeg(i,k22(i))/9.81
             !ccsrf           xff_ens3(4)=-1.e5*omeg(i,kbcon(i))/9.81
             !               xff_ens3(3)=-omeg(i,k22(i))/9.81
             !               xff_ens3(4)=-omeg(i,kbcon(i))/9.81
             !
             !--- more like krishnamurti et al.
             !
             !               xff_ens3(5)=mconv(i)
             !               xff_ens3(6)=mconv(i)
             !
             !--- more like fritsch chappel or kain fritsch (plus triggers)
             !
             !               xff_ens3(7)=aa1(i)/(60.*30.)
             !               xff_ens3(8)=aa1(i)/(60.*40.)
             !
             DO nens=1,maxens
                xk(nens)=(xaa0(i,nens)-aa1(i))/mbdt(nens)       
                IF(xk(nens).LE.0.AND.xk(nens).GT.-1.e-9)               &
                     xk(nens)=-1.e-9
                IF(xk(nens).GT.0.AND.xk(nens).LT.1.e-9)                     &
                     xk(nens)=1.e-9
             END DO
             !
             !--- add up all ensembles
             !
             DO ne=1,maxens
                !
                !--- for every xk, we have maxens3 xffs
                !--- iens is from outermost ensemble (most expensive!
                !
                !--- iedt (maxens2 belongs to it)
                !--- is from second, next outermost, not so expensive
                !
                !--- so, for every outermost loop, we have maxens*maxens2*3
                !--- ensembles!!! nall would be 0, if everything is on first
                !--- loop index, then ne would start counting, then iedt, then iens....
                !
                iresultd=0
                iresulte=0
                nall= (iens-1)*maxens3*maxens*maxens2              &
                     +(iedt-1)*maxens3*maxens                   &
                     +(  ne-1)*maxens3
                !
                !--- check for upwind convection
                !
                iresult=0
                massfld=0.
                CALL cup_direction2(i,j,dir,iact_old_gr,mix,mjx,       &
                     massflx,iresult,1,             &
                     massfld)
                !
                IF(xk(ne).LT.0.AND.xff0.GT.0.)iresultd=1
                iresulte=MAX(iresult,iresultd)
                !                  iresulte=1
                IF(iresulte.EQ.1)THEN
                   !
                   !--- special treatment for stability closures
                   !
                   IF(xff0.GT.0.)THEN
                      xf(i,nall+1)=MAX(0.,-xff_ens3(1)/xk(ne))       &
                           +massfld
                   ELSE
                      xf(i,nall+1)=massfld
                   END IF
                   !
                   !--- store new for next time step
                   !
                   DO nens3=1,maxens3
                      massfln(i,nall+nens3)=edt(i)                &
                           *xf(i,nall+nens3)
                      massfln(i,nall+nens3)=MAX(0.,             &
                           massfln(i,nall+nens3))
                      !
                   END DO
                END IF
             END DO
             go to 100
          END IF
       ELSEIF(ierr(i).NE.20.AND.ierr(i).NE.0)THEN
          DO n=1,ensdim
             xf(i,n)=0.
             massfln(i,n)=0.
          END DO
       END IF
100    CONTINUE
    END DO
    RETURN
  END SUBROUTINE cup_forcing_ens1




  SUBROUTINE cup_forcing_ens(aa0,aa1,xaa0,mbdt,dtime,xmb,ierr,            &
       mix,mkx,istart,iend,mjx,xf,j,name,                  &
       maxens,iens,iedt,maxens2,maxens3,mconv,                      &
       omeg,k22,pr_ens,edt,kbcon,massflx,                  &
       iact_old_gr,dir,ensdim,massfln,massfld,iresult)
    CHARACTER *(*) name
    INTEGER k,i,istart,iend,mix,mkx,mjx,j,maxens,maxens3
    INTEGER ensdim,ierr(mix),iens,nall,iedt,maxens2
    INTEGER k22(mix),kbcon(mix),iact_old_gr(mix,mjx)
    REAL aa0(mix),aa1(mix),xaa0(mix,maxens),xmb(mix)
    REAL mbdt(maxens),dtime,edt(mix),dir(mix)
    REAL xf(mix,ensdim)
    REAL pr_ens(mix,ensdim)
    REAL mconv(mix),omeg(mix,mkx)
    REAL xff_ens3(maxens3),xk(maxens),xff0
    REAL massflx(mix)
    REAL massfln(mix,ensdim)
    REAL xomg,massfld 
    INTEGER nens,ne,n,nens3,iresult,iresultd,            &
         iresulte,icoic
    ! srf- icoic is used for choice a specific closure
    ! icoic = 0 -> ensemble (all closures)
    ! icoic = 1 -> arakawa & schubert  (actually due grell)
    ! icoic = 4 -> low level omega
    ! icoic = 7 -> moisture convergence
    ! icoic =10 -> like fritsch chappel or kain fritsch
    !icoic=0
    icoic=0
    nens=0
    !
    !--- large scale forcing
    !
    DO i=istart,iend
       xmb(i)=0.
       IF(name.EQ.'deeps'.AND.ierr(i).GT.995)THEN
          aa0(i)=0.
          ierr(i)=0
       END IF
       IF(ierr(i).EQ.0)THEN
          !
          !--- treatment different for this closure
          !
          IF(name.EQ.'deeps')THEN
             !
             xff0= (aa1(i)-aa0(i))/dtime
             xff_ens3(1)=(aa1(i)-aa0(i))/dtime
             !   
             !--- more original arakawa-schubert (climatologic value of aa0)
             !
             !               xff_ens3(2)=max(0.,(aa1(i)-1500.)/dtime)
             xff_ens3(2)=.9*xff_ens3(1)
             xff_ens3(3)=1.1*xff_ens3(1)
             !
             !--- omeg is in bar/s, mconv done with omeg in pa/s
             !     more like brown (1979), or frank-cohen (199?)
             !
             !srf            xff_ens3(4)=-1.e5*omeg(i,k22(i))/9.81
             !srf            xff_ens3(5)=-1.e5*omeg(i,kbcon(i))/9.81
             !srf            xff_ens3(6)=-1.e5*omeg(i,1)/9.81
             xff_ens3(4)=     -omeg(i,k22(i))/9.81
             xff_ens3(5)=     -omeg(i,kbcon(i))/9.81
             xff_ens3(6)=     -omeg(i,1)/9.81
             DO k=2,kbcon(i)-1
                !srf              xomg=-1.e5*omeg(i,k)/9.81
                xomg=     -omeg(i,k)/9.81
                IF(xomg.GT.xff_ens3(6))xff_ens3(6)=xomg
             END DO
             !
             !--- more like krishnamurti et al.
             !
             xff_ens3(7)=mconv(i)
             xff_ens3(8)=.9*mconv(i)
             xff_ens3(9)=1.1*mconv(i)
             !
             !--- more like fritsch chappel or kain fritsch (plus triggers)
             !
             !               xff_ens3(10)=max(0.,(aa1(i)-1500.)/dtime)
             !               xff_ens3(11)=max(0.,(aa1(i)-1800.)/dtime)
             xff_ens3(10)=aa1(i)/(60.*20.)
             xff_ens3(11)=aa1(i)/(60.*30.)
             xff_ens3(12)=aa1(i)/(60.*40.)
             !
             DO nens=1,maxens
                xk(nens)=(xaa0(i,nens)-aa1(i))/mbdt(nens)
                IF(xk(nens).LE.0.AND.xk(nens).GT.-1.e-9)               &
                     xk(nens)=-1.e-9
                IF(xk(nens).GT.0.AND.xk(nens).LT.1.e-9)                     &
                     xk(nens)=1.e-9
             END DO
             !
             !--- add up all ensembles
             !
             DO ne=1,maxens
                !
                !--- for every xk, we have maxens3 xffs
                !--- iens is from outermost ensemble (most expensive!
                !
                !--- iedt (maxens2 belongs to it)
                !--- is from second, next outermost, not so expensive
                !
                !--- so, for every outermost loop, we have maxens*maxens2*3
                !--- ensembles!!! nall would be 0, if everything is on first
                !--- loop index, then ne would start counting, then iedt, then iens....
                !
                iresultd=0
                iresulte=0
                nall=(iens-1)*maxens3*maxens*maxens2         &
                     +(iedt-1)*maxens*maxens3               &
                     +(ne-1)*maxens3
                !
                !--- check for upwind convection
                !
                iresult=0
                massfld=0.
                CALL cup_direction2(i,j,dir,iact_old_gr,mix,mjx,       &
                     massflx,iresult,1,            &
                     massfld)
                !
                IF(xk(ne).LT.0.AND.xff0.GT.0.)iresultd=1
                iresulte=MAX(iresult,iresultd)
                !                  iresulte=1
                IF(iresulte.EQ.1)THEN
                   !
                   !--- special treatment for stability closures
                   !
                   IF(xff0.GT.0.)THEN
                      xf(i,nall+1)=MAX(0.,-xff_ens3(1)/xk(ne))       &
                           +massfld
                      xf(i,nall+2)=MAX(0.,-xff_ens3(2)/xk(ne))       &
                           +massfld
                      xf(i,nall+3)=MAX(0.,-xff_ens3(3)/xk(ne))       &
                           +massfld
                   ELSE
                      xf(i,nall+1)=massfld
                      xf(i,nall+2)=massfld
                      xf(i,nall+3)=massfld
                   END IF
                   !
                   !--- if iresult.eq.1, following independent of xff
                   !
                   xf(i,nall+4)=MAX(0.,xff_ens3(4) &
                        +massfld)                                 
                   xf(i,nall+5)=MAX(0.,xff_ens3(5) &
                        +massfld)
                   xf(i,nall+6)=MAX(0.,xff_ens3(6) &
                        +massfld)
                   xf(i,nall+7)=MAX(0.,xff_ens3(7) &
                        /pr_ens(i,nall+7))
                   xf(i,nall+8)=MAX(0.,xff_ens3(8) &
                        /pr_ens(i,nall+8))
                   xf(i,nall+9)=MAX(0.,xff_ens3(9) &
                        /pr_ens(i,nall+9))
                   IF(xk(ne).LT.0.)THEN
                      xf(i,nall+10)=MAX(0.,-xff_ens3(10)/xk(ne)) &
                           +massfld
                      xf(i,nall+11)=MAX(0.,-xff_ens3(11)/xk(ne)) &
                           +massfld
                      xf(i,nall+12)=MAX(0.,-xff_ens3(12)/xk(ne)) &
                           +massfld
                   ELSE
                      xf(i,nall+10)=massfld
                      xf(i,nall+11)=massfld
                      xf(i,nall+12)=massfld
                   END IF
                   IF(icoic.GE.1)THEN
                      xf(i,nall+1)=xf(i,nall+icoic)
                      xf(i,nall+2)=xf(i,nall+icoic)
                      xf(i,nall+3)=xf(i,nall+icoic)
                      xf(i,nall+4)=xf(i,nall+icoic)
                      xf(i,nall+5)=xf(i,nall+icoic)
                      xf(i,nall+6)=xf(i,nall+icoic)
                      xf(i,nall+7)=xf(i,nall+icoic)
                      xf(i,nall+8)=xf(i,nall+icoic)
                      xf(i,nall+9)=xf(i,nall+icoic)
                      xf(i,nall+10)=xf(i,nall+icoic)
                      xf(i,nall+11)=xf(i,nall+icoic)
                      xf(i,nall+12)=xf(i,nall+icoic)
                   END IF
                   !
                   !--- store new for next time step
                   !
                   DO nens3=1,maxens3
                      massfln(i,nall+nens3)=edt(i)             &
                           *xf(i,nall+nens3)
                      massfln(i,nall+nens3)=MAX(0.,             &
                           massfln(i,nall+nens3))
                   END DO
                END IF
             END DO
             go to 100
          END IF
       ELSEIF(ierr(i).NE.20.AND.ierr(i).NE.0)THEN
          DO n=1,ensdim
             xf(i,n)=0.
             massfln(i,n)=0.
          END DO
       END IF
100    CONTINUE
    END DO
    RETURN
  END SUBROUTINE cup_forcing_ens




  SUBROUTINE cup_output_ens(xf,ierr,dellat,dellaq,dellaqc,                 &
       outtem,outq,outqc,pre,pw,xmb,ktop,mix,mkx,istart,                 &
       iend,nx,nx2,iens,pr_ens,                           &
       outt_ens,maxens3,ensdim,massfln)

    INTEGER mix,mkx,istart,iend,                                  &
         ensdim,i,k,nx,n,nx2
    REAL xf(mix,ensdim),pr_ens(mix,ensdim),                              &
         outt_ens(mix,ensdim),massfln(mix,ensdim)
    REAL dellat(mix,mkx,nx),dellaq(mix,mkx,nx),outtem(mix,mkx),           &
         outq(mix,mkx),outqc(mix,mkx),pw(mix,mkx,nx),                     &
         pre(mix),xmb(mix),dellaqc(mix,mkx,nx),xfac1(mix)
    INTEGER ktop(mix),ierr(mix),ncount,iens,maxens3
    REAL outtes,ddtes
    REAL dtt,dtq,dtqc,dtpw
    !
    DO k=1,mkx
       DO i=istart,iend
          outtem(i,k) = 0.
          outq(i,k) = 0.
          outqc(i,k) = 0.
       END DO
    END DO
    !
    DO i=istart,iend
       pre(i)  =0.
       xmb(i)  =0.
       xfac1(i)=1.
    END DO
    !
    !--- calculate ensemble average mass fluxes
    !
    DO i=istart,iend
       ncount=0
       xmb(i)=0.
       !
       IF(ierr(i).EQ.0)THEN
          !      
          DO n=(iens-1)*nx*nx2*maxens3+1,iens*nx*nx2*maxens3
             pr_ens(i,n) =   pr_ens(i,n)*xf(i,n)
             outt_ens(i,n) = outt_ens(i,n)*xf(i,n)
             !
             IF(xf(i,n).GT.0.)THEN
                xmb(i) = xmb(i) + xf(i,n)
                ncount = ncount + 1 
             END IF
          END DO
          !
          IF(ncount.GT.0)THEN
             xmb(i)=xmb(i)/float(ncount)
          ELSE
             xmb(i)=0.
             ierr(i)=13
          END IF
          !
       END IF
       !      
    END DO
    !
    !-- now do feedback
    !
    ddtes=250.
    !     if(name.eq.'shal')ddtes=500.
    DO i=istart,iend
       xfac1(i)=xmb(i)
    END DO
    DO k=1,mkx
       DO i=istart,iend
          dtt  =0.
          dtq  =0.
          dtqc =0.
          dtpw =0.
          !
          IF(ierr(i).EQ.0.AND.k.LE.ktop(i))THEN
             DO n=1,nx
                dtt  = dtt  +  dellat(i,k,n)
                dtq  = dtq  +  dellaq(i,k,n)
                dtqc = dtqc + dellaqc(i,k,n)
                dtpw = dtpw +      pw(i,k,n)
                !
             END DO
             outtes = dtt*xmb(i)*86400./float(nx)
             !
             IF (outtes .GT. 2.*ddtes .AND. k.GT.2) THEN
                xmb(i) = 2.*ddtes/outtes * xmb(i)
                outtes = 1.*ddtes
             END IF
             !
             IF (outtes .LT. -ddtes)                THEN
                xmb(i) = -ddtes/outtes * xmb(i)
                outtes = -ddtes
             END IF
             !
             IF (outtes .GT. .5*ddtes .AND. k.LE.2) THEN
                xmb(i) =    ddtes/outtes * xmb(i)
                outtes = .5*ddtes
             END IF
             !
             outtem(i,k) = outtem(i,k) +xmb(i)*dtt /float(nx)
             outq(i,k) =   outq(i,k) +xmb(i)*dtq /float(nx)
             outqc(i,k) =  outqc(i,k) +xmb(i)*dtqc/float(nx)
             pre(i)      = pre(i)      +xmb(i)*dtpw/float(nx)
             ! 
          END IF
       END DO
    END DO
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          xfac1(i)=xmb(i)/xfac1(i)
          DO k=1,ensdim
             massfln(i,k)=massfln(i,k)*xfac1(i)
          END DO
       END IF
    END DO
    !
    RETURN
  END SUBROUTINE cup_output_ens




  SUBROUTINE zero_out(imax,kmax,jmax,ensdim                &
       ,maxens,maxens2                          &
       ,kzdown,kbmax,ierr                            &
       ,k22,kbcon,kb,jmin                          &
       ,ktop,kstabi,kstabm                          &
       ,k22x,kbconx,kbx,ktopx                          &
       ,he,hes,qes                                &
       ,z,tv,dby                                &
       ,qc,qrcd,pwd                                &
       ,pw,edt,edto,edtx                          &
       ,aa1,aa0,xaa0,hkb                          &
       ,hkbo,aad                                &
       ,xhkb,qkb,qkbo,xmb                          &
       ,heo,heso,qeso                                &
       ,zo,tvo,dbyo                                &
       ,qco,qrcdo,pwdo                                &
       ,pwo                                      &
       ,xpwav,xpwev,pwav,pwev                          &
       ,pwavo,pwevo                                &
       ,bu,buo                                      &
       ,xhe,xhes,xqes                                &
       ,xz,xtv,xt                                & 
       ,xq,xqc,xqrcd                                &
       ,xdby,xpwd,xpw                                &
       ,hcd,hcdo,xhcd                                &
       ,qcd,qcdo,xqcd                                &
       ,dbyd,dbydo                                &
       ,hc,hco,xhc                                &
       ,qrc,qrco,xqrc                                &
       ,zu,zuo,xzu                                &
       ,zd,zdo,xzd                                &
       ,dellah,dellaq                                &
       ,dellat,dellaqc                                &
       ,dellat_ens                                &
       ,dellaq_ens                                &
       ,dellaqc_ens                                &
       ,pwo_ens                                      &
       ,qes_cup,q_cup                                &
       ,he_cup,hes_cup                                &
       ,z_cup,p_cup                                &
       ,gamma_cup,t_cup                                &
       ,qeso_cup,qo_cup                                &
       ,heo_cup,heso_cup                          &
       ,zo_cup,po_cup                                &
       ,gammao_cup,tn_cup                          &
       ,xqes_cup,xq_cup                                &
       ,xhe_cup,xhes_cup                          &
       ,xz_cup,xt_cup                                &
       ,cd,cdd                                      &
       ,scr1,cap_max                                &
       ,mbdt_ens,xaa0_ens                          &
       ,edt_ens,edtc                                &
       ,cwf,pwf                                      &
       ,pwdf,eddt,predb                                &
       ,xktop,xkbas,xmass                          &
       ,xf                                      &
       ,xf_ens,pr_ens                                &
       ,outt_ens                                &
       ,vshear,sdp,vws )
    !      
    INTEGER imax,kmax,jmax,ensdim                       &
         ,maxens,maxens2
    !
    INTEGER                                       &
         kzdown(imax),kbmax(imax),ierr(imax)              &
         ,k22(imax),kbcon(imax),kb(imax),jmin(imax)         &
         ,ktop(imax),kstabi(imax),kstabm(imax)              &
         ,k22x(imax),kbconx(imax),kbx(imax),ktopx(imax)
    !
    REAL                                      & 
         he(imax,kmax),hes(imax,kmax),qes(imax,kmax)         &
         ,z(imax,kmax),tv(imax,kmax),dby(imax,kmax)        &
         ,qc(imax,kmax),qrcd(imax,kmax),pwd(imax,kmax)        &
         ,pw(imax,kmax),edt(imax),edto(imax),edtx(imax)        &
         ,aa1(imax),aa0(imax),xaa0(imax),hkb(imax)        &
         ,hkbo(imax),aad(imax)                          &
         ,xhkb(imax),qkb(imax),qkbo(imax),xmb(imax)        &
         ,heo(imax,kmax),heso(imax,kmax),qeso(imax,kmax)  &
         ,zo(imax,kmax),tvo(imax,kmax),dbyo(imax,kmax)        &
         ,qco(imax,kmax),qrcdo(imax,kmax),pwdo(imax,kmax) &
         ,pwo(imax,kmax)                          &
         ,xpwav(imax),xpwev(imax),pwav(imax),pwev(imax)        &
         ,pwavo(imax),pwevo(imax)                    &
         ,bu(imax),buo(imax)
    !
    REAL                                     &
         xhe(imax,kmax),xhes(imax,kmax),xqes(imax,kmax)       &
         ,xz(imax,kmax),xtv(imax,kmax),xt(imax,kmax)       &
         ,xq(imax,kmax),xqc(imax,kmax),xqrcd(imax,kmax)       &
         ,xdby(imax,kmax),xpwd(imax,kmax),xpw(imax,kmax) &
         ,hcd(imax,kmax),hcdo(imax,kmax),xhcd(imax,kmax) &
         ,qcd(imax,kmax),qcdo(imax,kmax),xqcd(imax,kmax) &
         ,dbyd(imax,kmax),dbydo(imax,kmax)             &
         ,hc(imax,kmax),hco(imax,kmax),xhc(imax,kmax)       &
         ,qrc(imax,kmax),qrco(imax,kmax),xqrc(imax,kmax) &
         ,zu(imax,kmax),zuo(imax,kmax),xzu(imax,kmax)       &
         ,zd(imax,kmax),zdo(imax,kmax),xzd(imax,kmax)
    !
    REAL                                       &
         dellah(imax,kmax),dellaq(imax,kmax)                  &
         ,dellat(imax,kmax),dellaqc(imax,kmax)
    !
    REAL                                       &
         dellat_ens(imax,kmax,maxens2)                       &
         ,dellaq_ens(imax,kmax,maxens2)                       &
         ,dellaqc_ens(imax,kmax,maxens2)                 &
         ,pwo_ens(imax,kmax,maxens2)
    !
    REAL                                       &
         qes_cup(imax,kmax),q_cup(imax,kmax)                 &
         ,he_cup(imax,kmax),hes_cup(imax,kmax)                &
         ,z_cup(imax,kmax),p_cup(imax,kmax)                &
         ,gamma_cup(imax,kmax),t_cup(imax,kmax)          &
         ,qeso_cup(imax,kmax),qo_cup(imax,kmax)           &
         ,heo_cup(imax,kmax),heso_cup(imax,kmax)          &
         ,zo_cup(imax,kmax),po_cup(imax,kmax)                 &
         ,gammao_cup(imax,kmax),tn_cup(imax,kmax)          &
         ,xqes_cup(imax,kmax),xq_cup(imax,kmax)            &
         ,xhe_cup(imax,kmax),xhes_cup(imax,kmax)           &
         ,xz_cup(imax,kmax),xt_cup(imax,kmax)
    !
    REAL                                      &
         cd(imax,kmax),cdd(imax,kmax)                    &
         ,scr1(imax,kmax),cap_max(imax)                    &
         ,mbdt_ens(maxens),xaa0_ens(imax,maxens)        &
         ,edt_ens(maxens2),edtc(imax,maxens2)
    !
    REAL     cwf(imax,jmax),pwf(imax,jmax)                    &
         ,pwdf(imax,jmax),eddt(imax,jmax),predb(imax,jmax)       &
         ,xktop(imax,jmax),xkbas(imax,jmax),xmass(imax,jmax)      &
         ,xf(imax,ensdim)
    !
    REAL                                       &
         xf_ens(imax,ensdim),pr_ens(imax,jmax,ensdim)      &
         ,outt_ens(imax,ensdim)
    !
    REAL vshear(imax),sdp(imax),vws(imax)
    !
    !integer
    kzdown          =0
    kbmax           =0
    ierr            =0
    k22             =0
    kbcon          =0
    kb                =0
    jmin            =0
    ktop          =0
    kstabi         =0
    kstabm          =0
    k22x          =0
    kbconx         =0
    kbx               =0
    ktopx         =0
    !real
    he               =0.
    hes               =0.
    qes               =0.
    z               =0.
    tv              =0.
    dby               =0.
    qc               =0.
    qrcd          =0.
    pwd               =0.
    pw               =0.
    edt               =0.
    edto          =0.
    edtx          =0.
    aa1             =0.
    aa0               =0.
    xaa0          =0.
    hkb               =0.
    hkbo          =0.
    aad               =0.
    xhkb          =0.
    qkb               =0.
    qkbo          =0.
    xmb                =0.
    heo                   =0.
    heso              =0.
    qeso              =0.
    zo                   =0.
    tvo                   =0.
    dbyo              =0.
    qco                   =0.
    qrcdo             =0.
    pwdo            =0.
    pwo                 =0.
    xpwav           =0.
    xpwev           =0.
    pwav          =0.
    pwev          =0.
    pwavo         =0.
    pwevo         =0.
    bu               =0.
    buo               =0.
    xhe               =0.
    xhes          =0.
    xqes          =0.
    xz               =0.
    xtv               =0.
    xt               =0.
    xq               =0.
    xqc               =0.
    xqrcd         =0.
    xdby          =0.
    xpwd          =0.
    xpw               =0.
    hcd               =0.
    hcdo          =0.
    xhcd          =0.
    qcd               =0.
    qcdo          =0.
    xqcd            =0.
    dbyd            =0.
    dbydo           =0.
    hc                 =0.
    hco                 =0.
    xhc                 =0.
    qrc                 =0.
    qrco            =0.
    xqrc            =0.
    zu                 =0.
    zuo                 =0.
    xzu                 =0.
    zd                 =0.
    zdo                 =0.
    xzd                 =0.
    dellah          =0.
    dellaq          =0.
    dellat          =0.
    dellaqc         =0.
    dellat_ens      =0.
    dellaq_ens      =0.
    dellaqc_ens     =0.
    pwo_ens         =0.
    qes_cup         =0.
    q_cup           =0.
    he_cup          =0.
    hes_cup         =0.
    z_cup           =0.
    p_cup         =0.
    gamma_cup         =0.
    t_cup         =0.
    qeso_cup         =0.
    qo_cup         =0.
    heo_cup         =0.
    heso_cup         =0.
    zo_cup         =0.
    po_cup         =0.
    gammao_cup         =0.
    tn_cup         =0.
    xqes_cup         =0.
    xq_cup         =0.
    xhe_cup         =0.
    xhes_cup         =0.
    xz_cup         =0.
    xt_cup         =0.
    cd               =0.
    cdd               =0.
    scr1          =0.
    cap_max         =0.
    mbdt_ens         =0.
    xaa0_ens         =0.
    edt_ens         =0.
    edtc          =0.
    cwf               =0.
    pwf             =0.
    pwdf          =0.
    eddt          =0.
    predb         =0.
    xktop         =0.
    xkbas         =0.
    xmass         =0.
    xf               =0.
    xf_ens         =0.
    pr_ens         =0.
    outt_ens         =0.
    vshear         =0.
    sdp               =0.
    vws               =0.
    !
    RETURN
  END SUBROUTINE zero_out




  SUBROUTINE cup_dd_he(hes_cup,hcd,z_cup,cdd,entr,         &
       jmin,ierr,mix,mkx,istart,iend,he,dby)
    INTEGER mix,mkx,istart,iend,i,k,ki
    REAL hcd(mix,mkx),z_cup(mix,mkx),         &
         cdd(mix,mkx),he(mix,mkx),dby(mix,mkx),                 &
         hes_cup(mix,mkx)
    REAL entr,dz
    INTEGER jmin(mix),ierr(mix)
    DO k=2,mkx
       DO i=istart,iend
          dby(i,k)=0.
          IF(ierr(i).EQ.0)THEN
             hcd(i,k)=hes_cup(i,k)
          END IF
       END DO
    END DO
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          k=jmin(i)
          hcd(i,k)=hes_cup(i,k)
          dby(i,k)=hcd(i,jmin(i))-hes_cup(i,k)
          !
          DO ki=jmin(i)-1,1,-1
             dz=z_cup(i,ki+1)-z_cup(i,ki)
             hcd(i,ki)=(hcd(i,ki+1)*(1.-.5*cdd(i,ki)*dz)      &
                  +entr*dz*he(i,ki)                        &
                  )/(1.+entr*dz-.5*cdd(i,ki)*dz)
             dby(i,ki)=hcd(i,ki)-hes_cup(i,ki)
          END DO
       END IF
       !--- end loop over i
    END DO
    RETURN
  END SUBROUTINE cup_dd_he




  SUBROUTINE cup_dd_moisture(zd,hcd,hes_cup,qcd,qes_cup,        &
       pwd,q_cup,z_cup,cdd,entr,jmin,ierr,                   &
       gamma_cup,pwev,mix,mkx,istart,iend,bu,qrcd,             &
       q,iloop)
    INTEGER mix,mkx,istart,iend,i,k,ki
    REAL zd(mix,mkx),qcd(mix,mkx),pwd(mix,mkx),                     &
         pwev(mix),qrcd(mix,mkx)
    REAL hes_cup(mix,mkx),hcd(mix,mkx),qes_cup(mix,mkx),        &
         q_cup(mix,mkx),z_cup(mix,mkx),cdd(mix,mkx),              &
         gamma_cup(mix,mkx),q(mix,mkx)
    REAL xl,bu(mix),entr,dz,dqeva,dh
    INTEGER jmin(mix),ierr(mix),iloop
    xl=2.5e6
    DO i=istart,iend
       bu(i)=0.
       pwev(i)=0.
    END DO
    DO k=1,mkx
       DO i=istart,iend
          qcd(i,k)=0.
          qrcd(i,k)=0.
          pwd(i,k)=0.
       END DO
    END DO
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          k=jmin(i)
          dz=z_cup(i,k+1)-z_cup(i,k)
          qcd(i,k)=q_cup(i,k)
          qrcd(i,k)=qes_cup(i,k)
          pwd(i,jmin(i))=MIN(0.,qcd(i,k)-qrcd(i,k))
          pwev(i)=pwev(i)+pwd(i,jmin(i))
          qcd(i,k)=qes_cup(i,k)
          dh=hcd(i,k)-hes_cup(i,k)
          bu(i)=dz*dh
          !
          DO ki=jmin(i)-1,1,-1
             dz=z_cup(i,ki+1)-z_cup(i,ki)
             qcd(i,ki)=(qcd(i,ki+1)*(1.-.5*cdd(i,ki)*dz)               &
                  +entr*dz*q(i,ki)                               &
                  )/(1.+entr*dz-.5*cdd(i,ki)*dz)
             !
             !--- to be negatively buoyant, hcd should be smaller than hes!
             !
             dh=hcd(i,ki)-hes_cup(i,ki)
             bu(i)=bu(i)+dz*dh
             qrcd(i,ki)=qes_cup(i,ki)+(1./xl)*(gamma_cup(i,ki)          &
                  /(1.+gamma_cup(i,ki)))*dh
             dqeva=qcd(i,ki)-qrcd(i,ki)
             IF(dqeva.GT.0.) dqeva=0.
             pwd(i,ki)=zd(i,ki)*dqeva
             qcd(i,ki)=qrcd(i,ki)
             pwev(i)=pwev(i)+pwd(i,ki)
          END DO
          !
          !--- end loop over i
          IF(bu(i).GE.0.AND.iloop.EQ.1)THEN
             !         print *,'problem with buoy in cup_dd_moisture',i
             ierr(i)=7
          END IF
       END IF
    END DO
    RETURN
  END SUBROUTINE cup_dd_moisture




  SUBROUTINE cup_dd_nms(zd,z_cup,cdd,entr,jmin,ierr,      &
       mix,mkx,istart,iend,itest,kdet,z1)
    INTEGER mix,mkx,istart,iend,i,k,ki
    REAL zd(mix,mkx),z_cup(mix,mkx),cdd(mix,mkx),z1(mix)
    REAL entr,dz,a,perc
    INTEGER jmin(mix),ierr(mix),itest,kdet(mix)
    !
    !--- perc is the percentage of mass left when hitting the ground
    !
    perc=.03
    !
    DO k=1,mkx
       DO i=istart,iend
          zd(i,k)=0.
          IF(itest.EQ.0)cdd(i,k)=0.
       END DO
    END DO
    a=1.-perc
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          zd(i,jmin(i))=1.
          !
          !--- integrate downward, specify detrainment(cdd)!
          !
          DO ki=jmin(i)-1,1,-1
             dz=z_cup(i,ki+1)-z_cup(i,ki)
             IF(ki.LE.kdet(i).AND.itest.EQ.0)THEN
                cdd(i,ki)=entr+(1.- (a*(z_cup(i,ki)-z1(i))      &
                     +perc*(z_cup(i,kdet(i))-z1(i)) )            &
                     /(a*(z_cup(i,ki+1)-z1(i))                  &
                     +perc*(z_cup(i,kdet(i))-z1(i))))/dz
             END IF
             zd(i,ki)=zd(i,ki+1)*(1.+(entr-cdd(i,ki))*dz)
          END DO
          !
       END IF
       !--- end loop over i
    END DO
    RETURN
  END SUBROUTINE cup_dd_nms




  SUBROUTINE cup_dd_aa0(edt,ierr,aa0,jmin,gamma_cup,t_cup,         &
       hcd,hes_cup,z,mix,mkx,istart,iend,zd)
    INTEGER i,k,kk,mix,mkx,istart,iend
    REAL aa0(mix),gamma_cup(mix,mkx),t_cup(mix,mkx),              &
         z(mix,mkx),hes_cup(mix,mkx),        &
         edt(mix),zd(mix,mkx),hcd(mix,mkx)
    INTEGER jmin(mix),ierr(mix)
    REAL dz
    DO k=1,mkx-1
       DO i=istart,iend
          IF(ierr(i).EQ.0.AND.k.LT.jmin(i))THEN
             kk=jmin(i)-k
             !
             !--- original
             !
             dz=(z(i,kk)-z(i,kk+1))
             aa0(i)=aa0(i)+zd(i,kk)*edt(i)*dz*(9.81/(1004.*t_cup(i,kk)))  &
                  *((hcd(i,kk)-hes_cup(i,kk))/(1.+gamma_cup(i,kk)))
          END IF
       END DO
    END DO
    RETURN
  END SUBROUTINE cup_dd_aa0





  SUBROUTINE cup_dd_edt(ierr,us,vs,z,ktop,kbcon,edt,p,pwav   &
       ,pwev,mix,mkx,istart,iend,edtmax,edtmin,maxens2,edtc  &
       ,vshear,sdp,vws)
    INTEGER i,kk,k,mix,mkx,istart,iend,maxens2
    REAL us(mix,mkx),vs(mix,mkx),z(mix,mkx),p(mix,mkx),      &
         pwav(mix),pwev(mix)
    REAL edt(mix),edtc(mix,maxens2),einc
    INTEGER ktop(mix),kbcon(mix),ierr(mix)
    REAL pef,vshear(mix),sdp(mix),vws(mix),edtmax,edtmin
    REAL pefb,prezk,zkbc
    !
    !--- determine downdraft strength in terms of windshear
    !
    ! */ calculate an average wind shear over the depth of the cloud
    !
    DO i=istart,iend
       edt(i)=0.
       vws(i)=0.
       sdp(i)=0.
       vshear(i)=0.
    END DO
    DO kk = 1,mkx-1
       DO i=istart,iend
          IF(ierr(i).NE.0)go to 62
          IF (kk .LE. min0(ktop(i),mkx-1) .AND. kk .GE. kbcon(i)) THEN
             vws(i) = vws(i)+                                            &
                  (ABS((us(i,kk+1)-us(i,kk))/(z(i,kk+1)-z(i,kk)))              &
                  +   ABS((vs(i,kk+1)-vs(i,kk))/(z(i,kk+1)-z(i,kk)))) *              &
                  (p(i,kk) - p(i,kk+1))
             sdp(i) = sdp(i) + p(i,kk) - p(i,kk+1)
          END IF
          IF (kk .EQ. mkx-1)                                           &
               vshear(i) = 1.e3 * vws(i) / sdp(i)
62        CONTINUE
       END DO
    END DO
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          pef=(1.591-.639*vshear(i)+.0953*(vshear(i)**2)      &
               -.00496*(vshear(i)**3))
          IF(pef.GT.edtmax)pef=edtmax
          IF(pef.LT.edtmin)pef=edtmin
          !
          !--- cloud base precip efficiency
          !
          zkbc=z(i,kbcon(i))*3.281e-3
          prezk=.02
          IF(zkbc.GT.3.)THEN
             prezk=.96729352+zkbc*(-.70034167+zkbc*(.162179896+zkbc*(-  &
                  1.2569798e-2+zkbc*(4.2772e-4-zkbc*5.44e-6))))
          END IF
          IF(zkbc.GT.25)THEN
             prezk=2.4
          END IF
          pefb=1./(1.+prezk)
          IF(pefb.GT.edtmax)pefb=edtmax
          IF(pefb.LT.edtmin)pefb=edtmin
          edt(i)=1.-.5*(pefb+pef)
          !--- edt here is 1-precipeff!
          !           einc=(1.-edt(i))/float(maxens2)
          einc=edt(i)/float(maxens2+1)
          DO k=1,maxens2
             edtc(i,k)=edt(i)+float(k-maxens2/2-1)*einc
             edtc(i,k)=edt(i)-float(k)*einc
          END DO
       END IF
    END DO
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          DO k=1,maxens2
             edtc(i,k)=-edtc(i,k)*pwav(i)/pwev(i)
             IF(edtc(i,k).GT.edtmax)edtc(i,k)=edtmax
             IF(edtc(i,k).LT.edtmin)edtc(i,k)=edtmin
          END DO
       END IF
    END DO
    RETURN
  END SUBROUTINE cup_dd_edt





  SUBROUTINE cup_env(z,qes,he,hes,t,q,p,z1,mix,mkx,       &
       istart,iend,psur,ierr,tcrit,itest)
    INTEGER mix,mkx,i,k,istart,iend,iph
    REAL z(mix,mkx),p(mix,mkx),qes(mix,mkx),he(mix,mkx),      &
         hes(mix,mkx),t(mix,mkx),q(mix,mkx),z1(mix),            &
         psur(mix),tv(mix,mkx)
    REAL ae(2),be(2),ht(2),tcrit,e,tvbar
    INTEGER ierr(mix),itest
    REAL xl,cp
    xl=2.5e06
    cp=1004.
    ht(1)=xl/cp
    ht(2)=2.834e6/cp
    be(1)=.622*ht(1)/.286
    ae(1)=be(1)/273.+alog(610.71)
    be(2)=.622*ht(2)/.286
    ae(2)=be(2)/273.+alog(610.71)
    DO k=1,mkx
       DO i=istart,iend
          IF(ierr(i).EQ.0)THEN
             !sgb - iph is for phase, dependent on tcrit (water or ice)
             iph = 1
             IF(t(i,k).LE.tcrit)  iph = 2
             e = EXP(ae(iph)-be(iph)/t(i,k))
             qes(i,k) = .622*e/(100.*p(i,k)-e)
             IF(qes(i,k) .LE. 1.e-08)        qes(i,k)=1.e-08
             IF(q(i,k)   .GT. qes(i,k))        q(i,k)=qes(i,k)
             tv(i,k) = t(i,k)+.608*q(i,k)*t(i,k)
          END IF
       END DO
    END DO
    !
    !--- z's are calculated with changed h's and q's and t's
    !--- if itest=2
    !
    IF(itest.NE.2)THEN
       DO i=istart,iend
          IF(ierr(i).EQ.0)THEN
             z(i,1)=MAX(0.,z1(i))-(alog(p(i,1))-      &
                  alog(psur(i)))*287.*tv(i,1)/9.81
             !
          END IF
       END DO
       !
       !--- calculate heights
       DO k=2,mkx
          DO i=istart,iend
             IF(ierr(i).EQ.0)THEN
                tvbar=.5*tv(i,k)+.5*tv(i,k-1)
                z(i,k)=z(i,k-1)-(alog(p(i,k))-       &
                     alog(p(i,k-1)))*287.*tvbar/9.81
                !
             END IF
          END DO
       END DO
    ELSE
       DO k=1,mkx
          DO i=istart,iend
             IF(ierr(i).EQ.0)THEN
                z(i,k)=(he(i,k)-1004.*t(i,k)-2.5e6*q(i,k))/9.81
                z(i,k)=MAX(1.e-3,z(i,k))
             END IF
          END DO
       END DO
    END IF
    !
    !--- calculate moist static energy - he
    !--- saturated moist static energy - hes
    !
    DO k=1,mkx
       DO i=istart,iend
          IF(ierr(i).EQ.0)THEN
             IF(itest.EQ.0)he(i,k)=9.81*z(i,k)+1004.*t(i,k)+2.5e06*q(i,k)
             hes(i,k)=9.81*z(i,k)+1004.*t(i,k)+2.5e06*qes(i,k)
             IF(he(i,k).GE.hes(i,k))he(i,k)=hes(i,k)
             !
          END IF
       END DO
    END DO
    !
    RETURN
  END SUBROUTINE cup_env





  SUBROUTINE cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,       &
       q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,             &
       psur,mix,mkx,istart,iend,ierr,z1)
    INTEGER i,k,mix,mkx,istart,iend
    REAL qes_cup(mix,mkx),q_cup(mix,mkx),he_cup(mix,mkx),              &
         hes_cup(mix,mkx),z_cup(mix,mkx),p_cup(mix,mkx),              &
         gamma_cup(mix,mkx),t_cup(mix,mkx)
    REAL qes(mix,mkx),q(mix,mkx),he(mix,mkx),                          &
         hes(mix,mkx),z(mix,mkx),p(mix,mkx),                        &
         t(mix,mkx),psur(mix),z1(mix)
    INTEGER ierr(mix)
    REAL xl,rv,cp
    xl=2.5e6
    rv=461.9
    cp=1004.
    DO k=2,mkx
       DO i=istart,iend
          IF(ierr(i).EQ.0)THEN
             qes_cup(i,k) = .5*(qes(i,k-1) + qes(i,k))
             q_cup(i,k) = .5*(  q(i,k-1) +   q(i,k))
             hes_cup(i,k) = .5*(hes(i,k-1) + hes(i,k))
             he_cup(i,k) = .5*( he(i,k-1) +  he(i,k))
             IF(he_cup(i,k) .GT. hes_cup(i,k)) he_cup(i,k) = hes_cup(i,k)
             !
             z_cup(i,k) = .5*(z(i,k-1) + z(i,k))
             p_cup(i,k) = .5*(p(i,k-1) + p(i,k))
             t_cup(i,k) = .5*(t(i,k-1) + t(i,k))
             !
             gamma_cup(i,k)=(xl/cp)*(xl/(rv*t_cup(i,k)        &
                  *t_cup(i,k)))*qes_cup(i,k)
          END IF
       END DO
    END DO
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          qes_cup(i,1) =  qes(i,1)
          q_cup(i,1) =    q(i,1)
          hes_cup(i,1) =  hes(i,1)
          he_cup(i,1) =   he(i,1)
          !      
          z_cup(i,1) = .5*( z(i,1) +   z1(i))
          p_cup(i,1) = .5*( p(i,1) + psur(i))
          t_cup(i,1) =      t(i,1)
          !      
          gamma_cup(i,1)=xl/cp*(xl/(rv*t_cup(i,1)            &
               *t_cup(i,1)))*qes_cup(i,1)
       END IF
    END DO
    !
    RETURN
  END SUBROUTINE cup_env_clev



  SUBROUTINE cup_direction2(i,j,dir,id,mix,mjx,massflx,         &
       iresult,imass,massfld)
    INTEGER mix,mjx,i,j,iresult,imass
    INTEGER ia,ib,ja,jb
    INTEGER id(mix,mjx)
    REAL dir(mix),massflx(mix,mjx)
    REAL massfld
    REAL diff
    IF(imass.EQ.1)THEN
       massfld=massflx(i,j)
    END IF
    iresult=0
    !      return
    !
    diff=22.5
    IF(dir(i).LT.22.5)dir(i)=360.+dir(i)
    IF(id(i,j).EQ.1)iresult=1
    !      ja=max(2,j-1)
    !      ia=max(2,i-1)
    !      jb=min(mjx-1,j+1)
    !      ib=min(mix-1,i+1)
    ja=j-1
    ia=i-1
    jb=j+1
    ib=i+1
    IF(dir(i).GT.90.-diff.AND.dir(i).LE.90.+diff)THEN
       !
       !--- steering flow from the east
       IF(id(ib,j).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(ib,j),massflx(i,j))
          END IF
          RETURN
       END IF
    ELSE IF(dir(i).GT.135.-diff.AND.dir(i).LE.135.+diff)THEN
       !
       !--- steering flow from the south-east
       IF(id(ib,ja).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(ib,ja),massflx(i,j))
          END IF
          RETURN
       END IF
       !
       !--- steering flow from the south
    ELSE IF(dir(i).GT.180.-diff.AND.dir(i).LE.180.+diff)THEN
       IF(id(i,ja).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(i,ja),massflx(i,j))
          END IF
          RETURN
       END IF
       !
       !--- steering flow from the south west
    ELSE IF(dir(i).GT.225.-diff.AND.dir(i).LE.225.+diff)THEN
       IF(id(ia,ja).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(ia,ja),massflx(i,j))
          END IF
          RETURN
       END IF
       !
       !--- steering flow from the west
    ELSE IF(dir(i).GT.270.-diff.AND.dir(i).LE.270.+diff)THEN
       IF(id(ia,j).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(ia,j),massflx(i,j))
          END IF
          RETURN
       END IF
       !
       !--- steering flow from the north-west
    ELSE IF(dir(i).GT.305.-diff.AND.dir(i).LE.305.+diff)THEN
       IF(id(ia,jb).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(ia,jb),massflx(i,j))
          END IF
          RETURN
       END IF
       !
       !--- steering flow from the north
    ELSE IF(dir(i).GT.360.-diff.AND.dir(i).LE.360.+diff)THEN
       IF(id(i,jb).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(i,jb),massflx(i,j))
          END IF
          RETURN
       END IF
       !
       !--- steering flow from the north-east
    ELSE IF(dir(i).GT.45.-diff.AND.dir(i).LE.45.+diff)THEN
       IF(id(ib,jb).EQ.1)THEN
          iresult=1
          IF(imass.EQ.1)THEN
             massfld=MAX(massflx(ib,jb),massflx(i,j))
          END IF
          RETURN
       END IF
    END IF
    !
    RETURN
  END SUBROUTINE cup_direction2



  SUBROUTINE cup_ktop(ilo,dby,kbcon,ktop,            &
       mix,mkx,istart,iend,ierr)
    INTEGER mix,mkx,i,k,istart,iend,ilo
    REAL dby(mix,mkx)
    INTEGER ierr(mix),kbcon(mix),ktop(mix)
    !
    DO i=istart,iend
       ktop(i)=1
       IF(ierr(i).EQ.0)THEN
          !
          DO k=kbcon(i)+1,mkx-2
             !           
             IF(dby(i,k).LE.0.)THEN
                ktop(i)=k-1
                go to 41
             END IF
          END DO
          IF(ilo.EQ.1)ierr(i)=5
          IF(ilo.EQ.2)ierr(i)=998
          CYCLE
41        CONTINUE
          DO k=ktop(i)+1,mkx
             dby(i,k)=0.
          END DO
       END IF
    END DO
    RETURN
  END SUBROUTINE cup_ktop



  SUBROUTINE cup_kbcon(iloop,k22,kbcon,he_cup,hes_cup,      &
       mix,mkx,istart,iend,ierr,kbmax,p_cup,cap_max)
    INTEGER i,mix,mkx,istart,iend,iloop
    INTEGER kbcon(mix),k22(mix),ierr(mix),kbmax(mix)
    REAL he_cup(mix,mkx),hes_cup(mix,mkx),p_cup(mix,mkx)
    REAL pbcdif,cap_max(mix)
    !srf
    !      integer k
    !srf
    !
    !--- determine the level of convective cloud base  - kbcon
    !
    DO i=istart,iend
       kbcon(i)=1
       IF(ierr(i).NE.0)go to 27
       kbcon(i)=k22(i)
       !
       go to 32
31     CONTINUE
       kbcon(i)=kbcon(i)+1
       IF(kbcon(i).GT.kbmax(i)+2)THEN
          IF(iloop.EQ.1)ierr(i)=3
          IF(iloop.EQ.2)ierr(i)=997
          !srf
          !       print*,'2',i,ierr(i),k22(i),kbcon(i),cap_max(i)
          !srf
          !
          go to 27
       END IF
32     CONTINUE
       IF(he_cup(i,k22(i)).LT.hes_cup(i,kbcon(i)))go to 31
       !
       !     cloud base pressure and max moist static energy pressure
       !     i.e., the depth (in mb) of the layer of negative buoyancy
       !
       IF(kbcon(i)-k22(i).EQ.1) THEN
          !srf
          !       print*,'2',i,ierr(i),k22(i),kbcon(i),cap_max(i)
          !srf
          go to 27
       END IF
       !            
       pbcdif=-p_cup(i,kbcon(i))+p_cup(i,k22(i))
       IF(pbcdif.GT.cap_max(i))THEN
          k22(i)=k22(i)+1
          kbcon(i)=k22(i)
          go to 32
       END IF
27     CONTINUE
    END DO
    !
    RETURN
  END SUBROUTINE cup_kbcon



  SUBROUTINE cup_kbcon_cin(iloop,k22,kbcon,he_cup,hes_cup,   &
       z,tmean,qes,                                   &
       mix,mkx,istart,iend,ierr,kbmax,cap_max)
    !srf      include 'metcon'
    INTEGER i,mix,mkx,istart,iend,iloop
    INTEGER kbcon(mix),k22(mix),ierr(mix),kbmax(mix)
    REAL he_cup(mix,mkx),hes_cup(mix,mkx)
    REAL z(mix,mkx),tmean(mix,mkx),qes(mix,mkx)
    REAL cap_max(mix)
    REAL cin,cin_max,dh,tprim,gamma
    !
    !srf - substitue o metcon include file
    REAL lovcp_p,lv_p,rv_p,cpd_p
    REAL xl,rv,cp
    xl=2.5e6
    rv=461.9
    cp=1004.
    lovcp_p = xl/cp
    lv_p    = xl
    rv_p    = rv
    cpd_p   = cp
    !srf- -----------------------------------
    !
    !--- determine the level of convective cloud base  - kbcon
    !
    DO i=istart,iend
       cin_max=-cap_max(i)
       kbcon(i)=1
       cin = 0.
       IF(ierr(i).NE.0)go to 27
       kbcon(i)=k22(i)
       go to 32
31     CONTINUE
       kbcon(i)=kbcon(i)+1
       IF(kbcon(i).GT.kbmax(i)+2)THEN
          IF(iloop.EQ.1)ierr(i)=3
          IF(iloop.EQ.2)ierr(i)=997
          !
          !srf
          !      print*,'kcon_cin ',i,k22(i),kbcon(i),ierr(i),cin
          !srf
          !
          go to 27
       END IF
32     CONTINUE
       dh      = he_cup(i,k22(i)) - hes_cup(i,kbcon(i))
       IF (dh.LT. 0.) THEN
          gamma = lovcp_p*(lv_p/(rv_p*(tmean(i,k22(i))**2)))*qes(i,k22(i))
          tprim = dh/(cpd_p*(1.+gamma))
          !
          cin = cin + 9.8066 * tprim                           &
               *(z(i,k22(i))-z(i,k22(i)-1)) / tmean(i,k22(i))
          !srf
          !        print*,i,k22(i),tmean(i,k22(i)),z(i,k22(i)),tprim,cin
          !        print*, he_cup(i,k22(i)),hes_cup(i,kbcon(i))
          !srf
          !
          go to 31
       END IF
       !
       !     if negative energy in negatively buoyant layer
       !       exceeds convective inhibition (cin) threshold,
       !       then set k22 level one level up and see if that
       !       will work.
       !
       IF(cin.LT.cin_max)THEN
          k22(i)=k22(i)+1
          kbcon(i)=k22(i)
          go to 32
       END IF
       !
27     CONTINUE
    END DO
    !
    RETURN
  END SUBROUTINE cup_kbcon_cin



  SUBROUTINE minimi(array,mxx,mzx,ks,kend,kt,istart,iend,ierr)
    !
    !
    INTEGER mxx,mzx,istart,iend,i,k
    REAL array(mxx,mzx),x(mxx)
    INTEGER kt(mxx),ks(mxx),kend(mxx),kstop,ierr(mxx)
    !
    DO i=istart,iend
       kt(i)=ks(i)
       IF(ierr(i).EQ.0)THEN
          x(i)=array(i,ks(i))
          kstop=MAX(ks(i)+1,kend(i))
          DO k=ks(i)+1,kstop
             IF(array(i,k).LT.x(i)) THEN
                x(i)=array(i,k)
                kt(i)=k
             END IF
          END DO
       END IF
    END DO
    !
    RETURN
  END SUBROUTINE minimi



  SUBROUTINE maximi(array,mxx,mzx,ks,ke,maxx,istart,iend,ierr)
    !
    !
    INTEGER mxx,mzx,ks,ke(mxx),istart,iend,i,k
    REAL array(mxx,mzx),x(mxx)
    REAL xar
    INTEGER maxx(mxx),ierr(mxx)
    !
    DO i=istart,iend
       maxx(i)=ks
       IF(ierr(i).EQ.0)THEN
          x(i)=array(i,ks)
          !
          DO k=ks,ke(i)
             xar=array(i,k)
             IF(xar.GE.x(i)) THEN
                x(i)=xar
                maxx(i)=k
             END IF
          END DO
       END IF
    END DO
    !
    RETURN
  END SUBROUTINE maximi



  SUBROUTINE cup_up_he(k22,hkb,z_cup,cd,entr,he_cup,hc,mix,mkx,  &
       kbcon,ierr,istart,iend,dby,he,hes_cup)
    INTEGER i,k,mix,mkx,istart,iend
    REAL he_cup(mix,mkx),hc(mix,mkx),hkb(mix),                      &
         z_cup(mix,mkx),cd(mix,mkx),dby(mix,mkx),he(mix,mkx),       &
         hes_cup(mix,mkx)
    INTEGER kbcon(mix),ierr(mix),k22(mix)
    REAL entr,dz
    !
    !--- moist static energy inside cloud
    !
    DO i=istart,iend
       IF(ierr(i).EQ.0.)THEN
          hkb(i)=he_cup(i,k22(i))
          DO k=1,k22(i)
             hc(i,k)=he_cup(i,k)
             dby(i,k)=0.
          END DO
          DO k=k22(i),kbcon(i)-1
             hc(i,k)=hkb(i)
             dby(i,k)=0.
          END DO
          k=kbcon(i)
          hc(i,k)=hkb(i)
          dby(i,kbcon(i))=hkb(i)-hes_cup(i,k)
       END IF
    END DO
    DO k=2,mkx-1
       DO i=istart,iend
          IF(k.GT.kbcon(i).AND.ierr(i).EQ.0.)THEN
             dz=z_cup(i,k)-z_cup(i,k-1)
             hc(i,k)=(hc(i,k-1)*(1.-.5*cd(i,k)*dz)+entr*        &
                  dz*he(i,k-1))/(1.+entr*dz-.5*cd(i,k)*dz)
             dby(i,k)=hc(i,k)-hes_cup(i,k)
          END IF
       END DO
       !
    END DO
    RETURN
  END SUBROUTINE cup_up_he



  SUBROUTINE cup_up_moisture(ierr,z_cup,qc,qrc,pw,pwav,              &
       kbcon,ktop,mix,mkx,istart,iend,cd,dby,mentr_rate,       &
       q,gamma_cup,zu,qes_cup,k22,qe_cup)
    INTEGER istart,iend,mix,mkx,i,k
    REAL q(mix,mkx),zu(mix,mkx),gamma_cup(mix,mkx),qe_cup(mix,mkx),      &
         dby(mix,mkx),cd(mix,mkx),z_cup(mix,mkx),qes_cup(mix,mkx)
    REAL qc(mix,mkx),qrc(mix,mkx),pw(mix,mkx),pwav(mix)
    INTEGER kbcon(mix),ktop(mix),ierr(mix),k22(mix),IALL
    REAL radius,xl,dz,qrch,c0,mentr_rate
    IALL=0
    xl=2.5e6
    c0=.002
    !        c0=.000   !para teste de conservacao (prec=0)
    !
    !--- no precip for small clouds
    !
    IF(mentr_rate.GT.0.)THEN
       radius=.2/mentr_rate
       IF(radius.LT.900.)c0=0.
    END IF
    DO i=istart,iend
       pwav(i)=0.
    END DO
    DO k=1,mkx
       DO i=istart,iend
          pw(i,k) =0.
          !_srf     qc(i,k) =qes_cup(i,k)
          qc(i,k) =qe_cup(i,k)
          qrc(i,k)=0.
       END DO
    END DO
    DO i=istart,iend
       IF(ierr(i).EQ.0.)THEN
          DO k=k22(i),kbcon(i)-1
             qc(i,k)=qe_cup(i,k22(i))
          END DO
       END IF
    END DO
    !
    DO k=2,mkx-1
       DO i=istart,iend
          IF(ierr(i).NE.0)go to 100
          IF(k.LT.kbcon(i))go to 100
          IF(k.GT.ktop(i))go to 100
          dz=z_cup(i,k)-z_cup(i,k-1)
          !
          !--- 1. steady state plume equation, for what could
          !---    be in cloud without condensation
          !
          qc(i,k)=(qc(i,k-1)*(1.-.5*cd(i,k)*dz)+mentr_rate*        &
               dz*q(i,k-1))/(1.+mentr_rate*dz-.5*cd(i,k)*dz)
          !
          !--- saturation  in cloud, this is what is allowed to be in it
          !
          qrch=qes_cup(i,k)+(1./xl)*(gamma_cup(i,k)        &
               /(1.+gamma_cup(i,k)))*dby(i,k)
          !
          !--- liquid water content in cloud after rainout
          !
          qrc(i,k)=(qc(i,k)-qrch)/(1.+c0*dz)
          IF(qrc(i,k).LT.0.)THEN
             qrc(i,k)=0.
          END IF
          !
          !--- 3.condensation
          !
          pw(i,k)=c0*dz*qrc(i,k)*zu(i,k)
          IF(IALL.EQ.1)THEN
             qrc(i,k)=0.
             pw(i,k)=(qc(i,k)-qrch)*zu(i,k)
             IF(pw(i,k).LT.0.)pw(i,k)=0.
          END IF
          !
          !--- set next level
          !
          qc(i,k)=qrc(i,k)+qrch
          !
          !--- integrated normalized ondensate
          !
          pwav(i)=pwav(i)+pw(i,k)
100       CONTINUE
       END DO
    END DO
    RETURN
  END SUBROUTINE cup_up_moisture



  SUBROUTINE cup_up_nms(zu,z_cup,entr,cd,kbcon,ktop,mix,mkx,       &
       istart,iend,ierr,k22)
    INTEGER i,k,mix,mkx,istart,iend
    REAL zu(mix,mkx),z_cup(mix,mkx),cd(mix,mkx)
    INTEGER kbcon(mix),ktop(mix),k22(mix),ierr(mix)
    REAL entr,dz
    DO k=1,mkx
       DO i=istart,iend
          zu(i,k)=0.
       END DO
    END DO
    DO i=istart,iend
       IF(ierr(i).EQ.0)THEN
          DO k=k22(i),kbcon(i)
             zu(i,k)=1.
          END DO
          DO k=kbcon(i)+1,ktop(i)
             dz=z_cup(i,k)-z_cup(i,k-1)
             zu(i,k)=zu(i,k-1)*(1.+(entr-cd(i,k))*dz)
          END DO
       END IF
    END DO
    RETURN
  END SUBROUTINE cup_up_nms
  !----------------------------------------------------------------------
  SUBROUTINE cup_up_aa0(aa0,z,zu,dby,gamma_cup,t_cup,            &
       kbcon,ktop,mkx,mix,istart,iend,ierr)
    INTEGER i,k,mix,mkx,istart,iend
    REAL aa0(mix),z(mix,mkx),zu(mix,mkx),gamma_cup(mix,mkx),   &
         t_cup(mix,mkx),dby(mix,mkx)
    INTEGER kbcon(mix),ktop(mix),ierr(mix)
    REAL  dz,da
    DO i=istart,iend
       aa0(i)=0.
    END DO
    DO k=2,mkx-1
       DO i=istart,iend
          IF(ierr(i).NE.0)go to 100
          IF(k.LE.kbcon(i))go to 100
          IF(k.GT.ktop(i))go to 100
          dz=z(i,k)-z(i,k-1)
          da=zu(i,k)*dz*(9.81/(1004.*(                         &
               (t_cup(i,k)))))*dby(i,k-1)/                   &
               (1.+gamma_cup(i,k))
          IF(k.EQ.ktop(i).AND.da.LE.0.)go to 100
          aa0(i)=aa0(i)+da
          IF(aa0(i).LT.0.)aa0(i)=0.
          !
100       CONTINUE
       END DO
    END DO
    !
    RETURN
  END SUBROUTINE cup_up_aa0






  SUBROUTINE shcuso(te,qe,ps,sl,tfz,qfz,dt,imax,kmax)
    !
    ! Shallow cumulus heating and moistening tendencies
    ! Enio Pereira de Souza 12/Jul/2001
    !
    !
    INTEGER, INTENT(IN   ) :: imax
    INTEGER, INTENT(IN   ) :: kmax
    REAL,    INTENT(INOUT) :: te(imax,kmax)  ! air temperature (K)
    REAL,    INTENT(INOUT) :: qe(imax,kmax)  ! specific humidity (kg/kg)
    REAL,    INTENT(IN   ) :: ps(imax)       ! surface pressure (cb)
    REAL,    INTENT(IN   ) :: sl(kmax)      ! sigma layers
    REAL,    INTENT(IN   ) :: tfz(imax)     ! sensible heat flux (W/m^2)
    REAL,    INTENT(IN   ) :: qfz(imax)     ! latent heat flux (W/m^2)
    REAL,    INTENT(IN   ) :: dt            ! time step (s)
    !
    INTEGER :: kzi(imax)
    INTEGER :: ktop(imax)
    INTEGER :: klcl(imax)
    !
    LOGICAL :: llift(imax)
    !
    REAL :: press(imax,kmax)
    REAL :: tv(imax,kmax)
    REAL :: ze(imax,kmax)
    REAL :: den(imax,kmax)
    REAL :: delz(imax,kmax)
    REAL :: pi(imax,kmax)
    REAL :: th(imax,kmax)
    REAL :: thv(imax,kmax)
    REAL :: es(imax,kmax)
    REAL :: qes(imax,kmax)
    REAL :: se(imax,kmax)
    REAL :: uhe(imax,kmax)
    REAL :: uhes(imax,kmax)
    REAL :: uhc(imax,kmax)
    REAL :: dtdt(imax,kmax)
    REAL :: dqdt(imax,kmax)
    REAL :: gamma(imax,kmax)
    REAL :: dlamb(imax,kmax)
    REAL :: dldzby2(imax,kmax)
    REAL :: sc(imax,kmax)
    REAL :: sc0(imax,kmax)
    REAL :: qc(imax,kmax)
    REAL :: ql(imax,kmax)
    REAL :: tvm(imax,kmax)
    REAL :: scv(imax,kmax)
    REAL :: sev(imax,kmax)
    REAL :: sc0v(imax,kmax)
    REAL :: scvm(imax,kmax)
    REAL :: sevm(imax,kmax)
    REAL :: sc0vm(imax,kmax)
    REAL :: buoy1(imax)
    REAL :: buoy2(imax)
    REAL :: cape(imax)
    REAL :: tcape(imax)
    REAL :: emp(imax,kmax)
    REAL :: efic(imax)
    REAL :: fin(imax)
    REAL :: tcold(imax)
    REAL :: thot(imax)
    REAL :: sigwshb(imax)
    REAL :: wc(imax,kmax)
    REAL :: wssc(imax,kmax)
    REAL :: wqsc(imax,kmax)
    REAL :: dsdt(imax,kmax)
    REAL :: tpar(imax)
    REAL :: qex1(imax)
    REAL :: qpar(imax)
    REAL :: tt(imax)   
    REAL :: espar(imax)    
    REAL :: qspar(imax)    
    REAL :: qexces(imax)
    REAL :: dqdp(imax)
    REAL :: deltap(imax)
    REAL :: plcl(imax)
    REAL :: tlcl(imax)
    REAL :: tx
    REAL :: es1
    !
    ! constants
    !
    INTEGER :: i, k, klcl1, icount, ki
    !
    REAL, PARAMETER :: grav=9.806
    REAL, PARAMETER :: vlat=2.5e6
    REAL, PARAMETER :: cp=1004.5
    REAL, PARAMETER :: rd=287.
    REAL, PARAMETER :: rcp=.286
    REAL, PARAMETER :: p00=100000.
    REAL, PARAMETER :: gammad=0.00976
    REAL, PARAMETER :: es00=611.2
    REAL, PARAMETER :: epslon=.622
    REAL, PARAMETER :: ummeps=.378
    REAL, PARAMETER :: ta0=273.15
    ! REAL, PARAMETER :: co1=17223003.15
    REAL, PARAMETER :: co1=21709759.15
    REAL, PARAMETER :: co2=29.65
    REAL, PARAMETER :: co3=17.67
    REAL, PARAMETER :: c0=0.
    REAL, PARAMETER :: dlamb0=1e-6
    REAL, PARAMETER :: zref=600.
    REAL, PARAMETER :: dthv=2.0
    REAL, PARAMETER :: fifty=50.
    !
    REAL, SAVE :: d1,d2,d3,d4,d5,d6,d7
    !
    DATA d1/ .6107042e0/, d2/4.441157e-2/, d3/1.432098e-3/, &
         d4/2.651396e-5/, d5/3.009998e-7/, d6/2.008880e-9/, &
         d7/6.192623e-12/
    es1(tx) = d1 + tx*(d2 + tx*(d3 + tx*(d4 + tx*(d5 + &
         tx*(d6 + d7*tx)))))
    !
    DO i=1,imax
       DO k=1,kmax
          dtdt(i,k)=0.
          dqdt(i,k)=0.
          dsdt(i,k)=0.
          ze(i,k)=0.0 
          ql (i,k)=0.0
          press(i,k)=0.0
          tv   (i,k)=0.0
          th   (i,k)=0.0
       END DO
       tcape  (i)=0.0
       sigwshb(i)=0.0
       efic   (i)=0.0
    END DO
    !
    ! begining of a long loop in index " i "
    !
    !
    ! constructing height profile
    !
    DO k=1,kmax
       DO i=1,imax
          press(i,k)=ps(i)*sl(k)
          press(i,k)=press(i,k)*1000.
          tv(i,k)=te(i,k)*(1+0.608*qe(i,k))
          th(i,k)=te(i,k)*EXP(rcp*LOG(p00/press(i,k)))
          pi(i,k)=cp*EXP(rcp*LOG(press(i,k)/p00))
          thv(i,k)=th(i,k)*(1+0.608*qe(i,k))
          es(i,k)=es00*EXP(co3*(te(i,k)-ta0)/(te(i,k)-co2))
          qes(i,k)=epslon*es(i,k)/(press(i,k)-ummeps*es(i,k))
          den(i,k)=press(i,k)/(rd*tv(i,k))
       END DO
    END DO
    DO i=1,imax
       !ze(i,1)=0.0
       ze(i,1)=29.25*te(i,1)*(101.3-ps(i))/ps(i)
       ze(i,1)=MAX(0.0,ze(i,1))
    END DO
    DO k=2,kmax
       DO i=1,imax
          delz(i,k)=.5*rd*(tv(i,k-1)+tv(i,k))* &
               LOG(press(i,k-1)/press(i,k))/grav
          ze(i,k)=ze(i,k-1)+delz(i,k)
       END DO
    END DO

    DO i=1,imax
       !
       ! constructing height profile
       !
       !       DO k=1,kmax
       !          press(i,k)=ps(i)*sl(k)
       !        press(i,k)=press(i,k)*1000.
       !          tv(i,k)=te(i,k)*(1+0.608*qe(i,k))
       !          th(i,k)=te(i,k)*EXP(rcp*LOG(p00/press(i,k)))
       !          pi(i,k)=cp*EXP(rcp*LOG(press(i,k)/p00))
       !          thv(i,k)=th(i,k)*(1+0.608*qe(i,k))
       !          es(i,k)=es00*EXP(co3*(te(i,k)-ta0)/(te(i,k)-co2))
       !          qes(i,k)=epslon*es(i,k)/(press(i,k)-ummeps*es(i,k))
       !          den(i,k)=press(i,k)/(rd*tv(i,k))
       !       END DO
       !       !ze(i,1)=0.0
       !       ze(i,1)=29.25*te(i,1)*(101.3-ps(i))/ps(i)
       !       ze(i,1)=MAX(0.0,ze(i,1))
       !       DO k=2,kmax
       !          delz(i,k)=.5*rd*(tv(i,k-1)+tv(i,k))* &
       !               LOG(press(i,k-1)/press(i,k))/grav
       !          ze(i,k)=ze(i,k-1)+delz(i,k)
       !       END DO
       !
       IF(tfz(i).LT.0.0)THEN
          !EXIT 
          CYCLE
       END IF
       !
       ki=1
       !
       llift(i)=.FALSE.
       qex1(i) = 0.
       qpar(i) = qe(i,ki)
       !     
       !     lift parcel from k=ki until it becomes saturated
       !
       DO k = ki, kmax
          IF(.NOT.llift(i)) THEN
             tpar(i) = te(i,ki)* &
                  EXP(rcp*LOG(press(i,k)/press(i,ki)))
             tt(i)=tpar(i)-ta0
             IF(tt(i).GE.-fifty) THEN
                espar(i) = es1(tt(i))
             ELSE
                espar(i) =.00636e0* &
                     EXP(25.6e0*(tt(i)+fifty)/(ta0-fifty))
             END IF
             qspar(i) = epslon*espar(i)*1000./ &
                  (press(i,k)-(1-epslon)*espar(i)*1000.)
             qexces(i) = qpar(i) - qspar(i)
             !
             !     if parcel not saturated,  try next level
             !
             IF (qexces(i).LT.0.) THEN
                qex1(i) = qexces(i)
                !
                !     saturated - go down and find p,t, sl at the lcl;
                !     if sat exists in first layer (k=ki), use this as lcl
                !
             ELSE IF (k.EQ.ki) THEN
                plcl(i)  = press(i,k)
                tlcl(i)  = tpar(i)
                tlcl(i)  = tlcl(i) + 1.
                klcl(i)  = k
                llift(i) = .TRUE.
             ELSE
                dqdp(i)   = (qexces(i)-qex1(i))/ &
                     (press(i,k-1)-press(i,k))
                deltap(i) = qexces(i)/dqdp(i)
                plcl(i)   = press(i,k) + deltap(i)
                tlcl(i)   = tpar(i) * (1.+2.*rcp*deltap(i) &
                     /(press(i,k)+press(i,k-1)))
                tlcl(i)   = tlcl(i) + 1.
                klcl(i)   = k
                llift(i) = .TRUE.
                !
                !     give parcel a one-degree goose
                !
             ENDIF
             !
             !     lifting cond level found - get out of loop
             !
          ENDIF
       END DO
       !
       !     quit if parcel still unsat at k = kthick - - -
       !     in this case,set low value of plcl as signal to shalmano
       !
       !IF(.NOT.llift(i)) EXIT 
       IF(.NOT.llift(i)) CYCLE
       !
       ! testing the difference in height between lcl and zi, if zi < z(lcl)
       ! there will be no shallow cumulus
       !
       DO k=3,kmax
          IF((thv(i,k)-thv(i,k-1)).GT.dthv)THEN
             kzi(i)=k
             EXIT
          END IF
       END DO

       IF(kzi(i).LT.klcl(i))THEN
          !EXIT 
          CYCLE
       END IF
       !
       ! static energy profiles
       !
       DO k=1,kmax
          se(i,k)=cp*te(i,k)+grav*ze(i,k)
          uhe(i,k)=se(i,k)+vlat*qe(i,k)
          uhes(i,k)=se(i,k)+vlat*qes(i,k)
          gamma(i,k)=co1*press(i,k)*qes(i,k)*qes(i,k)/es(i,k)
          !   gamma(i,k)=gamma(i,k)/((te(i,k)-co2)*(te(i,k)-co2))
          gamma(i,k)=gamma(i,k)/(te(i,k)*te(i,k))
       END DO
       !
       ! vertical profile of the entrainment rate and cloud moist
       ! static energy
       !
       klcl1=klcl(i)+1
       uhc(i,1)=uhe(i,1)
       IF(klcl(i).GT.2)THEN
          DO k=2,klcl(i)
             uhc(i,k)=uhe(i,1)
          END DO
       ELSE
          !EXIT 
          CYCLE
       END IF
       DO k=klcl1,kmax
          dlamb(i,k)=EXP(LOG(dlamb0)+2.3*ze(i,k)/zref)
          dlamb(i,k)=MIN(0.1,dlamb(i,k))
          dldzby2(i,k)=dlamb(i,k)*delz(i,k)/2
          uhc(i,k)=(uhc(i,k-1)-dldzby2(i,k)*(uhc(i,k-1)-uhe(i,k)- &
               uhe(i,k-1)))/(1+dldzby2(i,k))
       END DO
       !
       ! calculating cloud variables qc, ql, sc
       !
       DO k=1,kmax
          sc(i,k)=se(i,k)+(uhc(i,k)-uhes(i,k))/(1+gamma(i,k))
          sc0(i,k)=se(i,k)+(uhc(i,1)-uhes(i,k))/(1+gamma(i,k))
          qc(i,k)=qes(i,k)+gamma(i,k)*(uhc(i,k)-uhes(i,k))/ &
               (vlat*(1+gamma(i,k)))
          ql(i,k)=0.
       END DO
       DO k=klcl1,kmax
          ql(i,k)=ql(i,k-1)-(qc(i,k)-qc(i,k-1))-dlamb(i,k)* &
               (qc(i,k)-qe(i,k))*delz(i,k)- &
               (c0+dlamb(i,k))*ql(i,k-1)*delz(i,k)
          ql(i,k)=MAX(.00000001,ql(i,k))
       END DO
       !
       ! determining cloud top based on integrated buoyancy
       !
       DO k=1,kmax
          scv(i,k)=sc(i,k)+cp*te(i,k)*(.608*qc(i,k)-ql(i,k))
          sev(i,k)=se(i,k)+.608*cp*te(i,k)*qe(i,k)
          sc0v(i,k)=sc0(i,k)+.608*cp*te(i,k)*qe(i,k)
       END DO
       DO k=2,kmax
          scvm(i,k)=(scv(i,k)+scv(i,k-1))/2
          sevm(i,k)=(sev(i,k)+sev(i,k-1))/2
          sc0vm(i,k)=(sc0v(i,k)+sc0v(i,k-1))/2
          tvm(i,k)=(tv(i,k)+tv(i,k-1))/2
       END DO
       ! 
       ! determination of the integrated buoyancy between surface and lcl
       ! The calculation assumes that the surface flux is in W/m2. Therefore
       ! we divide it by density*cp in order to convert it to Km/s
       !
       IF(tfz(i).GT.0.)THEN
          buoy1(i)=tfz(i)*(1+.608*qe(i,1))/(cp*den(i,1))
          buoy1(i)=grav*ze(i,klcl(i))*buoy1(i)/tv(i,1)
          buoy1(i)=EXP(0.29+.6667*LOG(buoy1(i)))
       ELSE
          buoy1(i)=0.
          !EXIT 
          CYCLE
       END IF
       !
       ! checking wether the parcel is able to sustain positive
       ! buoyancy one level above lcl
       !
       cape(i)=0.
       buoy2(i)=0.
       buoy2(i)=buoy2(i)+gammad*(scvm(i,klcl1)-sevm(i,klcl1))* &
            delz(i,klcl1)/tvm(i,klcl1)
       IF((buoy1(i)+buoy2(i)).LE.0.)THEN
          !EXIT
          CYCLE
       END IF
       !
       ! calculating cloud top and cape
       !
       DO k=klcl1+1,kmax
          buoy2(i)=buoy2(i)+gammad*(scvm(i,k)-sevm(i,k))* &
               delz(i,k)/tvm(i,k)
          IF((buoy1(i)+buoy2(i)).LE.0.)THEN
             ktop(i)=k-1
             EXIT
          ENDIF
       END DO
       DO k=klcl(i),ktop(i)
          emp(i,k)=(sc0vm(i,k)-sevm(i,k))
          emp(i,k)=MAX(0.,emp(i,k))
          cape(i)=cape(i)+gammad*emp(i,k)*delz(i,k)/tvm(i,k)
       END DO
       !
       ! calculating the cloud base mass flux
       !
       thot(i)=te(i,1)
       tcold(i)=te(i,2)
       icount=1
       DO k=3,ktop(i)
          tcold(i)=tcold(i)+te(i,k)
          icount=icount+1
       END DO
       tcold(i)=tcold(i)/icount
       efic(i)=(thot(i)-tcold(i))/thot(i)
       !
       IF(efic(i).LE.0.0.OR.cape(i).LT.40)THEN
          !EXIT
          CYCLE
       END IF
       !
       fin(i)=tfz(i)+qfz(i)
       tcape(i)=2*cape(i)
       !tcape(i)=cape(i)
       sigwshb(i)=efic(i)*fin(i)/(den(i,klcl(i))*tcape(i))

       DO k=klcl(i),ktop(i)
          wc(i,k)=sigwshb(i)*((ze(i,ktop(i))-ze(i,k))/ &
               (ze(i,ktop(i))-ze(i,klcl(i))))
       END DO
       ! 
       DO k=1,kmax
          wssc(i,k)=0.
          wqsc(i,k)=0.
       END DO

       DO k=klcl(i),ktop(i)
          wssc(i,k)=wc(i,k)*(sc(i,k)-vlat*ql(i,k)-se(i,k))
          wqsc(i,k)=wc(i,k)*(qc(i,k)+ql(i,k)-qe(i,k))
       END DO

       DO k=klcl(i)+1,ktop(i)-1
          dsdt(i,k)=-(wssc(i,k+1)-wssc(i,k-1))/(ze(i,k+1)-ze(i,k-1))
          dtdt(i,k)=dsdt(i,k)/pi(i,k)
          dqdt(i,k)=-(wqsc(i,k+1)-wqsc(i,k-1))/(ze(i,k+1)-ze(i,k-1))
       END DO
       !
       ! updating temperature and moisture fields due to shallow convection 
       !  
       DO k=1,kmax
          !PRINT*,'p,dt,dq ',press(i,k),dtdt(i,k)*86400.,dqdt(i,k)*86400000.
          te(i,k)=te(i,k)+dtdt(i,k)*dt
          qe(i,k)=qe(i,k)+dqdt(i,k)*dt
          !print*,'z-dtq= ',ze(i,k),dtdt(i,k)*86400.,dqdt(i,k)*86400000.
       END DO
       !
       ! end of initial loop in index " i "
    END DO

    !CALL WRG(1,imax,kmax,dtdt)
    !CALL WRG(2,imax,kmax,dqdt)
    !CALL WRG(3,imax,kmax,ze  )
    !CALL WRG(1,imax,tcape  )
    !CALL WRG(2,imax,sigwshb)
    !CALL WRG(3,imax,efic   )
  END SUBROUTINE shcuso
END MODULE Convection
