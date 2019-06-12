!
!  $Author: pkubota $
!  $Date: 2008/08/19 16:57:04 $
!  $Revision: 1.3 $
!
MODULE GwddDriver

  USE Constants, ONLY :     &
       cp,            &
       grav,          &
       gasr,          &
       r8

  USE GwddSchemeAlpert, ONLY: &
      InitGwddSchAlpert  ,&
      GwddSchAlpert

  USE Diagnostics, ONLY: &
        updia,dodia , &
        StartStorDiag,&
        nDiag_txgwds, & ! gravity wave drag surface zonal stress
        nDiag_tygwds, & ! gravity wave drag surface meridional stress
        nDiag_gwduzc, & ! gravity wave drag zonal momentum change
        nDiag_gwdvmc    ! gravity wave drag meridional momentum change


  USE GridHistory, ONLY:       &
       IsGridHistoryOn, StoreGridHistory, StoreMaskedGridHistory, dogrh

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitGWDDDriver
  PUBLIC :: Gwdd_Driver

CONTAINS
  SUBROUTINE InitGWDDDriver(kmax,  si)
   IMPLICIT NONE
    INTEGER      , INTENT(IN) :: kmax
    REAL(KIND=r8), INTENT(IN) :: si(kMax+1)
    
    CALL InitGwddSchAlpert(si, kmax)
    
  END SUBROUTINE InitGWDDDriver

  !----------------------------------------------------------------------
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  !----------------------------------------------------------------------
  SUBROUTINE Gwdd_Driver (ps ,gu   ,gv   ,gt   , chug, chvg, xdrag, ydrag, &
            var, varcut, sigml, sig, delsig, ncols, kmax,latco)
   IMPLICIT NONE
    INTEGER      , INTENT(in   ) :: ncols
    INTEGER      , INTENT(in   ) :: kmax
    INTEGER      , INTENT(in   ) :: latco
    REAL(KIND=r8), INTENT(inout) :: ps     (ncols)
    REAL(KIND=r8), INTENT(in   ) :: gu     (ncols,kmax)
    REAL(KIND=r8), INTENT(in   ) :: gv     (ncols,kmax)
    REAL(KIND=r8), INTENT(in   ) :: gt     (ncols,kmax)
    REAL(KIND=r8), INTENT(inout) :: chug   (ncols,kmax)
    REAL(KIND=r8), INTENT(inout) :: chvg   (ncols,kmax)
    REAL(KIND=r8), INTENT(inout) :: xdrag  (ncols)
    REAL(KIND=r8), INTENT(inout) :: ydrag  (ncols)
    REAL(KIND=r8), INTENT(inout) :: var    (ncols)
    REAL(KIND=r8), INTENT(in   ) :: varcut
    REAL(KIND=r8), INTENT(in   ) :: sigml  (kmax+1)
    REAL(KIND=r8), INTENT(in   ) :: sig    (kmax)
    REAL(KIND=r8), INTENT(in   ) :: delsig (kmax)
    
    CALL GwddSchAlpert(ps ,gu    ,gv   ,gt   , chug  , chvg , xdrag, ydrag, &
                       var,varcut,sigml,sig  , delsig, ncols, kmax)


    !-----------------
    ! Storage Diagnostic Fields
    !------------------
    IF( StartStorDiag)THEN
       CALL GwddDiagnStorage(latco,nCols,kMax,xdrag,ydrag,chug,chvg )
    END IF 
    !-----------------
    ! Storage GridHistory Fields
    !------------------
    IF(IsGridHistoryOn())THEN
       CALL GwddGridHistoryStorage()
    END IF

  END SUBROUTINE Gwdd_Driver
  
  SUBROUTINE GwddGridHistoryStorage()
   IMPLICIT NONE

   RETURN      
  END SUBROUTINE GwddGridHistoryStorage
  
  
  SUBROUTINE GwddDiagnStorage(latco,nCols,kMax,xdrag,ydrag,&
                              chug,chvg )
   IMPLICIT NONE
   INTEGER      , INTENT(IN   ) :: latco
   INTEGER      , INTENT(IN   ) :: nCols
   INTEGER      , INTENT(IN   ) :: kMax
   REAL(KIND=r8), INTENT(in   ) :: xdrag (ncols)
   REAL(KIND=r8), INTENT(in   ) :: ydrag (ncols)
   REAL(KIND=r8), INTENT(in   ) :: chug  (ncols,kmax)
   REAL(KIND=r8), INTENT(in   ) :: chvg  (ncols,kmax) 
   
   IF(dodia(nDiag_txgwds))CALL updia(xdrag,nDiag_txgwds,latco)
   IF(dodia(nDiag_tygwds))CALL updia(ydrag,nDiag_tygwds,latco)
   IF(dodia(nDiag_gwduzc))CALL updia(chug, nDiag_gwduzc,latco)
   IF(dodia(nDiag_gwdvmc))CALL updia(chvg, nDiag_gwdvmc,latco)

  END SUBROUTINE GwddDiagnStorage
  !----------------------------------------------------------------------
END MODULE GwddDriver
