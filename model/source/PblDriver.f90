!
!  $Author: pkubota $
!  $Date: 2008/08/15 19:01:20 $
!  $Revision: 1.2 $
!
MODULE PblDriver

  USE Constants, ONLY :     &
       cp,            &
       grav,          &
       gasr,          &
       r8

  USE PlanBoundLayer, ONLY: &
       InitPlanBoundLayer  ,&
       ympbl0

  USE Diagnostics, ONLY: &
        updia,dodia , &
        StartStorDiag,&
        nDiag_vdheat, & ! vertical diffusion heating
        nDiag_vdmois, & ! vertical diffusion moistening
        nDiag_vduzon, & ! vertical diffusion zonal momentum change
        nDiag_vdvmer    ! vertical diffusion meridional momentum change


  USE GridHistory, ONLY:       &
       IsGridHistoryOn, StoreGridHistory, dogrh, &
       nGHis_vdheat, nGHis_vduzon, nGHis_vdvmer, nGHis_vdmois

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: InitPBLDriver
  PUBLIC :: Pbl_Driver

CONTAINS
  SUBROUTINE InitPBLDriver(kmax, sl, del_in, si)
   IMPLICIT NONE
    INTEGER      , INTENT(IN) :: kmax
    REAL(KIND=r8), INTENT(IN) :: sl     (kmax)
    REAL(KIND=r8), INTENT(IN) :: del_in (kmax)
    REAL(KIND=r8), INTENT(IN) :: si	(kMax+1)
    
    CALL InitPlanBoundLayer(kmax, sl, del_in, si(1:kmax))
    
  END SUBROUTINE InitPBLDriver

  !----------------------------------------------------------------------
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  !----------------------------------------------------------------------
  SUBROUTINE Pbl_Driver ( &
       gu    ,gv    ,gt    ,gq    ,delsig,ncols ,     &
       kmax  ,delt  ,colrad,tmtx  ,qmtx  ,umtx   ,gl0 ,PBL,latco )
   IMPLICIT NONE

    INTEGER, INTENT(in   ) :: ncols
    INTEGER, INTENT(in   ) :: kmax
    INTEGER, INTENT(in   ) :: PBL
    INTEGER, INTENT(in   ) :: latco
    REAL(KIND=r8),    INTENT(in   ) :: gu    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: gv    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: gt    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: gq    (ncols,kmax)
    REAL(KIND=r8),    INTENT(in   ) :: delsig(kmax)
    REAL(KIND=r8),    INTENT(in   ) :: delt
    REAL(KIND=r8),    INTENT(in   ) :: colrad(ncols)
    REAL(KIND=r8),    INTENT(inout) :: tmtx   (ncols,kmax,3)
    REAL(KIND=r8),    INTENT(inout) :: qmtx   (ncols,kmax,3)
    REAL(KIND=r8),    INTENT(inout) :: umtx   (ncols,kmax,4)
    REAL(KIND=r8),    INTENT(inout) :: gl0    (ncols)

    CALL ympbl0( &
         gu    ,gv    ,gt    ,gq    ,delsig,ncols ,     &
         kmax  ,delt  ,colrad,tmtx  ,qmtx  ,umtx  ,gl0 ,PBL )


    IF(PBL /= 1)THEN
       !-----------------
       ! Storage Diagnostic Fields
       !------------------
       IF( StartStorDiag)THEN
          CALL PblDiagnStorage(latco,nCols,kMax,tmtx,qmtx,umtx)
       END IF 
       !-----------------
       ! Storage GridHistory Fields
       !------------------
       IF(IsGridHistoryOn())THEN
          CALL PblGridHistoryStorage(latco,nCols,kMax,tmtx,qmtx,umtx)
       END IF
    END IF

  END SUBROUTINE Pbl_Driver
  
  SUBROUTINE PblGridHistoryStorage(latco,nCols,kMax,tmtx,qmtx,umtx)
   IMPLICIT NONE
   INTEGER      , INTENT(IN   ) :: latco
   INTEGER      , INTENT(IN   ) :: nCols
   INTEGER      , INTENT(IN   ) :: kMax
   REAL(KIND=r8), INTENT(in   ) :: tmtx (ncols,kmax,3)
   REAL(KIND=r8), INTENT(in   ) :: qmtx (ncols,kmax,3)
   REAL(KIND=r8), INTENT(in   ) :: umtx (ncols,kmax,4)
   
   IF(dogrh(nGHis_vdheat,latco))CALL StoreGridHistory(tmtx(:,:,3),nGHis_vdheat,latco)
   IF(dogrh(nGHis_vduzon,latco))CALL StoreGridHistory(umtx(:,:,3),nGHis_vduzon,latco)
   IF(dogrh(nGHis_vdvmer,latco))CALL StoreGridHistory(umtx(:,:,4),nGHis_vdvmer,latco)
   IF(dogrh(nGHis_vdmois,latco))CALL StoreGridHistory(qmtx(:,:,3),nGHis_vdmois,latco)
  
  END SUBROUTINE PblGridHistoryStorage
  
  
  SUBROUTINE PblDiagnStorage(latco,nCols,kMax,tmtx,qmtx,umtx)
   IMPLICIT NONE
   INTEGER      , INTENT(IN   ) :: latco
   INTEGER      , INTENT(IN   ) :: nCols
   INTEGER      , INTENT(IN   ) :: kMax
   REAL(KIND=r8), INTENT(in   ) :: tmtx (ncols,kmax,3)
   REAL(KIND=r8), INTENT(in   ) :: qmtx (ncols,kmax,3)
   REAL(KIND=r8), INTENT(in   ) :: umtx (ncols,kmax,4)
   
   IF(dodia(nDiag_vdheat)) CALL updia(tmtx(1:nCols,1:kMax,3),nDiag_vdheat,latco)
   IF(dodia(nDiag_vduzon)) CALL updia(umtx(1:nCols,1:kMax,3),nDiag_vduzon,latco)
   IF(dodia(nDiag_vdvmer)) CALL updia(umtx(1:nCols,1:kMax,4),nDiag_vdvmer,latco)
   IF(dodia(nDiag_vdmois)) CALL updia(qmtx(1:nCols,1:kMax,3),nDiag_vdmois,latco)
  
  END SUBROUTINE PblDiagnStorage
  !----------------------------------------------------------------------
END MODULE PblDriver
