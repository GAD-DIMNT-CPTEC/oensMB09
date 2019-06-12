!
!  $Author: pkubota $
!  $Date: 2011/04/07 16:00:31 $
!  $Revision: 1.12 $
!
MODULE Communications

  USE Parallelism, ONLY: &
       myId,             &
       maxNodes_four,    &
       myId_four,        &
       COMM_FOUR,        &
       mygroup_four,     &
       maxNodes

  USE Sizes, ONLY:     &
       mymmax,         &
       mymnmax,        &
       mymnextmax,     &
       kmax,           &
       kmaxloc,        &
       mmax,           &
       mmap,           &
       mnmax,          &
       mnmax_si,       &
       mnextmax,       &
       mnmaxlocal,     &
       mnextmaxlocal,  &
       ibmax,          &
       jbmax,          &
       jbMax_ext,      &
       ijmax,          &
       imax,           &
       jmax,           &
       ibmaxperjb,     &
       imaxperj,       &
       jbperij,        &
       ibperij,        &
       Msperproc,      &
       Msinproc,       &
       mnsPerProc,     &
       mnsExtPerProc,  &
       NodehasM,       &
       lm2m,           &
       myfirstlat,     &
       mylastlat,      &
       mysendsgr,      &
       mysendspr,      &
       myrecsgr,       &
       myrecspr,       &
       firstlat,       &
       lastlat,        &
       firstlon,       &
       lastlon,        &
       myfirstlon,     &
       mylastlon,      &
       messages_f,     &
       messproc_f,     &
       messages_g,     &
       messproc_g,     &
       nrecs_diag,     &
       nsends_diag,    &
       myfirstlat_diag,&
       mylastlat_diag, &
       myjmax_d,       &
       firstandlastlat,&
       myrecs_diag,    &
       myrecspr_diag,  &
       mysends_diag,   &
       mysendspr_diag, &
       havesurf,       &
       mnsendsmap_si,  &
       mymnmap_si,     &
       myfirstlev,     &
       map_four,       &
       ngroups_four,   &
       kfirst_four,    &
       klast_four,     &
       nlevperg_four,  &
       first_proc_four,&
       ncomm_spread,   &
       comm_spread,    &
       ms_spread,      &
       nlatsinproc_d,  &
       gridmap,        &
       pointsinproc
   
  USE Options, ONLY:   &
       nfprt,          &
       nClass,         &
       nAeros,         &
       reducedgrid

  USE Utils, ONLY:      &
       CyclicNearest_r, &
       CyclicLinear_ABS,&
       CyclicLinear

  USE IOLowLevel, ONLY:  FWriteField, &
         FWriteFieldb     ! add solange 27-01-2012

  USE Constants, ONLY:  i8,r8,i4

  IMPLICIT NONE

  INCLUDE 'mpif.h'

  PRIVATE

  PUBLIC :: Collect_Grid_Red
  PUBLIC :: Collect_Grid_Sur
  PUBLIC :: Collect_Grid_Sur_Print
  PUBLIC :: Collect_Grid_Sur_Print2  ! add solange 27-01-2012
  PUBLIC :: Collect_Grid_His
  PUBLIC :: Collect_Grid_Full
  PUBLIC :: Collect_Grid_FullI
  PUBLIC :: Collect_Grid_d
  PUBLIC :: Collect_Gauss
  PUBLIC :: Collect_Spec
  PUBLIC :: Collect_Spec_Ext
  PUBLIC :: Exchange_Fields
  PUBLIC :: Exchange_Winds
  PUBLIC :: Exchange_Hallos
  PUBLIC :: Exchange_si
  PUBLIC :: Exchange_ftog
  PUBLIC :: Exchange_diag
  PUBLIC :: Set_Communic_buffer
  PUBLIC :: SpectoSi
  PUBLIC :: SitoSpec
  PUBLIC :: Spread_surf_Spec
  PUBLIC :: p2d

  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: bufrec(:)
  REAL(KIND=r8), PUBLIC, ALLOCATABLE :: bufsend(:)
  INTEGER,         ALLOCATABLE :: isbrec(:)
  INTEGER,         ALLOCATABLE :: isbsend(:)
  INTEGER,         ALLOCATABLE :: ilrecbuf(:)
  INTEGER,         ALLOCATABLE :: ilsendbuf(:)
  INTEGER, PUBLIC              :: dimrecbuf 
  INTEGER, PUBLIC              :: dimsendbuf 
  TYPE p2d
     REAL(KIND=r8), POINTER :: p(:,:)
  END TYPE p2d

CONTAINS

  SUBROUTINE Set_Communic_buffer
    !
    INTEGER :: ndim

    ndim = kmax*8*jmax*mmax*2/maxnodes
    dimrecbuf = ndim
    dimsendbuf = ndim
    ALLOCATE (bufrec(dimrecbuf))
    ALLOCATE (bufsend(dimsendbuf))

  END SUBROUTINE Set_Communic_buffer

  SUBROUTINE Collect_Grid_Red(field, fieldglob)
    !
    !   Processor 0 has output in fieldglob
    !
    REAL(KIND=r8)   , INTENT(IN) :: field(ibMax*jbMax)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(ijmax)

    INTEGER :: ij, j, i, ii
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: ini(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE)

    comm = MPI_COMM_WORLD
    IF (myid.ne.0) THEN
       CALL MPI_ISEND(field,pointsinproc(myid),MPI_DOUBLE_PRECISION,0, &
                      91,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(0) = MPI_REQUEST_NULL
       ini(0) = 1
       ij=1+pointsinproc(0)
       DO ii=1,MaxNodes-1
          ini(ii) = ij
          CALL MPI_IRECV(bufrec(ij),pointsinproc(ii),MPI_DOUBLE_PRECISION,ii,91,&
                         comm,requestr(ii),ierr)
          ij = ij + pointsinproc(ii)
       ENDDO
       bufrec(1:pointsinproc(0)) = field(1:pointsinproc(0))
       DO ii=1,MaxNodes-1
          CALL MPI_WAITANY(MaxNodes-1,requestr(1),index,status,ierr)
       END DO
       ii = 1
       DO j = 1,jmax
          DO i = 1,imaxperj(j)
             ij = gridmap(i,j)
             fieldglob(ii) = bufrec(ini(ij))
             ii = ii + 1
             ini(ij) = ini(ij) + 1
          END DO
       END DO
    END IF

  END SUBROUTINE Collect_Grid_Red


  SUBROUTINE Collect_Grid_His(field, fieldglob, ngpts, ngptslocal, nproc, nf, &
                              ngptsperproc, mapglobal)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: nf
    INTEGER, INTENT(IN) :: ngpts
    INTEGER, INTENT(IN) :: ngptslocal
    INTEGER, INTENT(IN) :: mapglobal(ngpts)
    REAL(KIND=r8)   , INTENT(IN) :: field(ngptslocal,nf)
    INTEGER, INTENT(IN) :: ngptsperproc(0:maxnodes-1)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(ngpts,nf)

    INTEGER :: ij, i, n, i1, i2
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: ini(0:MaxNodes)
    INTEGER :: status(MPI_STATUS_SIZE)

    IF (dimrecbuf.lt.ngpts*nf) THEN
       dimrecbuf = ngpts*nf
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       CALL MPI_ISEND(field,ngptslocal*nf,MPI_DOUBLE_PRECISION,nproc,92,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(nproc) = MPI_REQUEST_NULL
       ini(0) = 0
       ij = 1
       DO i=0,MaxNodes-1
          IF (i.ne.nproc) THEN
             CALL MPI_IRECV(bufrec(ij),ngptsperproc(i)*nf,MPI_DOUBLE_PRECISION,i,92,&
                            comm,requestr(i),ierr)
          ENDIF
          ini(i+1) = ini(i) + ngptsperproc(i)
          ij = ij + ngptsperproc(i)*nf
       ENDDO
       i1 = ini(nproc)+1
       i2 = ini(nproc+1)
       fieldglob(mapglobal(i1:i2),:) = field(1:ngptslocal,:)
       DO i=1,MaxNodes-1
          CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
          ij = status(MPI_SOURCE)
          i1 = ini(ij)*nf
          !CDIR NODEP
          DO n=1,nf
             fieldglob(mapglobal(ini(ij)+1:ini(ij+1)),n) = &
                                             bufrec(i1+1:i1+ngptsperproc(ij))
             i1 = i1 + ngptsperproc(ij)
          ENDDO
       ENDDO
    END IF

  END SUBROUTINE Collect_Grid_His


  SUBROUTINE Collect_Grid_Sur(field, fieldglob, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    REAL(KIND=r8)   , INTENT(IN) :: field(imax,myjMax_d)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(imax,jmax)

    INTEGER :: ij, i
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       IF (myjmax_d.gt.0) THEN
          CALL MPI_ISEND(field,imax*myjmax_d,MPI_DOUBLE_PRECISION,nproc,93,comm,request,ierr)
          CALL MPI_WAIT(request,status,ierr)
       ENDIF
      ELSE
       requestr = MPI_REQUEST_NULL
       IF(myjmax_d.gt.0) fieldglob(:,myfirstlat_diag:mylastlat_diag) = field(:,:)
       ij=1
       DO i=0,MaxNodes-1
          IF (i.ne.nproc.and.nlatsinproc_d(i).gt.0) THEN
             CALL MPI_IRECV(fieldglob(1,ij),nlatsinproc_d(i)*imax,MPI_DOUBLE_PRECISION,i,93,&
                            comm,requestr(i),ierr)
          ENDIF
          ij = ij + nlatsinproc_d(i)
       ENDDO
       CALL MPI_WAITALL(MaxNodes,requestr(0),status,ierr)
    END IF

  END SUBROUTINE Collect_Grid_Sur


  SUBROUTINE Collect_Grid_d(field, fieldglob, levs, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: levs
    REAL(KIND=r8)   , INTENT(IN) :: field(imax,myjMax_d,levs)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(imax,jmax,levs)

    INTEGER :: ij, i, nr, k, j, ip
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: ini(0:MaxNodes)
    INTEGER :: index
    INTEGER :: status(MPI_STATUS_SIZE)

    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       IF (myjmax_d.gt.0) THEN
          CALL MPI_ISEND(field,imax*myjmax_d*levs,MPI_DOUBLE_PRECISION,nproc,93,comm,request,ierr)
          CALL MPI_WAIT(request,status,ierr)
       ENDIF
      ELSE
       requestr = MPI_REQUEST_NULL
       IF(myjmax_d.gt.0) fieldglob(:,myfirstlat_diag:mylastlat_diag,:) = field(:,:,:)
       ij=1
       nr = 0
       DO i=0,MaxNodes-1
          ini(i) = ij
          IF (i.ne.nproc.and.nlatsinproc_d(i).gt.0) THEN
             CALL MPI_IRECV(bufrec(ij),nlatsinproc_d(i)*imax*levs, &
                            MPI_DOUBLE_PRECISION,i,93,comm,requestr(i),ierr)
             nr = nr + 1
             ij = ij + nlatsinproc_d(i)*imax*levs
          ENDIF
       ENDDO
       DO i=1,nr
          CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
          ij = status(MPI_SOURCE)
          ip = ini(ij) - 1
          DO k=1,levs
             !CDIR NODEP
             DO j=firstandlastlat(1,ij),firstandlastlat(2,ij)
                fieldglob(1:imax,j,k)= bufrec(ip+1:ip+imax)
                ip = ip + imax
             ENDDO
          ENDDO
       ENDDO
    END IF

  END SUBROUTINE Collect_Grid_d



  SUBROUTINE Collect_Grid_Full(field, fieldglob, levs, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: levs
    REAL(KIND=r8)   , INTENT(IN) :: field(ibmax,levs,jbMax)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(imax,jmax*levs)

    INTEGER :: j, i, k, m, n, l, ic, iold, ks, j1, jc, js
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: requests(nsends_diag+1)
    INTEGER :: requestr(0:maxnodes)
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER :: stat(MPI_STATUS_SIZE,nsends_diag)
    INTEGER :: ib(0:maxnodes)

    comm = MPI_COMM_WORLD
    IF (dimrecbuf.lt.imax*jmax*levs) THEN
       dimrecbuf = imax*jmax*levs
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    IF (myid.ne.nproc) THEN
       js = myfirstlat_diag - 1
       jc = (mylastlat_diag - js)
      ELSE
       js = 0
       jc = jmax
    ENDIF
    ib(0) = 1
    m = 0
    DO k=1,nrecs_diag
       ic = 0
       DO i=m+1,myrecspr_diag(2,k)
          ic = ic + myrecs_diag(2,i)-myrecs_diag(1,i)+1
       ENDDO
       m = myrecspr_diag(2,k)
       ib(k) = ib(k-1) + ic*levs
       CALL MPI_IRECV(bufrec(ib(k-1)),ib(k)-ib(k-1),MPI_DOUBLE_PRECISION, &
                      myrecspr_diag(1,k),88,comm,requestr(k),ierr)
    ENDDO
    m = 0
    ic = 0
    iold = 0
    DO k=1,nsends_diag
        DO l=m+1,mysendspr_diag(2,k)
           j = mysends_diag(3,l)
           DO i=mysends_diag(1,l),mysends_diag(2,l)
              bufsend(ic+1:ic+levs) = field(ibperij(i,j),:,jbperij(i,j))
              ic = ic + levs
           ENDDO
        ENDDO
        CALL MPI_ISEND(bufsend(iold+1),ic-iold,MPI_DOUBLE_PRECISION,&
                       mysendspr_diag(1,k),88,comm,requests(k),ierr)
        m = mysendspr_diag(2,k)
        iold = ic
    ENDDO
    DO j=max(myfirstlat,myfirstlat_diag),min(mylastlat,mylastlat_diag)
       j1 = j-js
       DO k=1,levs
          DO i=myfirstlon(j),mylastlon(j)
             fieldglob(i,j1) = field(ibperij(i,j),k,jbperij(i,j))
          ENDDO
          j1 = j1 + jc
       ENDDO
    ENDDO
    DO k=1,nrecs_diag
       CALL MPI_WAITANY(nrecs_diag,requestr(1),index,status,ierr)
       ks = status(MPI_SOURCE)
       DO l=1,nrecs_diag
          IF (ks.eq.myrecspr_diag(1,l)) THEN
             n = l
             ic = ib(n-1) - 1
             m = myrecspr_diag(2,n-1)
             EXIT
          ENDIF
       ENDDO
       DO l = m+1,myrecspr_diag(2,n)
          j = myrecs_diag(3,l)-js
          DO i = myrecs_diag(1,l),myrecs_diag(2,l)
             j1 = j
             DO ks=1,levs
                fieldglob(i,j1) = bufrec(ic+ks)
                j1 = j1 + jc
             END DO
             ic = ic + levs
          END DO
       END DO
    END DO
    IF(nsends_diag.gt.0) CALL MPI_WAITALL(nsends_diag,requests(1),stat,ierr)
    IF (myid.ne.nproc) THEN
        ic = (mylastlat_diag-myfirstlat_diag+1)*imax*levs
        
        IF (ic.gt.0) CALL MPI_ISEND(fieldglob,ic,MPI_DOUBLE_PRECISION,&
                                    nproc,89,comm,requests(1),ierr)
        CALL MPI_WAIT(requests(1),status,ierr)
      ELSE
        ib(0) = 1
        requestr = MPI_REQUEST_NULL
        n = 0
        DO k=0,maxnodes-1
           IF (k.ne.myid) THEN
              ic = (firstandlastlat(2,k)-firstandlastlat(1,k)+1)*imax*levs
              IF (ic.gt.0) THEN
                 CALL MPI_IRECV(bufrec(ib(k)),ic,MPI_DOUBLE_PRECISION, &
                                k,89,comm,requestr(k),ierr)
                 n = n + 1
              ENDIF
             ELSE
              ic = 0
           ENDIF
           ib(k+1) = ib(k) + ic
        ENDDO
        DO k=1,n
           CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
           ks = status(MPI_SOURCE)
           ic = ib(ks) - 1
           DO l=1,levs
              j1 = (l-1)*jmax
              DO j=firstandlastlat(1,ks),firstandlastlat(2,ks)
                 Fieldglob(:,j1+j) = bufrec(ic+1:ic+imax)
                 ic = ic + imax
              ENDDO
           ENDDO
        ENDDO
        j1 = 1
        do l=1,levs
           do j=1,jmax
            write(97,*) j,l,(Fieldglob(i,j1),i=1,imaxperj(j))
            j1 = j1 + 1
           enddo
        enddo
     ENDIF
     
  END SUBROUTINE Collect_Grid_Full



  SUBROUTINE Collect_Grid_FullI(imask, fieldglob, levs, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: levs
    INTEGER(KIND=I8)   , INTENT(IN) :: imask(ibmax,levs,jbMax)
    INTEGER , INTENT(OUT):: fieldglob(imax,jmax*levs)

    INTEGER :: field(ibmax,levs,jbMax)
    INTEGER :: ibufrec(imax*jmax),ibufsend(imax*jmax)
    INTEGER :: j, i, k, m, n, l, ic, iold, ks, j1, jc, js
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: requests(nsends_diag+1)
    INTEGER :: requestr(0:maxnodes)
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER :: stat(MPI_STATUS_SIZE,nsends_diag)
    INTEGER :: ib(0:maxnodes)

    field = INT(imask,kind=i4)
    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       js = myfirstlat_diag - 1
       jc = (mylastlat_diag - js)
      ELSE
       js = 0
       jc = jmax
    ENDIF
    ib(0) = 1
    m = 0
    DO k=1,nrecs_diag
       ic = 0
       DO i=m+1,myrecspr_diag(2,k)
          ic = ic + myrecs_diag(2,i)-myrecs_diag(1,i)+1
       ENDDO
       m = myrecspr_diag(2,k)
       ib(k) = ib(k-1) + ic*levs
       CALL MPI_IRECV(ibufrec(ib(k-1)),ib(k)-ib(k-1),MPI_INTEGER, &
                      myrecspr_diag(1,k),88,comm,requestr(k),ierr)
    ENDDO
    m = 0
    ic = 0
    iold = 0
    DO k=1,nsends_diag
        DO l=m+1,mysendspr_diag(2,k)
           j = mysends_diag(3,l)
           DO i=mysends_diag(1,l),mysends_diag(2,l)
              ibufsend(ic+1:ic+levs) = field(ibperij(i,j),:,jbperij(i,j))
              ic = ic + levs
           ENDDO
        ENDDO
        CALL MPI_ISEND(ibufsend(iold+1),ic-iold,MPI_INTEGER,&
                       mysendspr_diag(1,k),88,comm,requests(k),ierr)
        m = mysendspr_diag(2,k)
        iold = ic
    ENDDO
    DO j=max(myfirstlat,myfirstlat_diag),min(mylastlat,mylastlat_diag)
       j1 = j-js
       DO k=1,levs
          DO i=myfirstlon(j),mylastlon(j)
             fieldglob(i,j1) = field(ibperij(i,j),k,jbperij(i,j))
          ENDDO
          j1 = j1 + jc
       ENDDO
    ENDDO
    DO k=1,nrecs_diag
       CALL MPI_WAITANY(nrecs_diag,requestr(1),index,status,ierr)
       ks = status(MPI_SOURCE)
       DO l=1,nrecs_diag
          IF (ks.eq.myrecspr_diag(1,l)) THEN
             n = l
             ic = ib(n-1) - 1
             m = myrecspr_diag(2,n-1)
             EXIT
          ENDIF
       ENDDO
       DO l = m+1,myrecspr_diag(2,n)
          j = myrecs_diag(3,l)-js
          DO i = myrecs_diag(1,l),myrecs_diag(2,l)
             j1 = j
             DO ks=1,levs
                fieldglob(i,j1) = ibufrec(ic+ks)
                j1 = j1 + jc
             END DO
             ic = ic + levs
          END DO
       END DO
    END DO
    IF(nsends_diag.gt.0) CALL MPI_WAITALL(nsends_diag,requests(1),stat,ierr)
    IF (myid.ne.nproc) THEN
        ic = (mylastlat_diag-myfirstlat_diag+1)*imax*levs
        
        IF (ic.gt.0) CALL MPI_ISEND(fieldglob,ic,MPI_INTEGER,&
                                    nproc,89,comm,requests(1),ierr)
        CALL MPI_WAIT(requests(1),status,ierr)
      ELSE
        ib(0) = 1
        requestr = MPI_REQUEST_NULL
        n = 0
        DO k=0,maxnodes-1
           IF (k.ne.myid) THEN
              ic = (firstandlastlat(2,k)-firstandlastlat(1,k)+1)*imax*levs
              IF (ic.gt.0) THEN
                 CALL MPI_IRECV(ibufrec(ib(k)),ic,MPI_INTEGER, &
                                k,89,comm,requestr(k),ierr)
                 n = n + 1
              ENDIF
             ELSE
              ic = 0
           ENDIF
           ib(k+1) = ib(k) + ic
        ENDDO
        DO k=1,n
           CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
           ks = status(MPI_SOURCE)
           ic = ib(ks) - 1
           DO l=1,levs
              j1 = (l-1)*jmax
              DO j=firstandlastlat(1,ks),firstandlastlat(2,ks)
                 Fieldglob(:,j1+j) = ibufrec(ic+1:ic+imax)
                 ic = ic + imax
              ENDDO
           ENDDO
        ENDDO
        j1 = 1
        do l=1,levs
           do j=1,jmax
            write(97,*) j,l,(Fieldglob(i,j1),i=1,imaxperj(j))
            j1 = j1 + 1
           enddo
        enddo
     ENDIF
     
  END SUBROUTINE Collect_Grid_FullI


  SUBROUTINE Collect_Grid_Sur_Print(fields,interp_type,nf,nproc,nunit)    
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: nf
    INTEGER, INTENT(IN) :: nunit
    INTEGER, INTENT(IN) :: interp_type(nf)
    TYPE(p2d), TARGET :: fields(nf)
    REAL(KIND=r8)   , POINTER   :: g(:,:)
    REAL(KIND=r8) :: fieldglob(imax,jmax*nf)
    REAL(KIND=r8) :: saux(imax)

    INTEGER :: j, i, k, m, n, l, ic, iold, ks, j1, j2, jc, js
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: requests(nsends_diag+1)
    INTEGER :: requestr(0:maxnodes)
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER :: stat(MPI_STATUS_SIZE,nsends_diag)
    INTEGER :: ib(0:maxnodes)

    comm = MPI_COMM_WORLD
    IF (dimrecbuf.lt.imax*jmax*nf) THEN
       dimrecbuf = imax*jmax*nf
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    IF (myid.ne.nproc) THEN
       js = myfirstlat_diag - 1
       jc = (mylastlat_diag - js)
      ELSE
       js = 0
       jc = jmax
    ENDIF
    ib(0) = 1
    m = 0
    DO k=1,nrecs_diag
       ic = 0
       DO i=m+1,myrecspr_diag(2,k)
          ic = ic + myrecs_diag(2,i)-myrecs_diag(1,i)+1
       ENDDO
       m = myrecspr_diag(2,k)
       ib(k) = ib(k-1) + ic*nf
       CALL MPI_IRECV(bufrec(ib(k-1)),ib(k)-ib(k-1),MPI_DOUBLE_PRECISION, &
                      myrecspr_diag(1,k),88,comm,requestr(k),ierr)
    ENDDO
    m = 0
    ic = 1
    iold = 1
    DO k=1,nsends_diag
        DO n=1,nf
           g => fields(n)%p
           DO l=m+1,mysendspr_diag(2,k)
              j = mysends_diag(3,l)
              DO i=mysends_diag(1,l),mysends_diag(2,l)
                 bufsend(ic) = g(ibperij(i,j),jbperij(i,j))
                 ic = ic + 1
              ENDDO
           ENDDO
        ENDDO
        CALL MPI_ISEND(bufsend(iold),ic-iold,MPI_DOUBLE_PRECISION,&
                       mysendspr_diag(1,k),88,comm,requests(k),ierr)
        m = mysendspr_diag(2,k)
        iold = ic
    ENDDO
    DO k=1,nf
       g => fields(k)%p
       DO j=max(myfirstlat,myfirstlat_diag),min(mylastlat,mylastlat_diag)
          j1 = j-js+(k-1)*jc
          DO i=myfirstlon(j),mylastlon(j)
             fieldglob(i,j1) = g(ibperij(i,j),jbperij(i,j))
          ENDDO
       ENDDO
    ENDDO
    DO k=1,nrecs_diag
       CALL MPI_WAITANY(nrecs_diag,requestr(1),index,status,ierr)
       ks = status(MPI_SOURCE)
       DO l=1,nrecs_diag
          IF (ks.eq.myrecspr_diag(1,l)) THEN
             n = l
             ic = ib(n-1)
             m = myrecspr_diag(2,n-1)
             EXIT
          ENDIF
       ENDDO
       DO ks=1,nf
          DO l = m+1,myrecspr_diag(2,n)
             j = myrecs_diag(3,l)-js+(ks-1)*jc
             DO i = myrecs_diag(1,l),myrecs_diag(2,l)
                fieldglob(i,j) = bufrec(ic)
                ic = ic + 1
             END DO
          END DO
       END DO
    END DO
    IF(nsends_diag.gt.0) CALL MPI_WAITALL(nsends_diag,requests(1),stat,ierr)
    IF (reducedgrid) THEN
       DO k=1,nf
          DO j=myfirstlat_diag,mylastlat_diag
             j1 = j-js+(k-1)*jc
             saux(1:imaxperj(j)) = fieldglob(1:imaxperj(j),j1)
             IF (interp_type(k).eq.1) THEN
                CALL CyclicLinear(iMaxPerJ(j), iMax, &
                                  saux,fieldglob(1,j1),1,imax)
             ELSEIF (interp_type(k).eq.2) THEN
                CALL CyclicNearest_r(iMaxPerJ(j), iMax, &
                                     saux,fieldglob(1,j1),1,imax)
             ELSEIF (interp_type(k).eq.3) THEN
                CALL CyclicLinear_ABS(iMaxPerJ(j), iMax, &
                                     saux,fieldglob(1,j1),1,imax)
             END IF
          ENDDO
       ENDDO
    ENDIF
    IF (myid.ne.nproc) THEN
        ic = (mylastlat_diag-myfirstlat_diag+1)*imax*nf
        
        IF (ic.gt.0) CALL MPI_ISEND(fieldglob,ic,MPI_DOUBLE_PRECISION,&
                                    nproc,89,comm,requests(1),ierr)
        CALL MPI_WAIT(requests(1),status,ierr)
      ELSE
        ib(0) = 1
        requestr = MPI_REQUEST_NULL
        n = 0
        DO k=0,maxnodes-1
           IF (k.ne.myid) THEN
              ic = (firstandlastlat(2,k)-firstandlastlat(1,k)+1)*imax*nf
              IF (ic.gt.0) THEN
                 CALL MPI_IRECV(bufrec(ib(k)),ic,MPI_DOUBLE_PRECISION, &
                                k,89,comm,requestr(k),ierr)
                 n = n + 1
              ENDIF
             ELSE
              ic = 0
           ENDIF
           ib(k+1) = ib(k) + ic
        ENDDO
        DO k=1,n
           CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
           ks = status(MPI_SOURCE)
           ic = ib(ks) - 1
           DO l=1,nf
              j1 = (l-1)*jmax
              DO j=firstandlastlat(1,ks),firstandlastlat(2,ks)
                 Fieldglob(:,j1+j) = bufrec(ic+1:ic+imax)
                 ic = ic + imax
              ENDDO
           ENDDO
        ENDDO
        DO l=1,nf
           j1 = (l-1)*jmax+1
           j2 = l * jmax
           CALL FWriteField(nunit,Fieldglob(:,j1:j2))
        ENDDO
     ENDIF
     
  END SUBROUTINE Collect_Grid_Sur_Print

! add solange 28-07-2011 para letkf. Solucao para chamada de outra interface para a escrita dos campos (WriteField em GridDump.f90)
  !SUBROUTINE Collect_Grid_Sur_Print2(fields,interp_type,nf,nproc,nunit)    
  SUBROUTINE Collect_Grid_Sur_Print2(fields,interp_type,registro_dado,nf,nregtot,nproc,nunit)

    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: nf
    INTEGER, INTENT(IN) :: nunit
    INTEGER, INTENT(IN) :: interp_type(nf)
    INTEGER, INTENT(IN) :: registro_dado(nf+1)   !add solange. Info sobre a ordem dos campos a serem escritos, poix existem campos 3D
                                               ! a serem escritos num arquivo sequential.
    INTEGER, INTENT(IN) :: nregtot             !add solange. Numero total de registro. nregtot=<nf
    
    
    TYPE(p2d), TARGET :: fields(nf)
    REAL(KIND=r8)   , POINTER   :: g(:,:)
    REAL(KIND=r8) :: fieldglob(imax,jmax*nf)
    REAL(KIND=r8) :: saux(imax)
    REAL(KIND=r8), ALLOCATABLE, DIMENSION(:,:,:) :: VarOut   !VarOut(imax,jmax,nregtot)  !add solange. Valor maximo da matrix para o indice 3D.

    INTEGER :: j, i, k, m, n, l, ic, iold, ks, j1, j2, jc, js,ii
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: requests(nsends_diag+1)
    INTEGER :: requestr(0:maxnodes)
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER :: stat(MPI_STATUS_SIZE,nsends_diag)
    INTEGER :: ib(0:maxnodes)

    comm = MPI_COMM_WORLD
    IF (dimrecbuf.lt.imax*jmax*nf) THEN
       dimrecbuf = imax*jmax*nf
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    IF (myid.ne.nproc) THEN
       js = myfirstlat_diag - 1
       jc = (mylastlat_diag - js)
      ELSE
       js = 0
       jc = jmax
    ENDIF
    ib(0) = 1
    m = 0
    DO k=1,nrecs_diag
       ic = 0
       DO i=m+1,myrecspr_diag(2,k)
          ic = ic + myrecs_diag(2,i)-myrecs_diag(1,i)+1
       ENDDO
       m = myrecspr_diag(2,k)
       ib(k) = ib(k-1) + ic*nf
       CALL MPI_IRECV(bufrec(ib(k-1)),ib(k)-ib(k-1),MPI_DOUBLE_PRECISION, &
                      myrecspr_diag(1,k),88,comm,requestr(k),ierr)
    ENDDO
    m = 0
    ic = 1
    iold = 1
    DO k=1,nsends_diag
        DO n=1,nf
           g => fields(n)%p
           DO l=m+1,mysendspr_diag(2,k)
              j = mysends_diag(3,l)
              DO i=mysends_diag(1,l),mysends_diag(2,l)
                 bufsend(ic) = g(ibperij(i,j),jbperij(i,j))
                 ic = ic + 1
              ENDDO
           ENDDO
        ENDDO
        CALL MPI_ISEND(bufsend(iold),ic-iold,MPI_DOUBLE_PRECISION,&
                       mysendspr_diag(1,k),88,comm,requests(k),ierr)
        m = mysendspr_diag(2,k)
        iold = ic
    ENDDO
    DO k=1,nf
       g => fields(k)%p
       DO j=max(myfirstlat,myfirstlat_diag),min(mylastlat,mylastlat_diag)
          j1 = j-js+(k-1)*jc
          DO i=myfirstlon(j),mylastlon(j)
             fieldglob(i,j1) = g(ibperij(i,j),jbperij(i,j))
          ENDDO
       ENDDO
    ENDDO
    DO k=1,nrecs_diag
       CALL MPI_WAITANY(nrecs_diag,requestr(1),index,status,ierr)
       ks = status(MPI_SOURCE)
       DO l=1,nrecs_diag
          IF (ks.eq.myrecspr_diag(1,l)) THEN
             n = l
             ic = ib(n-1)
             m = myrecspr_diag(2,n-1)
             EXIT
          ENDIF
       ENDDO
       DO ks=1,nf
          DO l = m+1,myrecspr_diag(2,n)
             j = myrecs_diag(3,l)-js+(ks-1)*jc
             DO i = myrecs_diag(1,l),myrecs_diag(2,l)
                fieldglob(i,j) = bufrec(ic)
                ic = ic + 1
             END DO
          END DO
       END DO
    END DO
    IF(nsends_diag.gt.0) CALL MPI_WAITALL(nsends_diag,requests(1),stat,ierr)
    IF (reducedgrid) THEN
       DO k=1,nf
          DO j=myfirstlat_diag,mylastlat_diag
             j1 = j-js+(k-1)*jc
             saux(1:imaxperj(j)) = fieldglob(1:imaxperj(j),j1)
             IF (interp_type(k).eq.1) THEN
                CALL CyclicLinear(iMaxPerJ(j), iMax, &
                                  saux,fieldglob(1,j1),1,imax)
             ELSEIF (interp_type(k).eq.2) THEN
                CALL CyclicNearest_r(iMaxPerJ(j), iMax, &
                                     saux,fieldglob(1,j1),1,imax)
             ELSEIF (interp_type(k).eq.3) THEN
                CALL CyclicLinear_ABS(iMaxPerJ(j), iMax, &
                                     saux,fieldglob(1,j1),1,imax)
             END IF
          ENDDO
       ENDDO
    ENDIF
    IF (myid.ne.nproc) THEN
        ic = (mylastlat_diag-myfirstlat_diag+1)*imax*nf
        
        IF (ic.gt.0) CALL MPI_ISEND(fieldglob,ic,MPI_DOUBLE_PRECISION,&
                                    nproc,89,comm,requests(1),ierr)
        CALL MPI_WAIT(requests(1),status,ierr)
    ELSE
        ib(0) = 1
        requestr = MPI_REQUEST_NULL
        n = 0
        DO k=0,maxnodes-1
           IF (k.ne.myid) THEN
              ic = (firstandlastlat(2,k)-firstandlastlat(1,k)+1)*imax*nf
              IF (ic.gt.0) THEN
                 CALL MPI_IRECV(bufrec(ib(k)),ic,MPI_DOUBLE_PRECISION, &
                                k,89,comm,requestr(k),ierr)
                 n = n + 1
              ENDIF
             ELSE
              ic = 0
           ENDIF
           ib(k+1) = ib(k) + ic
        ENDDO
        DO k=1,n
           CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
           ks = status(MPI_SOURCE)
           ic = ib(ks) - 1
           DO l=1,nf
              j1 = (l-1)*jmax
              DO j=firstandlastlat(1,ks),firstandlastlat(2,ks)
                 Fieldglob(:,j1+j) = bufrec(ic+1:ic+imax)
                 ic = ic + imax
              ENDDO
           ENDDO
        ENDDO
        !
        ! solange: modificado
        ! Fieldglob(:,j1:j2) serah agora 2D ou 3D, olhando o "registro_dado(nf)"
        ! Alocar uma nova variavel para o write ("VarOut")
        !
        ii=0
        DO l=1,nf
           j1 = (l-1)*jmax+1
           j2 = l * jmax
           IF ( registro_dado(l) /= registro_dado(l+1) )  THEN
               if ( ii==0 ) then
                  CALL FWriteFieldb(nunit,Fieldglob(:,j1:j2))
               else
                  ! guarda o ultimo dado na matriz referente ao registro_dado(l)
                  ! escreve registro contendo "ii" dados
                  ii=ii+1
                  !IF (.NOT.ALLOCATED(VarOut)) ALLOCATE(VarOut(imax,jmax,1))
                  varOut(:,:,ii)=Fieldglob(:,j1:j2)  ! registro para o campo mm
                  CALL FWriteFieldb(nunit,varOut(:,:,1:ii)) 
                  DEALLOCATE (varOut)
                  ii=0
               endif
           ELSE
              ! guarda "ii" dado na matriz referente ao registro
              IF (.NOT.ALLOCATED(VarOut)) ALLOCATE(VarOut(imax,jmax,nregtot))
              ii=ii+1
              varOut(1:imax,1:jmax,ii)=Fieldglob(:,j1:j2)  ! registro para o campo mm            
           ENDIF
       ENDDO
       IF (ALLOCATED(VarOut)) DEALLOCATE (VarOut)
    ENDIF
     
  END SUBROUTINE Collect_Grid_Sur_Print2

  SUBROUTINE Collect_Gauss(gauss, gauss_out, nf)  
    !
    INTEGER, INTENT(IN) :: nf
    REAL(KIND=r8), INTENT(IN)  :: gauss(ibmax,nf,jbmax)
    REAL(KIND=r8), INTENT(OUT) :: gauss_out(imax,myjmax_d,nf)
    REAL(KIND=r8) :: saux(imax)

    INTEGER :: j, i, k, m, n, l, ic, iold, ks, j1
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: requests(nsends_diag+1)
    INTEGER :: requestr(0:maxnodes)
    INTEGER :: status(MPI_STATUS_SIZE)
    INTEGER :: stat(MPI_STATUS_SIZE,nsends_diag)
    INTEGER :: ib(0:maxnodes)

    comm = MPI_COMM_WORLD
    IF (dimrecbuf.lt.imax*jmax*nf) THEN
       dimrecbuf = imax*jmax*nf
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    
    IF (dimsendbuf.lt.ibmax*jbmax*nf) THEN
       dimsendbuf = ibmax*jbmax*nf
       DEALLOCATE (bufsend)
       ALLOCATE (bufsend(dimsendbuf))
    ENDIF
    
    ib(0) = 1
    m = 0
    DO k=1,nrecs_diag
       ic = 0
       DO i=m+1,myrecspr_diag(2,k)
          ic = ic + myrecs_diag(2,i)-myrecs_diag(1,i)+1
       ENDDO
       m = myrecspr_diag(2,k)
       ib(k) = ib(k-1) + ic*nf
       CALL MPI_IRECV(bufrec(ib(k-1)),ib(k)-ib(k-1),MPI_DOUBLE_PRECISION, &
                      myrecspr_diag(1,k),88,comm,requestr(k),ierr)
    ENDDO
    m = 0
    ic = 1
    iold = 1
    DO k=1,nsends_diag
        DO n=1,nf
           DO l=m+1,mysendspr_diag(2,k)
              j = mysends_diag(3,l)
              DO i=mysends_diag(1,l),mysends_diag(2,l)
                 bufsend(ic) = gauss(ibperij(i,j),n,jbperij(i,j))
                 ic = ic + 1
              ENDDO
           ENDDO
        ENDDO
        CALL MPI_ISEND(bufsend(iold),ic-iold,MPI_DOUBLE_PRECISION,&
                       mysendspr_diag(1,k),88,comm,requests(k),ierr)
        m = mysendspr_diag(2,k)
        iold = ic
    ENDDO
    DO k=1,nf
       DO j=max(myfirstlat,myfirstlat_diag),min(mylastlat,mylastlat_diag)
          j1 = j-myfirstlat_diag+1
          DO i=myfirstlon(j),mylastlon(j)
             gauss_out(i,j1,k) = gauss(ibperij(i,j),k,jbperij(i,j))
          ENDDO
       ENDDO
    ENDDO
    DO k=1,nrecs_diag
       CALL MPI_WAITANY(nrecs_diag,requestr(1),index,status,ierr)
       ks = status(MPI_SOURCE)
       DO l=1,nrecs_diag
          IF (ks.eq.myrecspr_diag(1,l)) THEN
             n = l
             ic = ib(n-1)
             m = myrecspr_diag(2,n-1)
             EXIT
          ENDIF
       ENDDO
       DO ks=1,nf
          DO l = m+1,myrecspr_diag(2,n)
             j = myrecs_diag(3,l)-myfirstlat_diag+1
             DO i = myrecs_diag(1,l),myrecs_diag(2,l)
                gauss_out(i,j,ks) = bufrec(ic)
                ic = ic + 1
             END DO
          END DO
       END DO
    END DO
    IF(nsends_diag.gt.0) CALL MPI_WAITALL(nsends_diag,requests(1),stat,ierr)
    IF (reducedgrid) THEN
       DO k=1,nf
          DO j=myfirstlat_diag,mylastlat_diag
             j1 = j-myfirstlat_diag+1
             saux(1:imaxperj(j)) = gauss_out(1:imaxperj(j),j1,k)
             CALL CyclicLinear(iMaxPerJ(j), iMax, &
                                saux,gauss_out(1,j1,k),1,imax)
          ENDDO
       ENDDO
    ENDIF
     
  END SUBROUTINE Collect_Gauss



  SUBROUTINE Collect_Spec(field, fieldglob, levs, levsg, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor (has to be the first 
                                 ! processor of one fourier group)
    INTEGER, INTENT(IN) :: levs
    INTEGER, INTENT(IN) :: levsg
    REAL(KIND=r8)   , INTENT(IN) :: field(2*mymnmax,levs)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(2*mnmax,levsg)

    CHARACTER(LEN=*), PARAMETER :: h="**(Collect_Spec)**"
    INTEGER :: j, i, m, mn, mnloc, ns, l, lev, kdim, kp, kl, ll
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes)
    INTEGER :: statu(MPI_STATUS_SIZE)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    !
    !   Collect inside fourier groups (to first processor in each group)
    !
    IF (.not.ANY(first_proc_four.eq.nproc)) THEN
       WRITE(nfprt,*) ' nproc ',nproc 
       WRITE(nfprt,"(a, ' Spectral fields should be collected to a first processor in a fourier group')") h
       STOP h
    ELSE IF (levsg.eq.1.and..not.havesurf) THEN
       WRITE(nfprt,*) ' myid  ',myid  
       WRITE(nfprt,"(a, ' should not be calling collect_spec of surface field')") h
       STOP h
    ELSE IF (levsg.ne.1.and.levsg.ne.kmax) THEN
       WRITE(nfprt,*) ' levsg ',levsg 
       WRITE(nfprt,"(a, ' collect_spec should be used for a global or a surface spectral field')") h
       STOP h
    END IF
    comm = COMM_FOUR
    kdim = 2*mnmaxlocal*levs
    IF (dimrecbuf.lt.kdim*maxnodes_four) THEN
       dimrecbuf = kdim*maxnodes_four
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    IF (myid_four.ne.0) THEN
       CALL MPI_ISEND(field,2*levs*mymnmax,MPI_DOUBLE_PRECISION,0,95,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(0) = MPI_REQUEST_NULL
       DO i=1,MaxNodes_four-1
          CALL MPI_IRECV(bufrec(1+i*kdim),2*mnsPerProc(i)*levs, &
                       MPI_DOUBLE_PRECISION,i,95,comm,requestr(i),ierr)
       ENDDO
       mnloc=0
       mn=0
       kl = myfirstlev - 1
       DO m=1,Mmax
          ns=2*(Mmax-m+1)
          IF(NodeHasM(m,mygroup_four).eq.0) THEN
             DO l=1,ns
                fieldglob(mn+l,kl+1:kl+levs) = field(mnloc+l,:)
             ENDDO
             mnloc = mnloc+ns
          ENDIF
          mn = mn + ns
       ENDDO
       DO i=1,MaxNodes_four-1
          CALL MPI_WAITANY(MaxNodes_four,requestr(0),index,statu,ierr)
          j = statu(MPI_SOURCE)
          DO lev=1,levs
             mnloc = 2*mnsPerProc(j)*(lev-1)
             mn=0
             DO m=1,Mmax
                ns=2*(Mmax-m+1)
                IF(NodeHasM(m,mygroup_four).eq.j) THEN
                   DO l=1,ns
                      fieldglob(mn+l,kl+lev) = bufrec(mnloc+l+j*kdim)
                   ENDDO
                   mnloc = mnloc+ns
                ENDIF
                mn = mn + ns
             ENDDO
          ENDDO
       ENDDO

       IF (levsg.eq.1.or.Ngroups_four.eq.1) RETURN
       !   Collect Global Field
       !
       comm = MPI_COMM_WORLD
       IF (myid.ne.nproc) THEN
          CALL MPI_ISEND(fieldglob(1,kl+1),2*levs*mnmax,MPI_DOUBLE_PRECISION, &
                         nproc,96,comm,request,ierr)
          CALL MPI_WAIT(request,status,ierr)
         ELSE
          requestr(1:Ngroups_four) = MPI_REQUEST_NULL
          DO i=1,Ngroups_four
             kp = first_proc_four(i)
             kl = kfirst_four(kp)
             ll = nlevperg_four(i)
             IF (kp.ne.nproc) THEN
                CALL MPI_IRECV(fieldglob(1,kl),2*ll*mnmax, &
                               MPI_DOUBLE_PRECISION,kp,96,comm,requestr(i),ierr)
             ENDIF 
          ENDDO
          CALL MPI_WAITALL(Ngroups_four,requestr(1),status,ierr)
       ENDIF
    ENDIF

  END SUBROUTINE Collect_Spec


  SUBROUTINE Spread_surf_Spec(field)
    !
    REAL(KIND=r8)   , INTENT(INOUT) :: field(2*mymnmax)

    CHARACTER(LEN=*), PARAMETER :: h="**(Spread_surf_Spec)**"
    INTEGER :: i, m, mng, len, np, n
    INTEGER :: ini(maxnodes)
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: requestr(0:MaxNodes)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    !
    ! 
    IF (ngroups_four.eq.1) RETURN
    comm = MPI_COMM_WORLD
    requestr = MPI_REQUEST_NULL
    ini(1) = 0
    DO n=1,ncomm_spread
       ini(n+1) = ini(n) + comm_spread(n,2)
    ENDDO
    IF (mygroup_four.EQ.1) THEN
       DO n=2,ngroups_four
          mng = 0
          DO m=1,mymmax
             len = 2 * (MMax + 1 - lm2m(m))
             np = ms_spread(m,n)
             bufsend(ini(np)+1:ini(np)+len) = field(mng+1:mng+len)
             ini(np) = ini(np) + len
             mng = mng + len 
          ENDDO
       ENDDO
       ini(1) = 1
       DO n=1,ncomm_spread
          ini(n+1) = ini(n) + comm_spread(n,2)
          CALL MPI_ISEND(bufsend(ini(n)),comm_spread(n,2),MPI_DOUBLE_PRECISION,comm_spread(n,1),75,comm,requestr(n),ierr)
       ENDDO
       CALL MPI_WAITALL(ncomm_spread,requestr(1),status,ierr)
      ELSE
       DO i=1,ncomm_spread
          CALL MPI_IRECV(bufrec(ini(i)+1),comm_spread(i,2), &
               MPI_DOUBLE_PRECISION,comm_spread(i,1),75,comm,requestr(i),ierr)
       ENDDO
       CALL MPI_WAITALL(ncomm_spread,requestr(1),status,ierr)
       mng = 0
       DO m=1,mymmax
          len = 2 * (MMax + 1 - lm2m(m))
          np = ms_spread(m,1)
          field(mng+1:mng+len) = bufrec(ini(np)+1:ini(np)+len)
          ini(np) = ini(np) + len
          mng = mng + len 
       ENDDO
    ENDIF

  END SUBROUTINE Spread_surf_Spec



  SUBROUTINE Collect_Spec_Ext(field, fieldglob, levs, levsg, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: levs
    INTEGER, INTENT(IN) :: levsg
    REAL(KIND=r8)   , INTENT(IN) :: field(2*mymnextmax,levs)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(2*mnextmax,levsg)

    CHARACTER(LEN=*), PARAMETER :: h="**(Collect_Spec_Ext)**"
    INTEGER :: j, i, m, mn, mnloc, ns, l, lev, kdim, kp, kl, ll
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes)
    INTEGER :: statu(MPI_STATUS_SIZE)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    !
    !   Collect inside fourier groups (to first processor in each group)
    !
    IF (.not.ANY(first_proc_four.eq.nproc)) THEN
       WRITE(nfprt,*) ' nproc ',nproc 
       WRITE(nfprt,"(a, ' Spectral fields should be collected to a first processor in a fourier group')") h
       STOP h
    ELSE IF (levs.eq.1.and..not.havesurf) THEN
       WRITE(nfprt,*) ' myid  ',myid  
       WRITE(nfprt,"(a, ' should not be calling collect_spec_ext of surface field')") h
       STOP h
    ELSE IF (levsg.ne.1.and.levsg.ne.kmax) THEN
       WRITE(nfprt,*) ' levsg ',levsg 
       WRITE(nfprt,"(a, ' collect_spec_ext should be used for a global or a surface spectral field')") h
       STOP h
    END IF
    comm = COMM_FOUR
    kdim = 2*mnextmaxlocal*levs
    IF (dimrecbuf.lt.kdim*maxnodes_four) THEN
       dimrecbuf = kdim*maxnodes_four
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    IF (myid_four.ne.0) THEN
       CALL MPI_ISEND(field,2*levs*mymnextmax,MPI_DOUBLE_PRECISION, &
                      0,95,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(0) = MPI_REQUEST_NULL
       DO i=1,MaxNodes_four-1
          CALL MPI_IRECV(bufrec(1+i*kdim),2*mnsExtPerProc(i)*levs, &
                       MPI_DOUBLE_PRECISION,i,95,comm,requestr(i),ierr)
       ENDDO
       mnloc=0
       mn=0
       kl = myfirstlev - 1
       DO m=1,Mmax
          ns=2*(Mmax-m+2)
          IF(NodeHasM(m,mygroup_four).eq.0) THEN
             DO l=1,ns
                fieldglob(mn+l,kl+1:kl+levs) = field(mnloc+l,:)
             ENDDO
             mnloc = mnloc+ns
          ENDIF
          mn = mn + ns
       ENDDO
       DO i=1,MaxNodes_four-1
          CALL MPI_WAITANY(MaxNodes_four,requestr(0),index,statu,ierr)
          j = statu(MPI_SOURCE)
          DO lev=1,levs
             mnloc = 2*mnsExtPerProc(j)*(lev-1)
             mn=0
             DO m=1,Mmax
                ns=2*(Mmax-m+2)
                IF(NodeHasM(m,mygroup_four).eq.j) THEN
                   DO l=1,ns
                      fieldglob(mn+l,kl+lev) = bufrec(mnloc+l+j*kdim)
                   ENDDO
                   mnloc = mnloc+ns
                ENDIF
                mn = mn + ns
             ENDDO
          ENDDO
       ENDDO

       IF (levsg.eq.1.or.Ngroups_four.eq.1) RETURN
       !   Collect Global Field
       !
       comm = MPI_COMM_WORLD
       IF (myid.ne.nproc) THEN
          CALL MPI_ISEND(fieldglob(1,kl+1),2*levs*mnextmax,MPI_DOUBLE_PRECISION, &
                         nproc,96,comm,request,ierr)
          CALL MPI_WAIT(request,status,ierr)
         ELSE
          requestr(1:Ngroups_four) = MPI_REQUEST_NULL
          DO i=1,Ngroups_four
             kp = first_proc_four(i)
             kl = kfirst_four(kp)
             ll = nlevperg_four(i)
             IF (kp.ne.nproc) THEN
                CALL MPI_IRECV(fieldglob(1,kl),2*ll*mnextmax, &
                               MPI_DOUBLE_PRECISION,kp,96,comm,requestr(i),ierr)
             ENDIF 
          ENDDO
          CALL MPI_WAITALL(Ngroups_four,requestr(1),status,ierr)
       ENDIF
    ENDIF

  END SUBROUTINE Collect_Spec_Ext

  SUBROUTINE Exchange_ftog(nrecs_f,nrecs_g)
    !
    INTEGER, INTENT(IN) :: nrecs_f
    INTEGER, INTENT(OUT) :: nrecs_g

    INTEGER :: i, m, k
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: ns(0:MaxNodes-1)
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: requests(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)
    ns=0
    nrecs_g=0
    comm = MPI_COMM_WORLD
    requestr(myid) = MPI_REQUEST_NULL
    requests(myid) = MPI_REQUEST_NULL
    messproc_g(2,0) = 0
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
             CALL MPI_IRECV(messproc_g(2,i+1),1,MPI_INTEGER,i,18,&
                            comm,requestr(i),ierr)
          ELSE
             messproc_g(2,i+1) = 0
       ENDIF
    ENDDO
    m = 0
    k = 1
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
          IF (k.le.nrecs_f.and.i.eq.messproc_f(1,k)) THEN
             ns(i) = messproc_f(2,k) - m
             m = messproc_f(2,k)
             k = k + 1
            ELSE
             ns(i) = 0
          ENDIF
          CALL MPI_ISEND(ns(i),1,MPI_INTEGER,i,18,comm,requests(i),ierr)
       ENDIF
    ENDDO
    CALL MPI_WAITALL(MaxNodes,requestr(0),status,ierr)
    CALL MPI_WAITALL(MaxNodes,requests(0),status,ierr)
    k = 0
    m = 0
    DO i=0,MaxNodes-1
       IF (messproc_g(2,i+1).ne.0) THEN
          k = k + 1
          m = m + messproc_g(2,i+1)
          messproc_g(2,k) = m
          messproc_g(1,k) = i
       ENDIF
    ENDDO
    nrecs_g = k
    m = 0
    DO i=1,nrecs_g
       ns(i) = messproc_g(2,i) - m
       CALL MPI_IRECV(messages_g(1,m+1),4*ns(i),MPI_INTEGER,messproc_g(1,i),19,&
                            comm,requestr(i),ierr)
       m = messproc_g(2,i)
    ENDDO
    m = 0
    DO i=1,nrecs_f
       ns(i) = messproc_f(2,i) - m
       CALL MPI_ISEND(messages_f(1,m+1),ns(i)*4,MPI_INTEGER,messproc_f(1,i),19,&
                            comm,requests(i),ierr)
       m = messproc_f(2,i)
    ENDDO
    IF (nrecs_g.gt.0) CALL MPI_WAITALL(nrecs_g,requestr(1),status,ierr)
    IF (nrecs_f.gt.0) CALL MPI_WAITALL(nrecs_f,requests(1),status,ierr)
    
  END SUBROUTINE Exchange_ftog  

  SUBROUTINE Exchange_diag()
    !

    INTEGER :: i, m, k
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: ns(0:MaxNodes)
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: requests(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    comm = MPI_COMM_WORLD
    requestr(myid) = MPI_REQUEST_NULL
    requests(myid) = MPI_REQUEST_NULL
    mysendspr_diag(2,0) = 0
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
             CALL MPI_IRECV(mysendspr_diag(2,i+1),1,MPI_INTEGER,i,18,&
                            comm,requestr(i),ierr)
          ELSE
             mysendspr_diag(2,i+1) = 0
       ENDIF
    ENDDO
    m = 0
    k = 1
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
          IF (k.le.nrecs_diag.and.i.eq.myrecspr_diag(1,k)) THEN
             ns(i) = myrecspr_diag(2,k) - m
             m = myrecspr_diag(2,k)
             k = k + 1
            ELSE
             ns(i) = 0
          ENDIF
          CALL MPI_ISEND(ns(i),1,MPI_INTEGER,i,18,comm,requests(i),ierr)
       ENDIF
    ENDDO
    CALL MPI_WAITALL(MaxNodes,requestr(0),status,ierr)
    CALL MPI_WAITALL(MaxNodes,requests(0),status,ierr)
    k = 0
    m = 0
    DO i=0,MaxNodes-1
       IF (mysendspr_diag(2,i+1).ne.0) THEN
          k = k + 1
          m = m + mysendspr_diag(2,i+1)
          mysendspr_diag(2,k) = m
          mysendspr_diag(1,k) = i
       ENDIF
    ENDDO
    ALLOCATE (mysends_diag   (4,m))    ;  mysends_diag  (1:4,1:m)  =0
    nsends_diag = k
    m = 0
    DO i=1,nsends_diag
       ns(i) = mysendspr_diag(2,i) - m
       CALL MPI_IRECV(mysends_diag(1,m+1),4*ns(i),MPI_INTEGER,mysendspr_diag(1,i),19,&
                            comm,requestr(i),ierr)
       m = mysendspr_diag(2,i)
    ENDDO
    m = 0
    DO i=1,nrecs_diag
       ns(i) = myrecspr_diag(2,i) - m
       CALL MPI_ISEND(myrecs_diag(1,m+1),ns(i)*4,MPI_INTEGER,myrecspr_diag(1,i),19,&
                            comm,requests(i),ierr)
       m = myrecspr_diag(2,i)
    ENDDO
    IF (nsends_diag.gt.0) CALL MPI_WAITALL(nsends_diag,requestr(1),status,ierr)
    IF (nrecs_diag.gt.0) CALL MPI_WAITALL(nrecs_diag,requests(1),status,ierr)
    
  END SUBROUTINE Exchange_diag 

  SUBROUTINE Exchange_si(ibs,ibr,nsend,mysends)
    !
    INTEGER, INTENT(IN) :: nsend
    INTEGER, INTENT(IN) :: ibs(nsend+1)
    INTEGER, INTENT(IN) :: mysends(nsend)
    INTEGER, INTENT(OUT) :: ibr(0:maxnodes-1)

    INTEGER :: i
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: ns(0:MaxNodes-1)
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: requests(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    comm = MPI_COMM_WORLD
    requestr(myid) = MPI_REQUEST_NULL
    requests(myid) = MPI_REQUEST_NULL
    ibr = 0
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
             CALL MPI_IRECV(ibr(i),1,MPI_INTEGER,i,28,&
                            comm,requestr(i),ierr)
       ENDIF
    ENDDO
    ns = 0
    DO i=1,nsend
       ns(mysends(i)) = ibs(i+1)-ibs(i)
    END DO
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
          CALL MPI_ISEND(ns(i),1,MPI_INTEGER,i,28,comm,requests(i),ierr)
        ELSE
          ibr(i) = ns(i)
       ENDIF
    ENDDO
    CALL MPI_WAITALL(MaxNodes,requestr(0),status,ierr)
    CALL MPI_WAITALL(MaxNodes,requests(0),status,ierr)
    
  END SUBROUTINE Exchange_si

  SUBROUTINE SpectoSi(ibs,ibr,nsend,nrec,mysends,myrecs,maps,mapr,kmg,kml,&
                      nlfour,q1,q2,q1_si,q2_si,q3,q3_si,qs1,qs1_si)
    !
    INTEGER, INTENT(IN) :: nsend
    INTEGER, INTENT(IN) :: nrec
    INTEGER, INTENT(IN) :: ibs(nsend+1)
    INTEGER, INTENT(IN) :: ibr(nrec+1)
    INTEGER, INTENT(IN) :: mysends(nsend)
    INTEGER, INTENT(IN) :: myrecs(nrec)
    INTEGER, INTENT(IN) :: maps(0:maxnodes-1)
    INTEGER, INTENT(IN) :: mapr(0:maxnodes-1)
    INTEGER, INTENT(IN) :: kml
    INTEGER, INTENT(IN) :: kmg
    INTEGER, INTENT(IN) :: nlfour(*)
    REAL(KIND=r8)   , INTENT(IN) :: q1(2*mymnmax,kml)
    REAL(KIND=r8)   , INTENT(IN) :: q2(2*mymnmax,kml)
    REAL(KIND=r8)   , INTENT(OUT):: q1_si(2*mnmax_si,kmg)
    REAL(KIND=r8)   , INTENT(OUT):: q2_si(2*mnmax_si,kmg)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(IN) :: q3(2*mymnmax,kml)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(IN) :: qs1(2*mymnmax)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(OUT):: q3_si(2*mnmax_si,kmg)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(OUT):: qs1_si(2*mnmax_si)

    INTEGER :: i, mn, mnr, idest, l, m, km, ndim
    INTEGER :: iself, id, k, iproc, mng
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: ips(nsend)
    INTEGER :: ipr(nrec)
    INTEGER :: requestr(nrec)
    INTEGER :: requests(nsend)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)
    LOGICAL :: dosurf

    ndim = max(ibs(nsend+1),ibr(nrec+1))
    IF (dimrecbuf.lt.ndim) THEN
       dimrecbuf = ndim
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
 
    IF (dimsendbuf.lt.ndim) THEN
       dimsendbuf = ndim
       DEALLOCATE (bufsend)
       ALLOCATE (bufsend(dimsendbuf))
    ENDIF
    comm = MPI_COMM_WORLD

    DO i=1,nrec 
       idest = myrecs(i)
       IF (idest.ne.myid) THEN
          CALL MPI_IRECV(bufrec(ibr(i)),ibr(i+1)-ibr(i),MPI_DOUBLE_PRECISION,&
               idest,38,comm,requestr(i),ierr)
        ELSE
          requestr(i) = MPI_REQUEST_NULL
          iself = i
       ENDIF
    ENDDO

    ips = ibs(1:nsend)-1
    ipr = ibr(1:nrec)-1
    dosurf = havesurf.and.PRESENT(qs1)
    DO mn=1,mymnmax
       idest = maps(mnsendsmap_si(mn))
       mnr = 2 * mn
       id = ips(idest)
       bufsend(id+1:id+kml) = q1(mnr-1,:)
       id =id+kml
       bufsend(id+1:id+kml) = q1(mnr,:)
       id =id+kml
       bufsend(id+1:id+kml) = q2(mnr-1,:)
       id =id+kml
       bufsend(id+1:id+kml) = q2(mnr,:)
       id =id+kml
       IF (PRESENT(q3)) THEN
          bufsend(id+1:id+kml) = q3(mnr-1,:)
          id =id+kml
          bufsend(id+1:id+kml) = q3(mnr,:)
          id =id+kml
       END IF
       IF (dosurf) THEN
          bufsend(id+1) = qs1(mnr-1)
          bufsend(id+2) = qs1(mnr)
          id =id+2
       END IF
       ips(idest) = id
    END DO
    DO i=1,nsend
       idest = mysends(i)
       IF (idest.ne.myid) THEN
          CALL MPI_ISEND(bufsend(ibs(i)),ips(i)-ibs(i)+1,MPI_DOUBLE_PRECISION, &
               idest,38,comm,requests(i),ierr)
        ELSE
          requests(i) = MPI_REQUEST_NULL
          bufrec(ibr(iself):ibr(iself)+ips(i)-ibs(i)) = bufsend(ibs(i):ips(i))
       ENDIF
    END DO
    CALL MPI_WAITALL(nrec,requestr(1),status,ierr)

    i = 0
    DO mn=1,mnmax_si
       mnr = 2 * mn
       mng = mymnmap_si(mn)
       m = mmap(mng)
       k = 0
       DO l=1,ngroups_four
          km = nlfour(l)
          IF (km.gt.0) THEN
             iproc = mapr(map_four(l,nodehasM(m,l)))
             id = ipr(iproc)
             q1_si(mnr-1,k+1:k+km) =  bufrec(id+1:id+km)
             id = id + km
             q1_si(mnr  ,k+1:k+km) =  bufrec(id+1:id+km)
             id = id + km
             q2_si(mnr-1,k+1:k+km) =  bufrec(id+1:id+km)
             id = id + km
             q2_si(mnr  ,k+1:k+km) =  bufrec(id+1:id+km)
             id = id + km
             IF (PRESENT(q3)) THEN
                q3_si(mnr-1,k+1:k+km) =  bufrec(id+1:id+km)
                id = id + km
                q3_si(mnr  ,k+1:k+km) =  bufrec(id+1:id+km)
                id = id + km
             ENDIF
             IF (l.eq.1.and.PRESENT(qs1)) THEN
                qs1_si(mnr-1) =  bufrec(id+1)
                qs1_si(mnr) =  bufrec(id+2)
                id = id + 2
             ENDIF
             ipr(iproc) = id
          ENDIF
          k = k + km
       END DO
    END DO

    CALL MPI_WAITALL(nsend,requests(1),status,ierr)
    
  END SUBROUTINE SpectoSi

  SUBROUTINE SitoSpec(ibs,ibr,nsend,nrec,mysends,myrecs,maps,mapr,kmg,kml,&
                      nlfour,toall,q1,q2,q1_si,q2_si,q3,q3_si,qs1,qs1_si)
    !
    INTEGER, INTENT(IN) :: nsend
    INTEGER, INTENT(IN) :: nrec
    INTEGER, INTENT(IN) :: ibs(nsend+1)
    INTEGER, INTENT(IN) :: ibr(nrec+1)
    INTEGER, INTENT(IN) :: mysends(nsend)
    INTEGER, INTENT(IN) :: myrecs(nrec)
    INTEGER, INTENT(IN) :: maps(0:maxnodes-1)
    INTEGER, INTENT(IN) :: mapr(0:maxnodes-1)
    INTEGER, INTENT(IN) :: kml
    INTEGER, INTENT(IN) :: kmg
    INTEGER, INTENT(IN) :: nlfour(*)
    LOGICAL, INTENT(IN) :: toall
    REAL(KIND=r8)   , INTENT(OUT):: q1(2*mymnmax,kml)
    REAL(KIND=r8)   , INTENT(OUT):: q2(2*mymnmax,kml)
    REAL(KIND=r8)   , INTENT(IN) :: q1_si(2*mnmax_si,kmg)
    REAL(KIND=r8)   , INTENT(IN) :: q2_si(2*mnmax_si,kmg)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(OUT):: q3(2*mymnmax,kml)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(OUT):: qs1(2*mymnmax)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(IN) :: q3_si(2*mnmax_si,kmg)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(IN) :: qs1_si(2*mnmax_si)

    INTEGER :: i, mn, mnr, idest, l, m, km, ndim
    INTEGER :: iself, id, k, iproc, mng
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: ips(nsend)
    INTEGER :: ipr(nrec)
    INTEGER :: requestr(nrec)
    INTEGER :: requests(nsend)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    ndim = max(ibs(nsend+1),ibr(nrec+1))
    IF (dimrecbuf.lt.ndim) THEN
       dimrecbuf = ndim
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
 
    IF (dimsendbuf.lt.ndim) THEN
       dimsendbuf = ndim
       DEALLOCATE (bufsend)
       ALLOCATE (bufsend(dimsendbuf))
    ENDIF
    comm = MPI_COMM_WORLD

    DO i=1,nrec 
       idest = myrecs(i)
       IF (idest.ne.myid) THEN
          CALL MPI_IRECV(bufrec(ibr(i)),ibr(i+1)-ibr(i),MPI_DOUBLE_PRECISION,&
               idest,39,comm,requestr(i),ierr)
        ELSE
          requestr(i) = MPI_REQUEST_NULL
          iself = i
       ENDIF
    ENDDO

    ips = ibs(1:nsend)-1
    ipr = ibr(1:nrec)-1

    DO mn=1,mnmax_si
       mnr = 2 * mn
       mng = mymnmap_si(mn)
       m = mmap(mng)
       k = 0
       DO l=1,ngroups_four
          iproc = maps(map_four(l,nodehasM(m,l)))
          km = nlfour(l)
          id = ips(iproc)
          bufsend(id+1:id+km) = q1_si(mnr-1,k+1:k+km)
          id = id + km
          bufsend(id+1:id+km) = q1_si(mnr  ,k+1:k+km)
          id = id + km
          bufsend(id+1:id+km) = q2_si(mnr-1,k+1:k+km)
          id = id + km
          bufsend(id+1:id+km) = q2_si(mnr  ,k+1:k+km)
          id = id + km
          IF (PRESENT(q3)) THEN
             bufsend(id+1:id+km) = q3_si(mnr-1,k+1:k+km)
             id = id + km
             bufsend(id+1:id+km) = q3_si(mnr  ,k+1:k+km)
             id = id + km
          ENDIF
          IF ((l.eq.1.or.toall).and.PRESENT(qs1)) THEN
             bufsend(id+1) = qs1_si(mnr-1)
             bufsend(id+2) = qs1_si(mnr)
             id = id + 2
          ENDIF
          ips(iproc) = id
          k = k + km
       END DO
    END DO

    DO i=1,nsend
       idest = mysends(i)
       IF (idest.ne.myid) THEN
          CALL MPI_ISEND(bufsend(ibs(i)),ips(i)-ibs(i)+1,MPI_DOUBLE_PRECISION, &
               idest,39,comm,requests(i),ierr)
        ELSE
          requests(i) = MPI_REQUEST_NULL
          bufrec(ibr(iself):ibr(iself)+ips(i)-ibs(i)) = bufsend(ibs(i):ips(i))
       ENDIF
    END DO
    CALL MPI_WAITALL(nrec,requestr(1),status,ierr)

    DO mn=1,mymnmax
       idest = mapr(mnsendsmap_si(mn))
       mnr = 2 * mn
       id = ipr(idest)
       q1(mnr-1,:) = bufrec(id+1:id+kml)
       id =id+kml
       q1(mnr,:) = bufrec(id+1:id+kml)
       id =id+kml
       q2(mnr-1,:) = bufrec(id+1:id+kml)
       id =id+kml
       q2(mnr,:) = bufrec(id+1:id+kml)
       id =id+kml
       IF (PRESENT(q3)) THEN
          q3(mnr-1,:) = bufrec(id+1:id+kml)
          id =id+kml
          q3(mnr,:) = bufrec(id+1:id+kml)
          id =id+kml
       END IF
       IF ((havesurf.or.toall).and.PRESENT(qs1)) THEN
          qs1(mnr-1) = bufrec(id+1)
          qs1(mnr) = bufrec(id+2)
          id = id + 2
       END IF
       ipr(idest) = id
    END DO
    CALL MPI_WAITALL(nsend,requests(1),status,ierr)
    
  END SUBROUTINE SitoSpec

  SUBROUTINE Exchange_Hallos(nrec,nsend,nf1,nf2)
    !
    INTEGER, INTENT(IN) :: nrec
    INTEGER, INTENT(IN) :: nf1
    INTEGER, INTENT(IN) :: nf2
    INTEGER, INTENT(OUT) :: nsend

    INTEGER :: i, m, k, irec, nlen, isnd, nf
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: ns(0:MaxNodes-1)
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: requests(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    comm = MPI_COMM_WORLD
    requestr(myid) = MPI_REQUEST_NULL
    requests(myid) = MPI_REQUEST_NULL
    nf = max(nf1,nf2)
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
          CALL MPI_IRECV(mysendspr(2,i+1),1,MPI_INTEGER,i,15,&
                         comm,requestr(i),ierr)
        ELSE
          mysendspr(2,i+1) = 0
       ENDIF
    ENDDO
    m = 0
    k = 1
    DO i=0,MaxNodes-1
       IF (i.ne.myid) THEN
          IF (k.le.nrec.and.i.eq.myrecspr(1,k)) THEN
             ns(i) = myrecspr(2,k) - m
             m = myrecspr(2,k)
             k = k + 1
            ELSE
             ns(i) = 0
          ENDIF
          CALL MPI_ISEND(ns(i),1,MPI_INTEGER,i,15,comm,requests(i),ierr)
       ENDIF
    ENDDO
    CALL MPI_WAITALL(MaxNodes,requestr(0),status,ierr)
    CALL MPI_WAITALL(MaxNodes,requests(0),status,ierr)
    k = 0
    m = 0
    DO i=0,MaxNodes-1
       IF (mysendspr(2,i+1).ne.0) THEN
          k = k + 1
          m = m + mysendspr(2,i+1)
          mysendspr(2,k) = m
          mysendspr(1,k) = i
       ENDIF
    ENDDO
    nsend = k
    m = 0
    DO i=1,nsend
       ns(i) = mysendspr(2,i) - m
       CALL MPI_IRECV(mysendsgr(1,m+1),4*ns(i),MPI_INTEGER,mysendspr(1,i),16,&
                            comm,requestr(i),ierr)
       m = mysendspr(2,i)
    ENDDO
    ALLOCATE (isbrec (nrec+1))
    ALLOCATE (ilrecbuf(nrec))
    ALLOCATE (isbsend(nsend+1))
    ALLOCATE (ilsendbuf(nsend))
    m = 0
    irec = 1
    DO i=1,nrec
       isbrec(i) = irec 
       ns(i) = myrecspr(2,i) - m
       CALL MPI_ISEND(myrecsgr(1,m+1),ns(i)*4,MPI_INTEGER,myrecspr(1,i),16,&
                            comm,requests(i),ierr)
       nlen = 0
       do k=m+1,myrecspr(2,i)
          nlen = nlen + myrecsgr(2,k) - myrecsgr(1,k) + 1
       enddo
       ilrecbuf(i) = nlen*nf1
       irec = irec + nlen*nf
       m = myrecspr(2,i)
    ENDDO
    isbrec(nrec+1) = irec 
    CALL MPI_WAITALL(nsend,requestr(1),status,ierr)
    CALL MPI_WAITALL(nrec,requests(1),status,ierr)
    
    m = 0
    isnd = 1
    DO i=1,nsend
       isbsend(i) = isnd 
       nlen = 0
       do k=m+1,mysendspr(2,i)
          nlen = nlen + mysendsgr(2,k) - mysendsgr(1,k) + 1
       enddo
       ilsendbuf(i) = nlen*nf1
       isnd = isnd+ nlen*nf
       m = mysendspr(2,i)
    ENDDO
    isbsend(nsend+1) = isnd 
!   write(*,*) myid, 'nrec  ',nrec,' isbrec ',isbrec
!   write(*,*) myid, 'nrec  ',nrec,' ilrecbuf ', ilrecbuf
!   write(*,*) myid, 'nsend ',nsend,' isbsend',isbsend
!   write(*,*) myid, 'nsend ',nsend,' ilsendbuf ', ilsendbuf

  END SUBROUTINE Exchange_Hallos

  SUBROUTINE Exchange_Fields (u,v,t,q,lps,fgpass_scalar,adr,nscalars, &
                              nrec,nsend,slagr,ice,liq,var)
    !
    INTEGER, INTENT(IN)    :: nsend
    INTEGER, INTENT(IN)    :: nrec 
    INTEGER, INTENT(IN)    :: nscalars
    INTEGER, INTENT(IN)    :: adr
    LOGICAL, INTENT(IN)    :: slagr
    REAL(KIND=r8)   , INTENT(INOUT) :: u(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , INTENT(INOUT) :: v(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , INTENT(INOUT) :: t(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , INTENT(INOUT) :: q(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(INOUT) :: ice(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(INOUT) :: liq(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , OPTIONAL,   INTENT(INOUT) :: var(ibMax,kmax,jbMax_ext,nClass+nAeros)

    REAL(KIND=r8)   , INTENT(INOUT) :: &
                      fgpass_scalar(ibMax,kmax,jbMax_ext,nscalars,2)
    REAL(KIND=r8)   , INTENT(INOUT) :: lps(ibMax,jbMax_ext)
    INTEGER :: index
    INTEGER :: statu(MPI_STATUS_SIZE)
    INTEGER :: status(MPI_STATUS_SIZE,nsend)
    INTEGER :: requests(nsend)
    INTEGER :: requestr(nrec)

    INTEGER :: j, ns, ibr, ibs, jbr, k,kk
    INTEGER :: m, i, kr, ks, n
    INTEGER :: comm, ierr

    IF (dimrecbuf.lt.isbrec(nrec+1)) THEN
       dimrecbuf = isbrec(nrec+1)
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    IF (dimsendbuf.lt.isbsend(nsend+1)) THEN
       dimsendbuf = isbsend(nsend+1)
       DEALLOCATE (bufsend)
       ALLOCATE (bufsend(dimsendbuf))
    ENDIF
    comm = MPI_COMM_WORLD
    DO k=1,nrec
       CALL MPI_IRECV(bufrec(isbrec(k)),ilrecbuf(k),MPI_DOUBLE_PRECISION,&
                      myrecspr(1,k),75,comm,requestr(k),ierr)
    ENDDO
    m = 1
    DO k=1,nsend
       ibs = isbsend(k)-1
       IF (slagr) THEN
          do ns=m,mysendspr(2,k)
             j=mysendsgr(3,ns)
             do i=mysendsgr(1,ns),mysendsgr(2,ns)
                jbr = jbperij(i,j)
                ibr = ibperij(i,j)
                bufsend(ibs+1:ibs+kmax)=u(ibr,:,jbr)
                ibs = ibs + kmax
                bufsend(ibs+1:ibs+kmax)=v(ibr,:,jbr)
                ibs = ibs + kmax
                bufsend(ibs+1:ibs+kmax)=t(ibr,:,jbr)
                ibs = ibs + kmax
                bufsend(ibs+1:ibs+kmax)=q(ibr,:,jbr)
                IF (PRESENT(ice)) THEN
                   ibs = ibs + kmax
                   bufsend(ibs+1:ibs+kmax)=ice(ibr,:,jbr)
                   ibs = ibs + kmax
                   bufsend(ibs+1:ibs+kmax)=liq(ibr,:,jbr)
                   IF (PRESENT(var)) THEN
                      DO kk=1,nClass+nAeros
                         ibs = ibs + kmax
                         bufsend(ibs+1:ibs+kmax)=var(ibr,:,jbr,kk)
                      END DO
                   END IF
                ENDIF
                ibs = ibs + kmax + 1 
                bufsend(ibs) = lps(ibr,jbr)
                do n=1,nscalars
                   bufsend(ibs+1:ibs+kmax)=fgpass_scalar(ibr,:,jbr,n,adr)
                   ibs = ibs + kmax
                enddo
             enddo
          enddo
        ELSE
          do ns=m,mysendspr(2,k)
             j=mysendsgr(3,ns)
             do i=mysendsgr(1,ns),mysendsgr(2,ns)
                jbr = jbperij(i,j)
                ibr = ibperij(i,j)
                bufsend(ibs+1:ibs+kmax)=q(ibr,:,jbr)
                IF (PRESENT(ice)) THEN
                   ibs = ibs + kmax
                   bufsend(ibs+1:ibs+kmax)=ice(ibr,:,jbr)
                   ibs = ibs + kmax
                   bufsend(ibs+1:ibs+kmax)=liq(ibr,:,jbr)
                   IF (PRESENT(var)) THEN
                      DO kk=1,nClass+nAeros
                         ibs = ibs + kmax
                         bufsend(ibs+1:ibs+kmax)=var(ibr,:,jbr,kk)
                      END DO
                   END IF
                ENDIF
                ibs = ibs + kmax
                do n=1,nscalars
                   bufsend(ibs+1:ibs+kmax)=fgpass_scalar(ibr,:,jbr,n,adr)
                   ibs = ibs + kmax
                enddo
             enddo
          enddo
       ENDIF
       CALL MPI_ISEND(bufsend(isbsend(k)),ilsendbuf(k),MPI_DOUBLE_PRECISION, &
                      mysendspr(1,k),75,comm,requests(k),ierr)
       m = mysendspr(2,k)+1
    ENDDO
    DO k=1,nrec
       CALL MPI_WAITANY(nrec,requestr,index,statu,ierr)
       kr = statu(MPI_SOURCE)
       do j=1,nrec
          if(myrecspr(1,j).eq.kr) then
             ks = j
             exit
          endif
       enddo
       ibs = isbrec(ks)-1
       if(ks.eq.1) then 
          m = 1
         else
          m = myrecspr(2,ks-1)+1
       endif
       IF (slagr) THEN
          do ns=m,myrecspr(2,ks)
             j=myrecsgr(3,ns)
             do i=myrecsgr(1,ns),myrecsgr(2,ns)
                jbr = jbperij(i,j)
                ibr = ibperij(i,j)
                u(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                ibs = ibs + kmax
                v(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                ibs = ibs + kmax
                t(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                ibs = ibs + kmax
                q(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                IF (PRESENT(ice)) THEN
                   ibs = ibs + kmax
                   ice(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                   ibs = ibs + kmax
                   liq(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                   IF (PRESENT(var)) THEN
                      DO kk=1,nClass+nAeros
                         ibs = ibs + kmax
                         var(ibr,:,jbr,kk) = bufrec(ibs+1:ibs+kmax)
                      END DO 
                   END IF
                ENDIF
                ibs = ibs + kmax + 1 
                lps(ibr,jbr) = bufrec(ibs)
                do n=1,nscalars
                   fgpass_scalar(ibr,:,jbr,n,adr) = bufrec(ibs+1:ibs+kmax)
                   ibs = ibs + kmax
                enddo
             enddo
          enddo
        ELSE
          do ns=m,myrecspr(2,ks)
             j=myrecsgr(3,ns)
             do i=myrecsgr(1,ns),myrecsgr(2,ns)
                jbr = jbperij(i,j)
                ibr = ibperij(i,j)
                q(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                IF (PRESENT(ice)) THEN
                   ibs = ibs + kmax
                   ice(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                   ibs = ibs + kmax
                   liq(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
                   IF (PRESENT(var)) THEN
                      DO kk=1,nClass+nAeros
                         ibs = ibs + kmax
                         var(ibr,:,jbr,kk) = bufrec(ibs+1:ibs+kmax)
                      END DO 
                   END IF
                ENDIF
                ibs = ibs + kmax
                do n=1,nscalars
                   fgpass_scalar(ibr,:,jbr,n,adr) = bufrec(ibs+1:ibs+kmax)
                   ibs = ibs + kmax
                enddo
             enddo
          enddo
       ENDIF
    ENDDO
    CALL MPI_WAITALL(nsend,requests,status,ierr)

  END SUBROUTINE Exchange_Fields



  SUBROUTINE Exchange_Winds (u,v,w,um,vm,nrec,nsend)
    !
    INTEGER, INTENT(IN)    :: nsend
    INTEGER, INTENT(IN)    :: nrec 
    REAL(KIND=r8)   , INTENT(INOUT) :: u(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , INTENT(INOUT) :: v(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , INTENT(INOUT) :: w(ibMax,kmax,jbMax_ext)
    REAL(KIND=r8)   , INTENT(INOUT) :: um(ibMax,jbMax_ext)
    REAL(KIND=r8)   , INTENT(INOUT) :: vm(ibMax,jbMax_ext)
    INTEGER :: index
    INTEGER :: statu(MPI_STATUS_SIZE)
    INTEGER :: status(MPI_STATUS_SIZE,nsend)
    INTEGER :: requests(nsend)
    INTEGER :: requestr(nrec)

    INTEGER :: j, ns, ibr, ibs, jbr, k
    INTEGER :: m, i, kr, ks
    INTEGER :: comm, ierr

    comm = MPI_COMM_WORLD
    IF (dimrecbuf.lt.isbrec(nrec+1)) THEN
       dimrecbuf = isbrec(nrec+1)
       DEALLOCATE (bufrec)
       ALLOCATE (bufrec(dimrecbuf))
    ENDIF
    IF (dimsendbuf.lt.isbsend(nsend+1)) THEN
       dimsendbuf = isbsend(nsend+1)
       DEALLOCATE (bufsend)
       ALLOCATE (bufsend(dimsendbuf))
    ENDIF

    DO k=1,nrec
       ibr = isbrec(k+1)-isbrec(k)
       CALL MPI_IRECV(bufrec(isbrec(k)),ibr,MPI_DOUBLE_PRECISION,&
                      myrecspr(1,k),76,comm,requestr(k),ierr)
    ENDDO
    m = 1
    DO k=1,nsend
       ibs = isbsend(k)-1
       DO ns=m,mysendspr(2,k)
          j=mysendsgr(3,ns)
          DO i=mysendsgr(1,ns),mysendsgr(2,ns)
             jbr = jbperij(i,j)
             ibr = ibperij(i,j)
             bufsend(ibs+1:ibs+kmax)=u(ibr,:,jbr)
             ibs = ibs + kmax
             bufsend(ibs+1:ibs+kmax)=v(ibr,:,jbr)
             ibs = ibs + kmax
             bufsend(ibs+1:ibs+kmax)=w(ibr,:,jbr)
             ibs = ibs + kmax + 1 
             bufsend(ibs) = um(ibr,jbr)
             ibs = ibs + 1 
             bufsend(ibs) = vm(ibr,jbr)
          ENDDO
       ENDDO

       CALL MPI_ISEND(bufsend(isbsend(k)),ibs-isbsend(k)+1,MPI_DOUBLE_PRECISION, &
                      mysendspr(1,k),76,comm,requests(k),ierr)
       m = mysendspr(2,k)+1
!      write(0,*) myid, ' sends ',ibs-isbsend(k)+1,'to ',mysendspr(1,k)
    ENDDO
    DO k=1,nrec
       CALL MPI_WAITANY(nrec,requestr,index,statu,ierr)
       kr = statu(MPI_SOURCE)

       do j=1,nrec
          if(myrecspr(1,j).eq.kr) then
             ks = j
             exit
          endif
       enddo
       ibs = isbrec(ks)-1
       if(ks.eq.1) then 
          m = 1
         else
          m = myrecspr(2,ks-1)+1
       endif
       do ns=m,myrecspr(2,ks)
          j=myrecsgr(3,ns)
          do i=myrecsgr(1,ns),myrecsgr(2,ns)
             jbr = jbperij(i,j)
             ibr = ibperij(i,j)
             u(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
             ibs = ibs + kmax
             v(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
             ibs = ibs + kmax
             w(ibr,:,jbr) = bufrec(ibs+1:ibs+kmax)
             ibs = ibs + kmax + 1 
             um(ibr,jbr) = bufrec(ibs)
             ibs = ibs + 1 
             vm(ibr,jbr) = bufrec(ibs)
          enddo
       enddo
!      write(0,*) myid, ' rec ',ibs-isbrec(ks), ' from ',ks
    ENDDO
    CALL MPI_WAITALL(nsend,requests,status,ierr)

  END SUBROUTINE Exchange_Winds

END MODULE Communications
