!
!  $Author: panetta $
!  $Date: 2007/08/12 13:52:18 $
!  $Revision: 1.5 $
!
MODULE Communications

  USE Parallelism, ONLY: &
       myId,             &
       maxNodes

  USE Sizes, ONLY:     &
       mymmax,         &
       mymnmax,        &
       mymnextmax,     &
       kmax,           &
       mmax,           &
       mnmax,          &
       mnextmax,       &
       mnmaxlocal,     &
       mnextmaxlocal,  &
       ibmax,          &
       jbmax,          &
       ijmax,          &
       imax,           &
       jmax,           &
       myjmax,         &
       ibmaxperjb,     &
       imaxperj,       &
       jbperij,        &
       ibperij,        &
       Msperproc,      &
       Msinproc,       &
       mnmaxinproc,    &
       mnextmaxinproc, &
       NodehasM,       &
       myfirstlat,     &
       mylastlat,      &
       nlatsinproc,    &
       firstlatinproc, &
       lastlatinproc,  &
       pointsinproc
   
  USE Options, ONLY:   &
       slagr

  USE Constants, ONLY:  r8

  IMPLICIT NONE

  INCLUDE 'mpif.h'

  PRIVATE

  PUBLIC :: Collect_Grid_Red
  PUBLIC :: Collect_Grid_Sur
  PUBLIC :: Collect_Grid_His
  PUBLIC :: Collect_Grid_Full
  PUBLIC :: Collect_Spec
  PUBLIC :: Collect_Spec_Ext
  PUBLIC :: Exchange_Fields
  PUBLIC :: Exchange_Winds
  PUBLIC :: Complete_Exchange

CONTAINS

  SUBROUTINE Collect_Grid_Red(field, fieldglob, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    REAL(KIND=r8)   , INTENT(IN) :: field(ibMax,jbMax)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(ijmax)

    INTEGER :: ij, j, i, ii
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE)

    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       ij=1
       DO j=1,jbMax
          DO i=1,ibMaxPerJB(j)
             fieldglob(ij)=field(i,j)
             ij=ij+1
          END DO
       END DO
       CALL MPI_ISEND(fieldglob,ij-1,MPI_DOUBLE_PRECISION,nproc,91,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(nproc) = MPI_REQUEST_NULL
       ij=1
       DO ii=0,MaxNodes-1
          IF (ii.ne.nproc) THEN
             CALL MPI_IRECV(fieldglob(ij),pointsinproc(ii),MPI_DOUBLE_PRECISION,ii,91,&
                            comm,requestr(ii),ierr)
             ij = ij + pointsinproc(ii)
            ELSE
             DO j=1,jbMax
                DO i=1,ibMaxPerJB(j)
                   fieldglob(ij)=field(i,j)
                   ij=ij+1
                END DO
             END DO
          ENDIF
       ENDDO
       DO i=1,MaxNodes-1
          CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
       ENDDO
    END IF

  END SUBROUTINE Collect_Grid_Red


  SUBROUTINE Collect_Grid_His(field, fieldglob, ngpts, ngptslocal, nproc, nf, &
                              ngptsperproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: nf
    INTEGER, INTENT(IN) :: ngpts
    INTEGER, INTENT(IN) :: ngptslocal
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
    REAL(KIND=r8)    :: work(ngpts*nf)

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
             CALL MPI_IRECV(work(ij),ngptsperproc(i)*nf,MPI_DOUBLE_PRECISION,i,92,&
                            comm,requestr(i),ierr)
          ENDIF
          ini(i+1) = ini(i) + ngptsperproc(i)
          ij = ij + ngptsperproc(i)*nf
       ENDDO
       i1 = ini(nproc)+1
       i2 = ini(nproc+1)
       fieldglob(i1:i2,:) = field(1:ngptslocal,:)
       DO i=1,MaxNodes-1
          CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
          ij = status(MPI_SOURCE)
          i1 = ini(ij)*nf
          !CDIR NODEP
          DO n=1,nf
             fieldglob(ini(ij)+1:ini(ij+1),n) = work(i1+1:i1+ngptsperproc(ij))
             i1 = i1 + ngptsperproc(ij)
          ENDDO
       ENDDO
    END IF

  END SUBROUTINE Collect_Grid_His


  SUBROUTINE Collect_Grid_Sur(field, fieldglob, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    REAL(KIND=r8)   , INTENT(IN) :: field(imax,myjMax)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(imax,jmax)

    INTEGER :: ij, i
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE,maxnodes)

    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       CALL MPI_ISEND(field,imax*myjmax,MPI_DOUBLE_PRECISION,nproc,93,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(nproc) = MPI_REQUEST_NULL
       fieldglob(:,myfirstlat:mylastlat) = field(:,:)
       ij=1
       DO i=0,MaxNodes-1
          IF (i.ne.nproc) THEN
             CALL MPI_IRECV(fieldglob(1,ij),nlatsinproc(i)*imax,MPI_DOUBLE_PRECISION,i,93,&
                            comm,requestr(i),ierr)
          ENDIF
          ij = ij + nlatsinproc(i)
       ENDDO
       CALL MPI_WAITALL(MaxNodes,requestr(0),status,ierr)
    END IF

  END SUBROUTINE Collect_Grid_Sur


  SUBROUTINE Collect_Grid_Full(field, fieldglob, levs, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: levs
    REAL(KIND=r8)   , INTENT(IN) :: field(imax,myjMax,levs)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(imax,jmax,levs)

    REAL(KIND=r8)    :: fcom(imax*jmax*levs)
    INTEGER :: ij, j, i, ip, k
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: ini(0:MaxNodes)
    INTEGER :: status(MPI_STATUS_SIZE)

    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       CALL MPI_ISEND(field,imax*myjmax*levs,MPI_DOUBLE_PRECISION,nproc,94,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(nproc) = MPI_REQUEST_NULL
       fieldglob(:,myfirstlat:mylastlat,:) = field(:,:,:)
       ij=1
       DO i=0,MaxNodes-1
          ini(i) = ij
          IF (i.ne.nproc) THEN
             CALL MPI_IRECV(fcom(ij),nlatsinproc(i)*imax*levs,MPI_DOUBLE_PRECISION,i,94,&
                            comm,requestr(i),ierr)
          ENDIF
          ij = ij + nlatsinproc(i)*imax*levs
       ENDDO
       ini(maxnodes) = ij
       DO i=1,MaxNodes-1
          CALL MPI_WAITANY(MaxNodes,requestr(0),index,status,ierr)
          ij = status(MPI_SOURCE)
          ip = ini(ij) - 1
          DO k=1,levs
             !CDIR NODEP
             DO j=firstlatinproc(ij),lastlatinproc(ij)
                fieldglob(1:imax,j,k)= fcom(ip+1:ip+imax)
                ip = ip + imax 
             ENDDO
          ENDDO
       ENDDO
    END IF

  END SUBROUTINE Collect_Grid_Full



  SUBROUTINE Collect_Spec(field, fieldglob, levs, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: levs
    REAL(KIND=r8)   , INTENT(IN) :: field(2*mymnmax,levs)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(2*mnmax,levs)

    REAL(KIND=r8)    :: fcom(2*mnmaxlocal*levs,0:maxnodes-1)
    INTEGER :: j, i, m, mn, mnloc, ns, l, lev
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE)

    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       CALL MPI_ISEND(field,2*levs*mymnmax,MPI_DOUBLE_PRECISION,nproc,95,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(nproc) = MPI_REQUEST_NULL
       DO i=0,MaxNodes-1
          IF (i.ne.nproc) THEN
             CALL MPI_IRECV(fcom(1,i),2*mnmaxinproc(i)*levs,MPI_DOUBLE_PRECISION,i,95,&
                            comm,requestr(i),ierr)
            ELSE
             mnloc=0
             mn=0
             DO m=1,Mmax
                ns=2*(Mmax-m+1)
                IF(NodeHasM(m).eq.nproc) THEN
                   DO l=1,ns
                      fieldglob(mn+l,:) = field(mnloc+l,:)
                   ENDDO
                   mnloc = mnloc+ns
                ENDIF
                mn = mn + ns
             ENDDO
          ENDIF
       ENDDO
       DO i=1,MaxNodes-1
          CALL MPI_WAITANY(MaxNodes,requestr,index,status,ierr)
          j = status(MPI_SOURCE)
          DO lev=1,levs
             mnloc = 2*mnmaxinproc(j)*(lev-1)
             mn=0
             DO m=1,Mmax
                ns=2*(Mmax-m+1)
                IF(NodeHasM(m).eq.j) THEN
                   DO l=1,ns
                      fieldglob(mn+l,lev) = fcom(mnloc+l,j)
                   ENDDO
                   mnloc = mnloc+ns
                ENDIF
                mn = mn + ns
             ENDDO
          ENDDO
       ENDDO

    END IF
  END SUBROUTINE Collect_Spec



  SUBROUTINE Collect_Spec_Ext(field, fieldglob, levs, nproc)
    !
    INTEGER, INTENT(IN) :: nproc ! destination processor
    INTEGER, INTENT(IN) :: levs
    REAL(KIND=r8)   , INTENT(IN) :: field(2*mymnextmax,levs)
    REAL(KIND=r8)   , INTENT(OUT):: fieldglob(2*mnextmax,levs)

    REAL(KIND=r8)    :: fcom(2*mnextmaxlocal*levs,0:maxnodes-1)
    INTEGER :: j, i, m, mn, mnloc, ns, l, lev
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: index
    INTEGER :: request
    INTEGER :: requestr(0:MaxNodes-1)
    INTEGER :: status(MPI_STATUS_SIZE)

    comm = MPI_COMM_WORLD
    IF (myid.ne.nproc) THEN
       CALL MPI_ISEND(field,2*levs*mymnextmax,MPI_DOUBLE_PRECISION,nproc,95,comm,request,ierr)
       CALL MPI_WAIT(request,status,ierr)
      ELSE
       requestr(nproc) = MPI_REQUEST_NULL
       DO i=0,MaxNodes-1
          IF (i.ne.nproc) THEN
             CALL MPI_IRECV(fcom(1,i),2*mnextmaxinproc(i)*levs,MPI_DOUBLE_PRECISION,i,95,&
                            comm,requestr(i),ierr)
            ELSE
             mnloc=0
             mn=0
             DO m=1,Mmax
                ns=2*(Mmax-m+2)
                IF(NodeHasM(m).eq.nproc) THEN
                   DO l=1,ns
                      fieldglob(mn+l,:) = field(mnloc+l,:)
                   ENDDO
                   mnloc = mnloc+ns
                ENDIF
                mn = mn + ns
             ENDDO
          ENDIF
       ENDDO
       DO i=1,MaxNodes-1
          CALL MPI_WAITANY(MaxNodes,requestr,index,status,ierr)
          j = status(MPI_SOURCE)
          DO lev=1,levs
             mnloc = 2*mnextmaxinproc(j)*(lev-1)
             mn=0
             DO m=1,Mmax
                ns=2*(Mmax-m+2)
                IF(NodeHasM(m).eq.j) THEN
                   DO l=1,ns
                      fieldglob(mn+l,lev) = fcom(mnloc+l,j)
                   ENDDO
                   mnloc = mnloc+ns
                ENDIF
                mn = mn + ns
             ENDDO
          ENDDO
       ENDDO

    END IF
  END SUBROUTINE Collect_Spec_Ext

  SUBROUTINE Exchange_Fields (u,v,t,q,lps,fgpass_scalar,nscalars,iold, &
                              jbminus,jbplus,jovlap,requests,requestr)
    !
    INTEGER, INTENT(IN)    :: jbminus
    INTEGER, INTENT(IN)    :: jbplus
    INTEGER, INTENT(IN)    :: jovlap
    INTEGER, INTENT(IN)    :: nscalars
    INTEGER, INTENT(IN)    :: iold
    REAL(KIND=r8)   , INTENT(INOUT) :: u(ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: v(ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: t(ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: q(ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: &
                      fgpass_scalar(ibmax,kmax,jbminus:jbplus,nscalars,3)
    REAL(KIND=r8)   , INTENT(INOUT) :: lps(ibmax,jbminus:jbplus)
    INTEGER, INTENT(INOUT) :: requests((10+2*nscalars)*jovlap)
    INTEGER, INTENT(INOUT) :: requestr((10+2*nscalars)*jovlap)

    INTEGER :: j, jr, js, ns, ibr, ibs, jbr, jbs, mnums, mnumr
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: itypess, isizess
    INTEGER :: itypers, isizers
    INTEGER :: itypesn, isizesn
    INTEGER :: itypern, isizern

    comm = MPI_COMM_WORLD
    requestr = MPI_REQUEST_NULL
    requests = MPI_REQUEST_NULL
    jr = 1
    js = 1
    isizers = 0
    isizern = 0
    isizess = 0
    isizesn = 0
    mnums = 600
    mnumr = 600
    DO j=1,jovlap
       IF (myid.ne.0) THEN
          jbr = jbperij(1,myfirstlat-j)
          ibr = ibperij(1,myfirstlat-j)
          IF (isizern.ne.imaxperj(myfirstlat-j)) THEN
             isizern = imaxperj(myfirstlat-j)
             CALL MPI_TYPE_VECTOR(kmax,isizern,ibmax,MPI_DOUBLE_PRECISION,itypern,ierr)
             CALL MPI_TYPE_COMMIT(itypern,ierr)
          ENDIF
          CALL MPI_IRECV(u(ibr,1,jbr),1,itypern      ,myid-1,mnumr ,comm,&
                         requestr(jr),ierr)
          CALL MPI_IRECV(v(ibr,1,jbr),1,itypern      ,myid-1,mnumr+1,comm,&
                         requestr(jr+1),ierr)
          CALL MPI_IRECV(t(ibr,1,jbr),1,itypern      ,myid-1,mnumr+2,comm,&
                         requestr(jr+2),ierr)
          CALL MPI_IRECV(q(ibr,1,jbr),1,itypern      ,myid-1,mnumr+3,comm,&
                         requestr(jr+3),ierr)
          CALL MPI_IRECV(lps(ibr,jbr),isizern,MPI_DOUBLE_PRECISION,myid-1,mnumr+4, &
                         comm,requestr(jr+4),ierr)
          DO ns=1,nscalars
             CALL MPI_IRECV(fgpass_scalar(ibr,1,jbr,ns,iold),1,itypern,myid-1, &
                            mnumr+4+ns,comm,requestr(jr+4+ns),ierr)
          ENDDO
       ENDIF
       mnumr = mnumr + 5 + nscalars
       jr = jr + 5 + nscalars
       IF (myid.ne.maxnodes-1) THEN
          jbr = jbperij(1,mylastlat+j)
          ibr = ibperij(1,mylastlat+j)
          IF (isizers.ne.imaxperj(mylastlat+j)) THEN
             isizers = imaxperj(mylastlat+j)
             CALL MPI_TYPE_VECTOR(kmax,isizers,ibmax,MPI_DOUBLE_PRECISION,itypers,ierr)
             CALL MPI_TYPE_COMMIT(itypers,ierr)
          ENDIF
          CALL MPI_IRECV(u(ibr,1,jbr),1,itypers   ,myid+1,mnumr,comm,&
                         requestr(jr),ierr)
          CALL MPI_IRECV(v(ibr,1,jbr),1,itypers   ,myid+1,mnumr+1,comm,&
                         requestr(jr+1),ierr)
          CALL MPI_IRECV(t(ibr,1,jbr),1,itypers   ,myid+1,mnumr+2,comm,&
                         requestr(jr+2),ierr)
          CALL MPI_IRECV(q(ibr,1,jbr),1,itypers   ,myid+1,mnumr+3,comm,&
                         requestr(jr+3),ierr)
          CALL MPI_IRECV(lps(ibr,jbr),isizers,MPI_DOUBLE_PRECISION,myid+1,mnumr+4,comm,&
                         requestr(jr+4),ierr)
          DO ns=1,nscalars
             CALL MPI_IRECV(fgpass_scalar(ibr,1,jbr,ns,iold),1,itypers,myid+1, &
                            mnumr+4+ns,comm,requestr(jr+4+ns),ierr)
          ENDDO
          jbs = jbperij(1,mylastlat+1-j)
          ibs = ibperij(1,mylastlat+1-j)
          IF (isizess.ne.imaxperj(mylastlat+1-j)) THEN
             isizess = imaxperj(mylastlat+1-j)
             CALL MPI_TYPE_VECTOR(kmax,isizess,ibmax,MPI_DOUBLE_PRECISION,itypess,ierr)
             CALL MPI_TYPE_COMMIT(itypess,ierr)
          ENDIF
          CALL MPI_ISEND(u(ibs,1,jbs),1,itypess,    myid+1,mnums,comm,&
                         requests(js),ierr)
          CALL MPI_ISEND(v(ibs,1,jbs),1,itypess,    myid+1,mnums+1,comm,&
                         requests(js+1),ierr)
          CALL MPI_ISEND(t(ibs,1,jbs),1,itypess,    myid+1,mnums+2,comm,&
                         requests(js+2),ierr)
          CALL MPI_ISEND(q(ibs,1,jbs),1,itypess,    myid+1,mnums+3,comm,&
                         requests(js+3),ierr)
          CALL MPI_ISEND(lps(ibs,jbs),isizess,MPI_DOUBLE_PRECISION,myid+1,mnums+4,comm,&
                         requests(js+4),ierr)
          DO ns=1,nscalars
             CALL MPI_ISEND(fgpass_scalar(ibs,1,jbs,ns,iold),1,itypess,myid+1, &
                            mnums+4+ns,comm,requests(js+4+ns),ierr)
          ENDDO
       ENDIF
       mnumr = mnumr + 5 + nscalars
       jr = jr + 5 + nscalars
       mnums = mnums + 5 + nscalars
       js = js + 5 + nscalars
       IF (myid.ne.0) THEN
          jbs = jbperij(1,myfirstlat-1+j)
          ibs = ibperij(1,myfirstlat-1+j)
          IF (isizesn.ne.imaxperj(myfirstlat-1+j)) THEN
             isizesn = imaxperj(myfirstlat-1+j)
             CALL MPI_TYPE_VECTOR(kmax,isizesn,ibmax,MPI_DOUBLE_PRECISION,itypesn,ierr)
             CALL MPI_TYPE_COMMIT(itypesn,ierr)
          ENDIF
          CALL MPI_ISEND(u(ibs,1,jbs),1,itypesn      ,myid-1,mnums,comm,&
                         requests(js),ierr)
          CALL MPI_ISEND(v(ibs,1,jbs),1,itypesn      ,myid-1,mnums+1,comm,&
                         requests(js+1),ierr)
          CALL MPI_ISEND(t(ibs,1,jbs),1,itypesn      ,myid-1,mnums+2,comm,&
                         requests(js+2),ierr)
          CALL MPI_ISEND(q(ibs,1,jbs),1,itypesn      ,myid-1,mnums+3,comm,&
                         requests(js+3),ierr)
          CALL MPI_ISEND(lps(ibs,jbs),isizesn,MPI_DOUBLE_PRECISION,myid-1,mnums+4,comm,&
                         requests(js+4),ierr)
          DO ns=1,nscalars
             CALL MPI_ISEND(fgpass_scalar(ibs,1,jbs,ns,iold),1,itypesn,myid-1, &
                            mnums+4+ns,comm,requests(js+4+ns),ierr)
          ENDDO
       ENDIF
       mnums = mnums + 5 + nscalars
       js = js + 5 + nscalars
    ENDDO
    IF (isizess.ne.0) CALL MPI_TYPE_FREE(itypess,ierr)
    IF (isizesn.ne.0) CALL MPI_TYPE_FREE(itypesn,ierr)
    IF (isizers.ne.0) CALL MPI_TYPE_FREE(itypers,ierr)
    IF (isizern.ne.0) CALL MPI_TYPE_FREE(itypern,ierr)

  END SUBROUTINE Exchange_Fields

  SUBROUTINE Exchange_Winds (u,v,w,um,vm,jbminus,jbplus,jovlap,requests,requestr)
    !
    INTEGER, INTENT(IN)    :: jbminus
    INTEGER, INTENT(IN)    :: jbplus
    INTEGER, INTENT(IN)    :: jovlap
    REAL(KIND=r8)   , INTENT(INOUT) :: u(ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: v(ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: w(ibmax,kmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: um(ibmax,jbminus:jbplus)
    REAL(KIND=r8)   , INTENT(INOUT) :: vm(ibmax,jbminus:jbplus)
    INTEGER, INTENT(INOUT) :: requests(10*jovlap)
    INTEGER, INTENT(INOUT) :: requestr(10*jovlap)

    INTEGER :: j, jr, ibr, ibs, jbr, jbs
    INTEGER :: comm
    INTEGER :: ierr
    INTEGER :: itypess, isizess
    INTEGER :: itypers, isizers
    INTEGER :: itypesn, isizesn
    INTEGER :: itypern, isizern

    comm = MPI_COMM_WORLD
    requestr = MPI_REQUEST_NULL
    requests = MPI_REQUEST_NULL
    jr = 1
    isizers = 0
    isizern = 0
    isizess = 0
    isizesn = 0
    DO j=1,jovlap
       IF (myid.ne.0) THEN
          jbr = jbperij(1,myfirstlat-j)
          ibr = ibperij(1,myfirstlat-j)
          IF (isizern.ne.imaxperj(myfirstlat-j)) THEN
             isizern = imaxperj(myfirstlat-j)
             CALL MPI_TYPE_VECTOR(kmax,isizern,ibmax,MPI_DOUBLE_PRECISION,itypern,ierr)
             CALL MPI_TYPE_COMMIT(itypern,ierr)
          ENDIF
          CALL MPI_IRECV(u(ibr,1,jbr),1,itypern      ,myid-1,205+j,comm,&
                         requestr(jr),ierr)
          CALL MPI_IRECV(v(ibr,1,jbr),1,itypern      ,myid-1,215+j,comm,&
                         requestr(jr+1),ierr)
          CALL MPI_IRECV(w(ibr,1,jbr),1,itypern      ,myid-1,225+j,comm,&
                         requestr(jr+2),ierr)
          CALL MPI_IRECV(um(ibr,jbr),isizern,MPI_DOUBLE_PRECISION,myid-1,235+j,comm,&
                         requestr(jr+3),ierr)
          CALL MPI_IRECV(vm(ibr,jbr),isizern,MPI_DOUBLE_PRECISION,myid-1,245+j,comm,&
                         requestr(jr+4),ierr)
       ENDIF
       IF (myid.ne.maxnodes-1) THEN
          jbr = jbperij(1,mylastlat+j)
          ibr = ibperij(1,mylastlat+j)
          IF (isizers.ne.imaxperj(mylastlat+j)) THEN
             isizers = imaxperj(mylastlat+j)
             CALL MPI_TYPE_VECTOR(kmax,isizers,ibmax,MPI_DOUBLE_PRECISION,itypers,ierr)
             CALL MPI_TYPE_COMMIT(itypers,ierr)
          ENDIF
          CALL MPI_IRECV(u(ibr,1,jbr),1,itypers   ,myid+1,255+j,comm,&
                         requestr(jr+5),ierr)
          CALL MPI_IRECV(v(ibr,1,jbr),1,itypers   ,myid+1,265+j,comm,&
                         requestr(jr+6),ierr)
          CALL MPI_IRECV(w(ibr,1,jbr),1,itypers   ,myid+1,275+j,comm,&
                         requestr(jr+7),ierr)
          CALL MPI_IRECV(um(ibr,jbr),isizers,MPI_DOUBLE_PRECISION,myid+1,285+j,comm,&
                         requestr(jr+8),ierr)
          CALL MPI_IRECV(vm(ibr,jbr),isizers,MPI_DOUBLE_PRECISION,myid+1,295+j,comm,&
                         requestr(jr+9),ierr)
          jbs = jbperij(1,mylastlat+1-j)
          ibs = ibperij(1,mylastlat+1-j)
          IF (isizess.ne.imaxperj(mylastlat+1-j)) THEN
             isizess = imaxperj(mylastlat+1-j)
             CALL MPI_TYPE_VECTOR(kmax,isizess,ibmax,MPI_DOUBLE_PRECISION,itypess,ierr)
             CALL MPI_TYPE_COMMIT(itypess,ierr)
          ENDIF
          CALL MPI_ISEND(u(ibs,1,jbs),1,itypess,    myid+1,205+j,comm,&
                         requests(jr),ierr)
          CALL MPI_ISEND(v(ibs,1,jbs),1,itypess,    myid+1,215+j,comm,&
                         requests(jr+1),ierr)
          CALL MPI_ISEND(w(ibs,1,jbs),1,itypess,    myid+1,225+j,comm,&
                         requests(jr+2),ierr)
          CALL MPI_ISEND(um(ibs,jbs),isizess,MPI_DOUBLE_PRECISION,myid+1,235+j,comm,&
                         requests(jr+3),ierr)
          CALL MPI_ISEND(vm(ibs,jbs),isizess,MPI_DOUBLE_PRECISION,myid+1,245+j,comm,&
                         requests(jr+4),ierr)
       ENDIF
       IF (myid.ne.0) THEN
          jbs = jbperij(1,myfirstlat-1+j)
          ibs = ibperij(1,myfirstlat-1+j)
          IF (isizesn.ne.imaxperj(myfirstlat-1+j)) THEN
             isizesn = imaxperj(myfirstlat-1+j)
             CALL MPI_TYPE_VECTOR(kmax,isizesn,ibmax,MPI_DOUBLE_PRECISION,itypesn,ierr)
             CALL MPI_TYPE_COMMIT(itypesn,ierr)
          ENDIF
          CALL MPI_ISEND(u(ibs,1,jbs),1,itypesn      ,myid-1,255+j,comm,&
                         requests(jr+5),ierr)
          CALL MPI_ISEND(v(ibs,1,jbs),1,itypesn      ,myid-1,265+j,comm,&
                         requests(jr+6),ierr)
          CALL MPI_ISEND(w(ibs,1,jbs),1,itypesn      ,myid-1,275+j,comm,&
                         requests(jr+7),ierr)
          CALL MPI_ISEND(um(ibs,jbs),isizesn,MPI_DOUBLE_PRECISION,myid-1,285+j,comm,&
                         requests(jr+8),ierr)
          CALL MPI_ISEND(vm(ibs,jbs),isizesn,MPI_DOUBLE_PRECISION,myid-1,295+j,comm,&
                         requests(jr+9),ierr)
       ENDIF
       jr = jr + 10
    ENDDO
    IF (isizess.ne.0) CALL MPI_TYPE_FREE(itypess,ierr)
    IF (isizesn.ne.0) CALL MPI_TYPE_FREE(itypesn,ierr)
    IF (isizers.ne.0) CALL MPI_TYPE_FREE(itypers,ierr)
    IF (isizern.ne.0) CALL MPI_TYPE_FREE(itypern,ierr)


  END SUBROUTINE Exchange_Winds

  SUBROUTINE Complete_Exchange(requests,requestr,isize)
    !
    INTEGER, INTENT(INOUT) :: requests(isize)
    INTEGER, INTENT(INOUT) :: requestr(isize)
    INTEGER, INTENT(IN   ) :: isize

    INTEGER :: status(MPI_STATUS_SIZE,isize)
    INTEGER :: ierr

    CALL MPI_WAITALL(isize,requestr,status,ierr)
    CALL MPI_WAITALL(isize,requests,status,ierr)

  END SUBROUTINE Complete_Exchange


END MODULE Communications
