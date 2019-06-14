      SUBROUTINE RG(NM,N,A,WR,WI,MATZ,Z,IERR,EPS,SCALE,ORT)
C*
C*    ** RG calculates the eigenvalues and/or eigenvectors
C*          of a real general matrix.
C*
C*    ** Arguments:
C*
C*    ** NM - row dimension of matrix A at the calling routine: Input
C*            Integer variable.
C*
C*    ** N - current dimension of matrix A: Input
C*            Integer variable; must be .LE. NM.
C*
C*    ** A - real matrix (destroyed): Input
C*           Real array with dimensions A(NM,N).
C*
C*    ** WR - real part of the eigenvalues: Output
C*            Real vector with dimensions WR(N).
C*
C*    ** WI - imaginary part of the eigenvalues: Output
C*            Real vector with dimensions WI(N).
C*
C*            OBS: There is nor ordenation for the eigenvalues, except
C*                 that for the conjuate complex pairs are put together
C*                 and the pair with real positive imaginary part comes
C*                 in first place.
C*
C*    ** MATZ - integer variable: Input
C*              Meaning:
C*    ** MATZ = 0 - only eigenvalues non-filtered
C*    ** MATZ = 1 - eigenvalues and eigenvectors normalized and filtered
C*    ** MATZ = 2 - eigenvalues and eigenvectors non-norm. and non-filt.
C*    ** MATZ = 3 - eigenvalues and eigenvectors normalized, non-filt.
C*    **            and without zeroes for .LE. TOLx (see ZNORMA).
C*
C*    ** Z - Eigenvectors: real and imaginary parts, so that:
C*           a) for a real J-th eigenvalue WR(J).NE.0.AND.WI(J).EQ.0,
C*              the J-th eigenvector is (Z(I,J),I=1,N);
C*           b) for a imaginary J-th eigenvalue with WI(J).NE.0,
C*              the (J+1)-th eigenvalue is its conjugate complex;
C*              the J-th eigenvector has real part (Z(I,J),I=1,N) and
C*              imaginary part (Z(I,J+1),I=1,N), and the (J+1)-th
C*              eigenvector has real part (Z(I,J),I=1,N) and
C*              imaginary part (-Z(I,J+1),I=1,N).
C*           Real array with dimensions Z(NM,N): Output
C*
C*    ** IERR - is a integer variable: Output, indicating:
C*              - if N .GT. NM, then the routine RG stop calculations
C*                and returns with IERR=10*N;
C*              - if 30 iteractions is exceeded for the J-th eigenvalue
C*                computation, then the routine RG stop calculations
C*                and returns with IERR=J and the J+1, J+2, ..., N
C*                eigenvalues are computed, but none eigenvector is
C*                computed;
C*              - for a normal termination IERR is set zero.
C*
C*    ** EPS - is a machine dependent parameter specifying the
C*             relative precision of the floating point arithmetic.
C*             It must be recomputed for the specific machine in use.
C*             It is 2**(-20) for 32 bitsand 2**(-50) for 64 bits.
C*             Real variable: Input.
C*
C*    ** TOLH - tolerance value to filter the Hessemberg matrix.
C*              Real variable: Local.
C*
C*    ** TOLW - tolerance value to filter the eigenvalues.
C*              Real variable: Local.
C*
C*    ** TOLZ - tolerance value to filter the eigenvectors.
C*              Real variable: Local.
C*
C*    ** SCALE - working real vector with dimensions SCALE(N).
C*
C*    ** ORT - working real vector with dimensions ORT(N).
C*
      INTEGER NM,N,MATZ,IERR,LOW,IGH
C*
      REAL EPS,TOLH,TOLW,TOLZ
      REAL A(NM,N),Z(NM,N),WR(N),WI(N),SCALE(N),ORT(N)
C*
      TOLH=EPS
      TOLW=EPS
      TOLZ=EPS
C*
      IF (N .GT. NM) THEN
      IERR=10*N
      RETURN
      ENDIF
C*
C*    Performing the balance of the input real general matrix
C*    (in place).
C*
      CALL BALANC(NM,N,A,LOW,IGH,SCALE)
C*
C*    Performing the redution of the balanced matrix (in place) to
C*    the Hessemberg superior form. It is used similarity orthogonal
C*    transformations.
C*
      CALL ORTHES(NM,N,LOW,IGH,A,ORT,TOLH)
C*
      IF (MATZ .NE. 0) THEN
C*
C*    Saving the transformations above for eigenvector computations.
C*
      CALL ORTRAN(NM,N,LOW,IGH,A,ORT,Z,TOLH)
      ENDIF
C*
C*    Computing the eigenvalues/eigenvectors of the Hessemberg matrix
C*    using the QR method.
C*
      CALL HQR2(NM,N,LOW,IGH,A,WR,WI,Z,IERR,MATZ,EPS,TOLH,*10)
C*
      IF (IERR .EQ. 0) THEN
C*
C*    Back-transforming the eigenvectors of the Hessembeg matrix to
C*    the eigenvectors of the original input matrix.
C*
      CALL BALBAK(NM,N,LOW,IGH,SCALE,N,Z)
C*
C*    Normalizing and filtering the eigenvectors (See MATZ above and
C*    comments inside ZNORMA routine).
C*
      CALL ZNORMA(NM,N,WR,WI,Z,MATZ,A,TOLW,TOLZ)
      ENDIF
C*
   10 IF (IERR .EQ. 0) RETURN
      WRITE(*,20) IERR
   20 FORMAT(/,1X,'***** The',I4,1X,'-th Eigenvalue Did Not Converge ',
     *       '*****',//)
C*
      RETURN
      END
