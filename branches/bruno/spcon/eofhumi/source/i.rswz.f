      SUBROUTINE RSWZ(NM,N,A,W,Z,B,IERR)
C*
C*    COMPUTES THE EIGENVALUES AND EIGENVECTORS
C*    OF A REAL SYMMETRIC MATRIX
C*
C*    NM      - FIRST DIMENSION OF MATRICES A AND Z, THE SAME
C*              VALUE SET ON DECLARATION AT THE CALLING ROUTINE
C*    N       - THE ORDER OF THE EIGEN-PROBLEM (MUST BE .LE. NM)
C*    A(NM,N) - INPUT SYMMETRIC MATRIX (FULL STORAGE)
C*    W(N)    - OUTPUT EIGENVALUES (ORDERED
C*              IN ASCENDENT MAGNITUDE VALUES)
C*    Z(NM,N) - EIGENVECTORS (EACH COLUMN IS ASSOCIATED
C*              TO THE CORRESPONDING EIGENVALUE)
C*    B(N)    - WORKING VECTOR
C*    IERR    - ERROR INDEX:
C*            = 0                  - NORMAL TERMINATION
C*            = 10*N               - N IS GREATER THAN NM
C*            = OTHER VALUE THAN 0 - THE COMPUTATION DOES NOT
C*              CONVERGE AFTER 50 ITERATIONS, BUT IT COMPUTES
C*              THE EIGENVALUES TILL IERR.
C*
C*    FORMAL ARGUMENTS:
C*
      INTEGER N,NM,IERR
      REAL A(NM,N),W(N),Z(NM,N),B(N)
C*
C*    CHECK FOR INPUT ERRORS
C*
      IF (N .GT. NM) THEN
      IERR=10*N
      RETURN
      ENDIF
C*
C*    FIND EIGENVALUES AND EIGENVECTORS
C*
      CALL TRED2(NM,N,A,W,B,Z)
      CALL TQL2(NM,N,W,B,Z,IERR)
C*
      RETURN
      END
