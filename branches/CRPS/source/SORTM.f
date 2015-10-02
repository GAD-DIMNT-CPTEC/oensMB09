      SUBROUTINE SORTM(A,N,NC,K)
C     generalized version of sort subroutine for multidim. arrays
C     a(n,nc) array of n rows, nc columns to be sorted by column k
      PARAMETER (M=50)
      INTEGER N,I,J,L,R,S,STACK(M,2)
      REAL A(n,nc), X, W
 
      S = 1
      STACK(1, 1) = 1
      STACK(1, 2) = N
  100 CONTINUE
c---  SIMULATE OUTER REPEAT LOOP ...
      L = STACK(S, 1)
      R = STACK(S, 2)
      S = S - 1
  200 CONTINUE
c---  SIMULATE MIDDLE REPEAT LOOP ...
      I = L
      J = R
c     change last # for column to be sorted *********
      X = A ((L+R)/2, K)
  300 CONTINUE
c---  SIMULATE INNER REPEAT LOOP
  400 CONTINUE
c---  SIMULATE WHILE LOOP
c     change #  **************************************
      IF (A(I, K).LT.X) THEN
       I = I + 1
       GOTO 400
      ENDIF
  500 CONTINUE
c---  SIMULATE WHILE LOOP
c     change #  **************************************
      IF (X.LT.A(J, K)) THEN
       J = J -1
       GOTO 500
      ENDIF
      IF (I.LE.J) THEN
c     2nd # is total # of columns **********************
      DO 1000 ICOL = 1, NC, 1
       W          = A(I, ICOL)
       A(I, ICOL) = A(J, ICOL)
       A(J, ICOL) = W
 1000 continue
      I = I + 1
      J = J - 1
      ENDIF
c---  END OF INNER REPEAT LOOP
      IF (I.LE.J) GOTO 300
      IF (I.LT.R) THEN
       S = S + 1
       IF (S.GT.M) THEN
        PRINT *, 'STACK OVERFLOW IN QSORT'
        STOP 'STACK OVF'
       ENDIF
       STACK(S, 1) = I
       STACK(S, 2) = R
      ENDIF
      R = J
c---  END OF MIDDLE REPEAT LOOP
      IF (L.LT.R) GOTO 200
c---  END OF OUTER REPEAT LOOP
      IF (S.GT.0) GOTO 100
      RETURN
      END

