      FUNCTION GASDEV(IDUM)
C*
C*    GASDEV = GENERATE A GAUSSIAN DEVIATE (RANDOM NUMBER)
C*
      INTEGER IDUM
      REAL GASDEV
c*
      REAL RAN1,X,Y,RSQ,S,U
      LOGICAL ODD
C*
      SAVE ODD,U
      DATA ODD /.TRUE./
C*
      IF (ODD) THEN
C*
   10 CONTINUE
      X=2.0*RAN1(IDUM)-1.0
      Y=2.0*RAN1(IDUM)-1.0
      RSQ=X*X+Y*Y
C*
      IF (RSQ .GE. 1.0) GOTO 10
C*
      S=SQRT(-2.0*LOG(RSQ)/RSQ)
      U=S*X
      GASDEV=S*Y
      ODD=.FALSE.
C*
      ELSE
      GASDEV=U
      ODD=.TRUE.
      ENDIF
c*
      RETURN
      END
