      SUBROUTINE PLY(N,RAD,P)
C*
      INTEGER N
      DOUBLE PRECISION RAD,P
C*
      INTEGER I
      DOUBLE PRECISION X,Y1,Y2,Y3,G
C*
      X=DCOS(RAD)
      Y1=1.0D+00
      Y2=X
C*
      DO 10 I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/DBLE(I)
      Y1=Y2
      Y2=Y3
   10 CONTINUE
C*
      P=Y3
C*
      RETURN
      END
