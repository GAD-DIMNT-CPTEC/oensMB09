      SUBROUTINE PLN0(N,RAD,P)
C*
      INTEGER N
      REAL RAD,P
C*
      INTEGER I
      REAL X,Y1,Y2,G,Y3
C*
      X=COS(RAD)
      Y1=1.0E0
      Y2=X
C*
      DO I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/FLOAT(I)
      Y1=Y2
      Y2=Y3
      ENDDO
C*
      P=Y3
C*
      RETURN
      END
