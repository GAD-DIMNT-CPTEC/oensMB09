      SUBROUTINE PLY(N,RAD,P)
      DOUBLE PRECISION X,RAD,P,Y1,Y2,Y3,G,DCOS
      X=DCOS(RAD)
      Y1=1.D+0
      Y2=X
      DO 1 I=2,N
      G=X*Y2
      Y3=G-Y1+G-(G-Y1)/FLOAT(I)
      Y1=Y2
      Y2=Y3
1     CONTINUE
      P=Y3
      RETURN
      END
