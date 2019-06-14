      SUBROUTINE IDENT(NM,N,Z)
C*
C*    ** Initialize Z to identity matrix by sqrt(2)
C*
      INTEGER NM,N,I,J
      REAL Z(NM,*)
C*
      REAL ZERO,ONE,TWO,SQRT2
      DATA ZERO /0.0E0/, ONE /1.0E0/, TWO /2.0E0/
C*
      SQRT2=ONE/SQRT(TWO)
      DO 10 I=1,N
      DO 20 J=1,N
      Z(I,J)=ZERO
   20 CONTINUE
      Z(I,I)=SQRT2
   10 CONTINUE
C*
      RETURN
      END
