      SUBROUTINE FFTRIG(TRIGS,N,MODE)
C*
      INTEGER N,MODE
      REAL TRIGS(*)
C*
      INTEGER IMODE,NN,L,I,NH,LA
      REAL PI,DEL,ANGLE
C*
      PI=2.0*ASIN(1.0)
      IMODE=IABS(MODE)
      NN=N
      IF (IMODE.GT.1 .AND. IMODE.LT.6) NN=N/2
      DEL=(PI+PI)/FLOAT(NN)
      L=NN+NN
      DO I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(I  )=COS(ANGLE)
      TRIGS(I+1)=SIN(ANGLE)
      ENDDO
      IF (IMODE .EQ. 1) RETURN
      IF (IMODE .EQ. 8) RETURN
C*
      DEL=0.5*DEL
      NH=(NN+1)/2
      L=NH+NH
      LA=NN+NN
      DO I=1,L,2
      ANGLE=0.5*FLOAT(I-1)*DEL
      TRIGS(LA+I  )=COS(ANGLE)
      TRIGS(LA+I+1)=SIN(ANGLE)
      ENDDO
      IF (IMODE .LE. 3) RETURN
C*
      DEL=0.5*DEL
      LA=LA+NN
      IF (MODE .NE. 5) THEN
      DO I=2,NN
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=2.0*SIN(ANGLE)
      ENDDO
      RETURN
      ENDIF
C*
      DEL=0.5*DEL
      DO I=2,N
      ANGLE=FLOAT(I-1)*DEL
      TRIGS(LA+I)=SIN(ANGLE)
      ENDDO
C*
      RETURN
      END
