      SUBROUTINE FFT99B(WORK,A,TRIGS,INC,JUMP,N,LOT)
C*
      INTEGER INC,JUMP,N,LOT
      REAL WORK(*),A(*),TRIGS(*)
C*
      INTEGER NH,NX,INK,IA,IB,JA,JB,L,IABASE,IBBASE,JABASE,JBBASE,K
      REAL SCALE,C,S

C*
      NH=N/2
      NX=N+1
      INK=INC+INC
C*
      SCALE=1.0/FLOAT(N)
      IA=1
      IB=2
      JA=1
      JB=N*INC+1
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO L=1,LOT
      A(JA)=SCALE*(WORK(IA)+WORK(IB))
      A(JB)=SCALE*(WORK(IA)-WORK(IB))
      A(JA+INC)=0.
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
      ENDDO
C*
      SCALE=0.5*SCALE
      IABASE=3
      IBBASE=N-1
      JABASE=2*INC+1
      JBBASE=(N-2)*INC+1
C*
      DO K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K  )
      S=TRIGS(N+K+1)
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO L=1,LOT
      A(JA)=SCALE*((WORK(IA)+WORK(IB))
     *      +(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JB)=SCALE*((WORK(IA)+WORK(IB))
     *      -(C*(WORK(IA+1)+WORK(IB+1))+S*(WORK(IA)-WORK(IB))))
      A(JA+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *          +(WORK(IB+1)-WORK(IA+1)))
      A(JB+INC)=SCALE*((C*(WORK(IA)-WORK(IB))-S*(WORK(IA+1)+WORK(IB+1)))
     *          -(WORK(IB+1)-WORK(IA+1)))
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
      ENDDO
      IABASE=IABASE+2
      IBBASE=IBBASE-2
      JABASE=JABASE+INK
      JBBASE=JBBASE-INK
      ENDDO
C*
      IF(IABASE.NE.IBBASE) RETURN
      IA=IABASE
      JA=JABASE
      SCALE=2.0*SCALE
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO L=1,LOT
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)
      IA=IA+NX
      JA=JA+JUMP
      ENDDO
C*
      RETURN
      END
