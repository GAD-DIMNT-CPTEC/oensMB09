      SUBROUTINE FFT99A(A,WORK,TRIGS,INC,JUMP,N,LOT)
C*
      INTEGER INC,JUMP,N,LOT
      REAL A(*),WORK(*),TRIGS(*)
C*
      INTEGER NH,NX,INK,IA,IB,JA,JB,L,IABASE,IBBASE,JABASE,JBBASE,K
      REAL C,S
C*
      NH=N/2
      NX=N+1
      INK=INC+INC
C*
      IA=1
      IB=N*INC+1
      JA=1
      JB=2
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO L=1,LOT
      WORK(JA)=A(IA)+A(IB)
      WORK(JB)=A(IA)-A(IB)
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
      ENDDO
C*
      IABASE=2*INC+1
      IBBASE=(N-2)*INC+1
      JABASE=3
      JBBASE=N-1
C*
      DO K=3,NH,2
      IA=IABASE
      IB=IBBASE
      JA=JABASE
      JB=JBBASE
      C=TRIGS(N+K)
      S=TRIGS(N+K+1)
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO L=1,LOT
      WORK(JA)=(A(IA)+A(IB))-
     *         (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JB)=(A(IA)+A(IB))+
     *         (S*(A(IA)-A(IB))+C*(A(IA+INC)+A(IB+INC)))
      WORK(JA+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))+
     *           (A(IA+INC)-A(IB+INC))
      WORK(JB+1)=(C*(A(IA)-A(IB))-S*(A(IA+INC)+A(IB+INC)))-
     *           (A(IA+INC)-A(IB+INC))
      IA=IA+JUMP
      IB=IB+JUMP
      JA=JA+NX
      JB=JB+NX
      ENDDO
      IABASE=IABASE+INK
      IBBASE=IBBASE-INK
      JABASE=JABASE+2
      JBBASE=JBBASE-2
      ENDDO
C*
      IF(IABASE.NE.IBBASE) RETURN
      IA=IABASE
      JA=JABASE
cpen Per Nyberg, HNSX Supercomputers March 1994
*vdir nodep
      DO L=1,LOT
      WORK(JA)=2.0*A(IA)
      WORK(JA+1)=-2.0*A(IA+INC)
      IA=IA+JUMP
      JA=JA+NX
      ENDDO
C*
      RETURN
      END
