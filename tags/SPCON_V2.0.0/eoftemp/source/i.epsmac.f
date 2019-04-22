      FUNCTION EPSMAC(N)
C*
      INTEGER N
      REAL EPSMAC,ONE,TWO,EPS
      CHARACTER EPSC*72
C*
      ONE=FLOAT(1)
      TWO=ONE+ONE
C*
      EPS=ONE
      N=0
   10 N=N+1
      EPS=EPS/TWO
      IF (ONE+EPS .GT. ONE) GOTO 10
      N=1-N
C*
   20 EPSMAC=TWO**N
C*
      EPS=ONE+EPSMAC
      WRITE(*,'(/,A,E72.64)')' EPSW =',EPS
      WRITE(EPSC,'(E72.64)')EPS
      WRITE(*,'(2A)')' EPSC =',EPSC
      READ(EPSC,'(E72.64)')EPS
      WRITE(*,'(A,E72.64)')' EPSR =',EPS
      IF (EPS-ONE .EQ. ONE-ONE) THEN
      N=N+1
      GOTO 20
      ENDIF
C*
      EPS=TWO**(N-1)
      WRITE(*,'(/,A,I4,2(A,1PG30.20))')
     *        ' NEXP-1 =',N-1,' EPS    =',EPS,
     *        ' 1+EPS    =',ONE+EPS
      WRITE(*,'(A,I4,2(A,1PG30.20),/)')
     *        ' NEXP   =',N,' EPSMAC =',EPSMAC,
     *        ' 1+EPSMAC =',ONE+EPSMAC
C*
      RETURN
      END
