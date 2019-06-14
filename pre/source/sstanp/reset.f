      SUBROUTINE RESET(DATA,N)
C @(#)reset.v1	1.7

C @(#)comnum	1.7
      COMMON/COMNUM/ ZERO ,ONE  ,TWO  ,THREE,FOUR ,FIVE ,EIGHT,TEN   ,
     1               EIGTEN,THOUSD,HALF  ,FOURTH,F7    ,ONEHAF,F360  ,
     2               F180  ,F09   ,F10M6 ,FLIM  ,F999  ,       F3600 ,
     3               PAI   ,TBAR  ,CNVFAC,       F02   ,OZFAC ,PSTND ,
     4               TENTH ,F10M3, DELQ  ,QMIN  ,F24   ,F07   ,HUNDRD,
     5               F10M2, FIFTY
CC--------------------------------------------------------
      DIMENSION DATA(N)
      DO 100 I=1,N
      DATA(I)=ZERO
  100 CONTINUE
      RETURN
      END
