!
!  $Author: alexalm $
!  $Date: 2005/10/17 14:25:37 $
!  $Revision: 1.1.1.1 $
!
!
! r8bt* :auxiliary mathematical and logical subroutines.
!
SUBROUTINE r8btgt(a,na,c,nc,b)

  ! substituir por array assignment b = a > c

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: nc
  REAL,    INTENT(in ) :: a(na)
  REAL,    INTENT(in ) :: c(nc)
  LOGICAL, INTENT(out) :: b(na)
  INTEGER :: i
  IF(nc.EQ.1) THEN
     DO i = 1,na
        b(i) = a(i).GT.c(1)
     END DO
  ELSE
     DO i = 1,na
        b(i) = a(i).GT.c(i)
     END DO
  END IF
END SUBROUTINE r8btgt






SUBROUTINE r8btge(a,na,c,nc,b)

  ! substituir por array assignment b = a >= c

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: nc
  REAL,    INTENT(in ) :: a(na)
  REAL,    INTENT(in ) :: c(nc)
  LOGICAL, INTENT(out) :: b(na)
  INTEGER :: i
  IF(nc.EQ.1) THEN
     DO i = 1,na
        b(i) = a(i).GE.c(1)
     END DO
  ELSE
     DO i = 1,na
        b(i) = a(i).GE.c(i)
     END DO
  END IF
END SUBROUTINE r8btge





SUBROUTINE r8bteq(a,na,c,nc,b)

  ! substituir por array assignment b = a == c

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: nc
  REAL,    INTENT(in ) :: a(na)
  REAL,    INTENT(in ) :: c(nc)
  LOGICAL, INTENT(out) :: b(na)
  INTEGER :: i
  IF(nc.EQ.1) THEN
     DO i = 1,na
        b(i) = a(i).EQ.c(1)
     END DO
  ELSE
     DO i = 1,na
        b(i) = a(i).EQ.c(i)
     END DO
  END IF
END SUBROUTINE r8bteq





SUBROUTINE r8scnt(b,n,nc)
  !
  ! r8scnt :auxiliary mathematical and logical subroutines.
  !

  ! nc = count(b)

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: n
  LOGICAL, INTENT(in ) :: b(n)
  INTEGER, INTENT(out) :: nc
  INTEGER :: i
  nc = 0
  DO i = 1,n
     IF (b(i)) nc = nc + 1
  END DO
END SUBROUTINE r8scnt





SUBROUTINE r8vctr(a,na,b,c,n)

  ! substituir por where (b); c = a; end where 

  !
  ! $author: cptec $
  ! $date: 1999/06/29 19:05:38 $
  ! $revision: 1.0 $
  !
  ! r8vctr :auxiliary mathematical and logical subroutines.
  !
  IMPLICIT NONE
  INTEGER, INTENT(in)  :: na
  INTEGER, INTENT(in)  :: n
  REAL   , INTENT(in)  :: a(na)
  REAL   , INTENT(out) :: c(n)
  LOGICAL, INTENT(in)  :: b(n)
  INTEGER :: i
  IF(na.EQ.1) THEN
     DO i = 1,n
        IF ( b(i) ) c(i) = a(1)
     END DO
  ELSE
     DO i = 1,n
        IF ( b(i) ) c(i) = a(i)
     END DO
  END IF
END SUBROUTINE r8vctr






SUBROUTINE r8vgat(v,nv,l,u,n)
  !
  ! r8vgat :auxiliary mathematical and logical subroutines.
  !

  ! parece ser pack (gather); depende da invocacao

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: nv
  INTEGER, INTENT(in ) :: n
  REAL,    INTENT(in ) :: v(nv)
  REAL,    INTENT(out) :: u(n)
  INTEGER, INTENT(in ) :: l(n)
  INTEGER :: i
  DO i = 1, n
     IF (l(i) <= nv) u(i) = v(l(i))
  END DO
END SUBROUTINE r8vgat





SUBROUTINE r8vsct(a,na,b,c,n)

  ! substituir por c(b(1:na)) = a

  !
  ! r8vsct :auxiliary mathematical and logical subroutines.
  !
  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: n
  REAL,    INTENT(in ) :: a(na)
  REAL,    INTENT(out) :: c(n)
  INTEGER, INTENT(in ) :: b(n)
  INTEGER :: i
  DO i = 1,na
     c(b(i)) = a(i)
  END DO
END SUBROUTINE r8vsct






SUBROUTINE b8not(b1,b3,n)
  LOGICAL b1(n),b3(n)
  DO i = 1,n
     b3(i) = .NOT.b1(i)
  END DO
END SUBROUTINE b8not





SUBROUTINE b8eq(b1,b3,n)
  LOGICAL b1(n),b3(n)
  DO i = 1,n
     b3(i) = b1(i)
  END DO
END SUBROUTINE b8eq





SUBROUTINE i8vgat(v,nv,l,u,n)
  !
  ! i8vgat :auxiliary mathematical and logical subroutines.
  !

  ! parece ser pack (gather); depende da invocacao

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: nv
  INTEGER, INTENT(in ) :: n
  INTEGER, INTENT(in ) :: v(nv)
  INTEGER, INTENT(in ) :: l(n)
  INTEGER, INTENT(out) :: u(n)
  INTEGER :: i
  DO i = 1,n
     IF (l(i) <= nv) u(i) = v(l(i))
  END DO
END SUBROUTINE i8vgat





SUBROUTINE i8vint(j,k,l,n)
  !
  ! i8vint :auxiliary mathematical and logical subroutines.
  !
  INTEGER l(n)
  l(1) = j
  DO i = 2,n
     l(i) = l(i-1) + k
  END DO
END SUBROUTINE i8vint





SUBROUTINE rvabs(a,b,n)

  ! substituir por b = abs(a)

  !
  ! $author: cptec $
  ! $date: 1999/06/29 19:05:38 $
  ! $revision: 1.0 $
  !
  ! rvabs  :gives the absolute value of a real vector.
  !
  IMPLICIT NONE
  INTEGER, INTENT(in ) :: n
  REAL,    INTENT(in ) :: a(n)
  REAL,    INTENT(out) :: b(n)
  INTEGER i
  DO i = 1,n
     b(i) = ABS(a(i))
  END DO
END SUBROUTINE rvabs






SUBROUTINE rvacos(a,b,n)
  !
  ! rvacos :gives the arc cosine of a real vector.
  !
  REAL a(n),b(n)
  DO i = 1,n
     b(i) = ACOS(a(i))
  END DO
END SUBROUTINE rvacos





SUBROUTINE rvexp(a,b,n)

  ! substituir por b = exp(a)

  !
  ! $author: cptec $
  ! $date: 1999/06/29 19:05:38 $
  ! $revision: 1.0 $
  !
  ! rvexp  :gives the exponential of a real vector.
  !
  IMPLICIT NONE
  INTEGER, INTENT(in ) :: n
  REAL,    INTENT(in ) :: a(n)
  REAL,    INTENT(out) :: b(n)
  INTEGER :: i
  DO i = 1,n
     b(i) = EXP(a(i))
  END DO
END SUBROUTINE rvexp





SUBROUTINE rvlog(a,b,n)
  !
  ! rvlog  :gives the natural logarithm of a real vector.
  !
  REAL a(n),b(n)
  DO i = 1,n
     b(i) = LOG(a(i))
  END DO
END SUBROUTINE rvlog





SUBROUTINE rvsqrt(a,b,n)

  ! substituir por b = sqrt(a)

  !
  ! $author: cptec $
  ! $date: 1999/06/29 19:05:38 $
  ! $revision: 1.0 $
  !
  ! rvsqrt :gives the square root of a real vector.
  !
  IMPLICIT NONE
  INTEGER, INTENT(in ) :: n
  REAL,    INTENT(in ) :: a(n)
  REAL,    INTENT(out) :: b(n)
  INTEGER :: i
  DO i = 1,n
     b(i) =  SQRT(a(i))
  END DO
END SUBROUTINE rvsqrt





SUBROUTINE vimax(a,na,c,nc)
  !
  ! $author: cptec $
  ! $date: 1999/06/29 19:05:38 $
  ! $revision: 1.0 $
  !
  ! vimax  :gives the maximum value of an integer vector.
  !

  ! substituir por array assignment c = max(c, a)

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: nc
  INTEGER, INTENT(in ) :: a(na)
  INTEGER, INTENT(out) :: c(nc)
  INTEGER :: i
  IF (na == 1) THEN
     DO i = 1,nc
        c(i) = MAX(a(1),c(i))
     END DO
  ELSE
     DO i = 1,nc
        c(i) = MAX(a(i),c(i))
     END DO
  END IF
END SUBROUTINE vimax





SUBROUTINE vimin(a,na,c,nc)
  !
  ! vimin  :gives the minimum value of an integer vector.
  ! 

  ! substituir por array assignment c = min(c, a)

  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: nc
  INTEGER, INTENT(in ) :: a(na)
  INTEGER, INTENT(out) :: c(nc)
  INTEGER :: i
  IF (na == 1) THEN
     DO i = 1,nc
        c(i) = MIN(a(1),c(i))
     END DO
  ELSE
     DO i = 1,nc
        c(i) = MIN(a(i),c(i))
     END DO
  END IF
END SUBROUTINE vimin





SUBROUTINE vamax(a,na,c,nc)

  ! substituir por array assignment c = max(c,a)

  !
  ! $author: cptec $
  ! $date: 1999/06/29 19:05:38 $
  ! $revision: 1.0 $
  !
  ! vamax  :gives the maximum value of a real vector.
  !
  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: nc
  REAL,    INTENT(in ) :: a(na)
  REAL,    INTENT(out) :: c(nc)
  INTEGER :: i
  IF(na.EQ.1) THEN
     DO i = 1,nc
        c(i) = MAX(a(1),c(i))
     END DO
  ELSE
     DO i = 1,nc
        c(i) = MAX(a(i),c(i))
     END DO
  END IF
END SUBROUTINE vamax





SUBROUTINE vamin(a,na,c,nc)

  ! substituir por array assignment c = min(c,a)

  !
  ! $author: cptec $
  ! $date: 1999/06/29 19:05:38 $
  ! $revision: 1.0 $
  !
  ! vamin  :gives the minimum value of a real vector.
  !
  IMPLICIT NONE
  INTEGER, INTENT(in ) :: na
  INTEGER, INTENT(in ) :: nc
  REAL,    INTENT(in ) :: a(na)
  REAL,    INTENT(out) :: c(nc)
  INTEGER :: i
  IF(na.EQ.1) THEN
     DO i = 1,nc
        c(i) = MIN(a(1),c(i))
     END DO
  ELSE
     DO i = 1,nc
        c(i) = MIN(a(i),c(i))
     END DO
  END IF
END SUBROUTINE vamin
