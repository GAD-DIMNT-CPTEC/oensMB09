
! SELECTED_INT_KIND(R):
!   The value of the result is the kind type parameter value 
!   of the integer type that can represent all integer values 
!   n in the range -10**R < n < 10**R
!   R must be a scalar of integer type (Ri4, Ri8).

! SELECTED_REAL_KIND(P,R):
!   The value of the result is the kind type parameter value 
!   of the real type that has a decimal precision greater than 
!   or equal to P digits as returned by the function PRECISION 
!   and a decimal exponent range greater than or equal to R 
!   as returned by the function RANGE.
!   P (optional) must be a scalar of integer type (Pr4, Pr8). 
!   R (optional) must be a scalar of integer type (Rr4, Rr8).

INTEGER, PARAMETER :: Ri4=9,  Ri8=15, &
                      Pr4=6,  Rr4=37, &
                      Pr8=15, Rr8=307

INTEGER, PARAMETER, PUBLIC :: ki4=SELECTED_INT_KIND(Ri4)
INTEGER, PARAMETER, PUBLIC :: ki8=SELECTED_INT_KIND(Ri8)
INTEGER, PARAMETER, PUBLIC :: kr4=SELECTED_REAL_KIND(Pr4,Rr4)
INTEGER, PARAMETER, PUBLIC :: kr8=SELECTED_REAL_KIND(Pr8,Rr8)

