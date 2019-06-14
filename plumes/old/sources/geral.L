SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:37 2005
FILE NAME: geral.f90
PROGRAM NAME: geral
DIAGNOSTIC LIST

  LINE  LEVEL( NO.): DIAGNOSTIC MESSAGE

    46  vec  (   3): Unvectorized loop.
SUPER-UX  R14.1 FORTRAN90/SX         Rev.313        Mon Aug 29 15:27:37 2005
FILE NAME: geral.f90
PROGRAM NAME: geral
FORMAT LIST

  LINE    LOOP     FORTRAN STATEMENT

     1:            MODULE geral
     2:            
     3:              INTEGER,PARAMETER :: contn=1,para=2,perg=3
     4:            
     5:              CONTAINS
     6:              
     7:              INTEGER FUNCTION Error(Mens,line,func,actv)
     8:                IMPLICIT NONE
     9:                
    10:                INTEGER,INTENT(IN) :: line,actv
    11:                CHARACTER(LEN=*) :: Mens,func
    12:                
    13:                PRINT *,'--------------------------------------------------------------'
    14:                PRINT *,'ERROR: ',Mens
    15:                PRINT *,'--------------------------------------------------------------'
    16:                PRINT *,'Funct: ',func
    17:                PRINT *,'Linha: ',line
    18:                PRINT *,'--------------------------------------------------------------'
    19:                
    20:                IF(actv==para) THEN
    21:                  STOP
    22:                ELSEIF(actv==contn) THEN
    23:                  Error=1
    24:                ELSE
    25:                  Error=0
    26:                END IF
    27:              
    28:              END FUNCTION Error
    29:            
    30:              !------------------------------------
    31:              INTEGER FUNCTION Asc2Int(String)
    32:              !------------------------------------
    33:                !Convert a ascii sequence of numeric representation
    34:                !	in a integer value
    35:                !Author: Luiz Flavio Rodrigues
    36:                !Date: 05/09/2003
    37:                IMPLICIT NONE
    38:                CHARACTER(LEN=*),INTENT(IN) :: String
    39:                INTEGER :: SizeOfString,MulFactor,Number,Position
    40:                CHARACTER :: NumberAsc
    41:                
    42:                Asc2int=0
    43:                
    44:                SizeOfString=LEN(String)
    45:                MulFactor=1
    46: +------>       DO Position=SizeOfString,1,-1		  !Decrements position
    47: |                NumberAsc=String(Position:Position+1)	  !Extract character
    48: |                Number=ICHAR(NumberAsc)-48		  !Find Ascii code
    49: |                IF(Number<0 .AND. Number>9) THEN  	  !Out of range?
    50: |            	 Print *,'ERROR: Asc position out of range'
    51: |            	 STOP
    52: |                 END IF
    53: |                 asc2int=asc2int+MulFactor*Number		  !Calc value of pound
    54: |                 MulFactor=MulFactor*10			  !Increment pound
    55: +------        END DO
    56:             
    57:              END FUNCTION Asc2Int
    58:            
    59:            
    60:            
    61:            END MODULE geral
