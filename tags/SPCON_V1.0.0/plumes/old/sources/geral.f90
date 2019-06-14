MODULE geral

  INTEGER,PARAMETER :: contn=1,para=2,perg=3

  CONTAINS
  
  INTEGER FUNCTION Error(Mens,line,func,actv)
    IMPLICIT NONE
    
    INTEGER,INTENT(IN) :: line,actv
    CHARACTER(LEN=*) :: Mens,func
    
    PRINT *,'--------------------------------------------------------------'
    PRINT *,'ERROR: ',Mens
    PRINT *,'--------------------------------------------------------------'
    PRINT *,'Funct: ',func
    PRINT *,'Linha: ',line
    PRINT *,'--------------------------------------------------------------'
    
    IF(actv==para) THEN
      STOP
    ELSEIF(actv==contn) THEN
      Error=1
    ELSE
      Error=0
    END IF
  
  END FUNCTION Error

  !------------------------------------
  INTEGER FUNCTION Asc2Int(String)
  !------------------------------------
    !Convert a ascii sequence of numeric representation
    !	in a integer value
    !Author: Luiz Flavio Rodrigues
    !Date: 05/09/2003
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN) :: String
    INTEGER :: SizeOfString,MulFactor,Number,Position
    CHARACTER :: NumberAsc
    
    Asc2int=0
    
    SizeOfString=LEN(String)
    MulFactor=1
    DO Position=SizeOfString,1,-1		  !Decrements position
      NumberAsc=String(Position:Position+1)	  !Extract character
      Number=ICHAR(NumberAsc)-48		  !Find Ascii code
      IF(Number<0 .AND. Number>9) THEN  	  !Out of range?
  	 Print *,'ERROR: Asc position out of range'
  	 STOP
       END IF
       asc2int=asc2int+MulFactor*Number		  !Calc value of pound
       MulFactor=MulFactor*10			  !Increment pound
    END DO
 
  END FUNCTION Asc2Int



END MODULE geral
