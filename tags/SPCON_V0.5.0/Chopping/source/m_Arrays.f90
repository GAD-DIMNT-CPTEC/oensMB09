MODULE m_Arrays

  USE m_Parameters, ONLY: Ki4, Kr4, Kr8, KmaxInp, KmaxInpp, NTracers, &
                          ImaxOut, JmaxOut, KmaxOut, KmaxOutp, &
                          Mnwv2Inp, Mnwv2Out, Mnwv3Inp, Mnwv3Out

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: Get_Arrays, Cls_Arrays, Get_SpHuTracers, Cls_SpHuTracers

  INTEGER (KIND=Ki4), DIMENSION (4), PUBLIC :: DateInitial, DateCurrent

  REAL (KIND=Kr4), DIMENSION (:), ALLOCATABLE, PUBLIC :: &
                   SigIInp, SigLInp, SigIOut, SigLOut

  REAL (KIND=Kr4), DIMENSION (:), ALLOCATABLE, PUBLIC :: qWorkInp, qWorkOut

  REAL (KIND=Kr8), DIMENSION (:), ALLOCATABLE, PUBLIC :: &
                   DelSigmaInp, SigInterInp, SigLayerInp, &
                   DelSigmaOut, SigInterOut, SigLayerOut

  REAL (KIND=Kr8), DIMENSION (:), ALLOCATABLE, PUBLIC :: &
                   qTopoInp, qLnPsInp, qTopoOut, qLnPsOut, qTopoRec

  REAL (KIND=Kr8), DIMENSION (:,:), ALLOCATABLE, PUBLIC :: &
                   qUvelInp, qVvelInp, qUvelOut, qVvelOut, &
                   qDivgInp, qVortInp, qTvirInp, &
                   qDivgOut, qVortOut, qTvirOut

  REAL (KIND=Kr4), DIMENSION (:,:), ALLOCATABLE, PUBLIC :: gWorkOut

  REAL (KIND=Kr8), DIMENSION (:,:), ALLOCATABLE, PUBLIC :: &
                   gTopoInp, gTopoOut, gTopoDel, &
                   gLnPsInp, gPsfcInp, gLnPsOut, gPsfcOut

  REAL (KIND=Kr8), DIMENSION (:,:,:), ALLOCATABLE, PUBLIC :: &
                   qSpHuInp, qSpHuOut, &
                   gUvelInp, gVvelInp, gTvirInp, &
                   gDivgInp, gVortInp, gPresInp, &
                   gUvelOut, gVvelOut, gTvirOut, &
                   gDivgOut, gVortOut, gPresOut

  REAL (KIND=Kr8), DIMENSION (:,:,:,:), ALLOCATABLE, PUBLIC :: &
                   gSpHuInp, gSpHuOut

  INTEGER (KIND=Ki4) :: Mnwv2Rec

CONTAINS


SUBROUTINE Get_Arrays

  IMPLICIT NONE

  Mnwv2Rec=MAX(Mnwv2Inp,Mnwv2Out)
  ALLOCATE (qTopoRec (Mnwv2Rec))
  ALLOCATE (qWorkInp (Mnwv2Inp))
  ALLOCATE (qWorkOut (Mnwv2Out))
  ALLOCATE (SigIInp (KmaxInpp), SigLInp (KmaxInp))
  ALLOCATE (SigIOut (KmaxOutp), SigLOut (KmaxOut))
  ALLOCATE (DelSigmaInp (KmaxInp), SigInterInp (KmaxInpp), SigLayerInp (KmaxInp))
  ALLOCATE (DelSigmaOut (KmaxOut), SigInterOut (KmaxOutp), SigLayerOut (KmaxOut))
  ALLOCATE (qTopoInp (Mnwv2Inp), qLnPsInp (Mnwv2Inp))
  ALLOCATE (qTopoOut (Mnwv2Out), qLnPsOut (Mnwv2Out))
  ALLOCATE (qDivgInp (Mnwv2Inp,KmaxInp), &
            qVortInp (Mnwv2Inp,KmaxInp))
  ALLOCATE (qDivgOut (Mnwv2Out,KmaxOut), &
            qVortOut (Mnwv2Out,KmaxOut))
  ALLOCATE (qUvelInp (Mnwv3Inp,KmaxInp), &
            qVvelInp (Mnwv3Inp,KmaxInp))
  ALLOCATE (qUvelOut (Mnwv3Out,KmaxOut), &
            qVvelOut (Mnwv3Out,KmaxOut))
  ALLOCATE (qTvirInp (Mnwv2Inp,KmaxInp))
  ALLOCATE (qTvirOut (Mnwv2Out,KmaxOut))
  ALLOCATE (gWorkOut (ImaxOut,JmaxOut))
  ALLOCATE (gTopoInp (ImaxOut,JmaxOut), &
            gTopoOut (ImaxOut,JmaxOut), &
            gTopoDel (ImaxOut,JmaxOut), &
            gLnPsInp (ImaxOut,JmaxOut), &
            gLnPsOut (ImaxOut,JmaxOut), &
            gPsfcInp (ImaxOut,JmaxOut), &
            gPsfcOut (ImaxOut,JmaxOut))
  ALLOCATE (gUvelInp (ImaxOut,JmaxOut,KmaxInp), &
            gVvelInp (ImaxOut,JmaxOut,KmaxInp))
  ALLOCATE (gDivgInp (ImaxOut,JmaxOut,KmaxInp), &
            gVortInp (ImaxOut,JmaxOut,KmaxInp))
  ALLOCATE (gTvirInp (ImaxOut,JmaxOut,KmaxInp), &
            gPresInp (ImaxOut,JmaxOut,KmaxInp))
  ALLOCATE (gUvelOut (ImaxOut,JmaxOut,KmaxOut), &
            gVvelOut (ImaxOut,JmaxOut,KmaxOut))
  ALLOCATE (gDivgOut (ImaxOut,JmaxOut,KmaxOut), &
            gVortOut (ImaxOut,JmaxOut,KmaxOut))
  ALLOCATE (gTvirOut (ImaxOut,JmaxOut,KmaxOut), &
            gPresOut (ImaxOut,JmaxOut,KmaxOut))

END SUBROUTINE Get_Arrays


SUBROUTINE Cls_Arrays

  IMPLICIT NONE

  DEALLOCATE (qTopoRec)
  DEALLOCATE (qWorkInp)
  DEALLOCATE (qWorkOut)
  DEALLOCATE (DelSigmaInp, SigInterInp, SigLayerInp)
  DEALLOCATE (DelSigmaOut, SigInterOut, SigLayerOut)
  DEALLOCATE (qTopoInp, qLnPsInp)
  DEALLOCATE (qTopoOut, qLnPsOut)
  DEALLOCATE (qDivgInp, qVortInp)
  DEALLOCATE (qDivgOut, qVortOut)
  DEALLOCATE (qUvelInp, qVvelInp)
  DEALLOCATE (qUvelOut, qVvelOut)
  DEALLOCATE (qTvirInp)
  DEALLOCATE (qTvirOut)
  DEALLOCATE (gWorkOut)
  DEALLOCATE (gTopoInp, gTopoOut, gTopoDel)
  DEALLOCATE (gLnPsInp, gLnPsOut, gPsfcInp, gPsfcOut)
  DEALLOCATE (gUvelInp, gVvelInp)
  DEALLOCATE (gDivgInp, gVortInp)
  DEALLOCATE (gTvirInp)
  DEALLOCATE (gPresInp)
  DEALLOCATE (gUvelOut, gVvelOut)
  DEALLOCATE (gDivgOut, gVortOut)
  DEALLOCATE (gTvirOut)
  DEALLOCATE (gPresOut)

END SUBROUTINE Cls_Arrays


SUBROUTINE Get_SpHuTracers

  IMPLICIT NONE

  ALLOCATE (qSpHuInp (Mnwv2Inp,KmaxInp,NTracers))
  ALLOCATE (qSpHuOut (Mnwv2Out,KmaxOut,NTracers))
  ALLOCATE (gSpHuInp (ImaxOut,JmaxOut,KmaxInp,NTracers))
  ALLOCATE (gSpHuOut (ImaxOut,JmaxOut,KmaxOut,NTracers))

END SUBROUTINE Get_SpHuTracers


SUBROUTINE Cls_SpHuTracers

  IMPLICIT NONE

  DEALLOCATE (qSpHuInp, qSpHuOut)
  DEALLOCATE (gSpHuInp, gSpHuOut)

END SUBROUTINE Cls_SpHuTracers


END MODULE m_Arrays
