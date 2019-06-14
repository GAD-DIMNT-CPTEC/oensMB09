MODULE m_Parameters

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: Get_Parameters

  INCLUDE "m_Parameters.h"

  INTEGER (KIND=Ki4), PUBLIC :: MendInp, ImaxInp, JmaxInp, KmaxInp, &
                                MendOut, ImaxOut, JmaxOut, KmaxOut, &
                                MendCut

  INTEGER (KIND=Ki4), PUBLIC :: Mnwv2Inp, Mnwv3Inp, &
                                Mend1Out, Mend2Out, Mend3Out, &
                                Mnwv2Out, Mnwv3Out, Mnwv0Out, Mnwv1Out, &
                                ImxOut,  JmaxhfOut, KmaxInpp, KmaxOutp, &
                                NTracers, Kdim, ICaseRec, ICaseDec, &
                                MFactorFourier, MTrigsFourier, Iter

  INTEGER (KIND=Ki4), PUBLIC :: ForecastDay

  REAL (KIND=Kr4), PUBLIC :: TimeOfDay

  REAL (KIND=Kr8), PUBLIC :: cTv, Smth_Per_Cut

  CHARACTER (LEN=128), PUBLIC :: DataInp, DataOut, DataTop, DataSig, &
                                 GDASInp, OzonInp, TracInp, OzonOut, TracOut

  CHARACTER (LEN=1024), PUBLIC :: DirInp, DirOut, DirTop, DirSig, DirGrd, DGDInp

  REAL (KIND=Kr8), PUBLIC :: EaRad =   6.37E6_Kr8 ! Earth Mean Radius (m)
  REAL (KIND=Kr8), PUBLIC :: g     =  9.80665_Kr8 ! Gravity Acceleration (m2/s)
  REAL (KIND=Kr8), PUBLIC :: Rd    =   287.05_Kr8 ! Dry Air Gas Constant (m2/s2/K)
  REAL (KIND=Kr8), PUBLIC :: Rv    =   461.50_Kr8 ! Water Vapour Air Gas Constant (m2/s2/K)
  REAL (KIND=Kr8), PUBLIC :: Cp    =   1004.6_Kr8 ! Dry Air Heat Capacity (m2/s2/K)
  REAL (KIND=Kr8), PUBLIC :: Lc    =    2.5E6_Kr8 ! Latent Heat of Condensation (m2/s2)
  REAL (KIND=Kr8), PUBLIC :: Gamma =  -6.5E-3_Kr8 ! Mean Atmospheric Lapse Rate (K/m)
  REAL (KIND=Kr8), PUBLIC :: GEps  =    1.E-9_Kr8 ! Precision For Constante Lapse Rate (No Dim)

  LOGICAL (KIND=Ki4), PUBLIC :: Get_Ozone=.FALSE., Get_Tracers=.FALSE., Smooth_Topo=.TRUE., &
                                GrADS=.TRUE., GrADSOnly=.FALSE., GDASOnly=.FALSE.


CONTAINS


SUBROUTINE Get_Parameters

  IMPLICIT NONE

  NAMELIST /ChopNML/ MendInp, ImaxInp, JmaxInp, KmaxInp, &
                     MendOut, ImaxOut, JmaxOut, KmaxOut, &
                     MendCut, Iter, Smth_Per_Cut, &
                     DirInp, DirOut, DirTop, DirSig, DirGrd, &
                     DataInp, DataOut, DataTop, DataSig, &
                     DGDInp, GDASInp, OzonInp, TracInp, OzonOut, TracOut, &
                     Get_Ozone, Get_Tracers, GrADS, GrADSOnly, GDASOnly, Smooth_Topo

  MendCut=0
  Iter=10
  Smth_Per_Cut=0.12_Kr8

  READ  (*, NML=ChopNML)
  WRITE (*, '(A)')      ' '
  WRITE (*, '(A)')      ' &ChopNML'
  WRITE (*, '(A,I5)')   '  MendInp      = ', MendInp
  WRITE (*, '(A,I5)')   '  ImaxInp      = ', ImaxInp
  WRITE (*, '(A,I5)')   '  JmaxInp      = ', JmaxInp
  WRITE (*, '(A,I5)')   '  KmaxInp      = ', KmaxInp
  WRITE (*, '(A,I5)')   '  MendOut      = ', MendOut
  WRITE (*, '(A,I5)')   '  ImaxOut      = ', ImaxOut
  WRITE (*, '(A,I5)')   '  JmaxOut      = ', JmaxOut
  WRITE (*, '(A,I5)')   '  KmaxOut      = ', KmaxOut
  WRITE (*, '(A,I5)')   '  MendCut      = ', MendCut
  WRITE (*, '(A,I5)')   '  Iter         = ', Iter
  WRITE (*, '(A,F7.3)') '  Smth_Per_Cut = ', Smth_Per_Cut
  WRITE (*, '(2A)')     '  DirInp       = ', TRIM(DirInp)
  WRITE (*, '(2A)')     '  DataInp      = ', TRIM(DataInp)
  WRITE (*, '(2A)')     '  DirOut       = ', TRIM(DirOut)
  WRITE (*, '(2A)')     '  DataOut      = ', TRIM(DataOut)
  WRITE (*, '(2A)')     '  DirTop       = ', TRIM(DirTop)
  WRITE (*, '(2A)')     '  DataTop      = ', TRIM(DataTop)
  WRITE (*, '(2A)')     '  DirSig       = ', TRIM(DirSig)
  WRITE (*, '(2A)')     '  DataSig      = ', TRIM(DataSig)
  WRITE (*, '(2A)')     '  DirGrd       = ', TRIM(DirGrd)
  WRITE (*, '(2A)')     '  DGDInp       = ', TRIM(DGDInp)
  WRITE (*, '(2A)')     '  GDASInp      = ', TRIM(GDASInp)
  WRITE (*, '(2A)')     '  OzonInp      = ', TRIM(OzonInp)
  WRITE (*, '(2A)')     '  TracInp      = ', TRIM(TracInp)
  WRITE (*, '(2A)')     '  OzonOut      = ', TRIM(OzonOut)
  WRITE (*, '(2A)')     '  TracOut      = ', TRIM(TracOut)
  WRITE (*, '(A,L6)')   '  Get_Ozone    = ', Get_Ozone
  WRITE (*, '(A,L6)')   '  Get_Tracers  = ', Get_Tracers
  WRITE (*, '(A,L6)')   '  GrADS        = ', GrADS
  WRITE (*, '(A,L6)')   '  GrADSOnly    = ', GrADSOnly
  WRITE (*, '(A,L6)')   '  GDSAOnly     = ', GDASOnly
  WRITE (*, '(A,L6)')   '  Smooth_Topo  = ', Smooth_Topo
  WRITE (*, '(A)')      ' /'

  WRITE (*, '(/,A)')    '  Numerical Precision (KIND): '
  WRITE (*, '(A,I5)')   '          Ki4 = ', Ki4
  WRITE (*, '(A,I5)')   '          Ki8 = ', Ki8
  WRITE (*, '(A,I5)')   '          Kr4 = ', Kr4
  WRITE (*, '(A,I5)')   '          Kr8 = ', Kr8
  WRITE (*, '(A)')      ' '

  Mnwv2Inp=(MendInp+1)*(MendInp+2)
  Mnwv3Inp=Mnwv2Inp+2*(MendInp+1)

  Mend1Out=MendOut+1
  Mend2Out=MendOut+2
  Mend3Out=MendOut+3
  Mnwv2Out=Mend1Out*Mend2Out
  Mnwv0Out=Mnwv2Out/2
  Mnwv3Out=Mnwv2Out+2*Mend1Out
  Mnwv1Out=Mnwv3Out/2

  ImxOut=ImaxOut+2
  JmaxhfOut=JmaxOut/2
  KmaxInpp=KmaxInp+1
  KmaxOutp=KmaxOut+1

  NTracers=1
  Kdim=1
  ICaseRec=-1
  ICaseDec=1

  MFactorFourier=64
  MTrigsFourier=3*ImaxOut/2

  IF (MendCut == 0) MendCut=MendOut

  cTv=Rv/Rd-1.0_Kr8

END SUBROUTINE Get_Parameters


END MODULE m_Parameters
