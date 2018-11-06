C
C===  crflag.inc
C===  Flags for little/big endian machines
C
      COMMON/CRFLAG/ ILOWEND,J2ORD(2),J1ORD(4)
     +              ,IWREVENT
C
      INTEGER        ILOWEND      ! =0 - big endian (DEC) =1 - little .. (HP)
     +              ,J2ORD        ! (1) - the less significant 16-b word  
     +              ,j1ORD        ! (1) - the less sign. byte (in equiv)
     +              ,IWREVENT     ! = 0 - do not write ADC/TDC to NTUPLE
C

