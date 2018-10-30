C == Vectors for current target polarization (Z)
      INTEGER MXTPOLZ
      PARAMETER (MXTPOLZ=561)
      VECTOR POLTARZ(561),TARZ(561)
      VECTOR TPOLCUR(5)
C  TPOLCUR(1) - polar, (2)=target, (3) - hcoil, (4) >0 - Z vector stored, (5)=1 - Z/Hcoil beyond the limits
