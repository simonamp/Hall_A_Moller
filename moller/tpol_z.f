      REAL FUNCTION TPOL_Z(Z)
C
C===    Returns the target polarization for the given Z - long. coordinate along the foil
C       Uses 2 vectors which should be initialized vis get_targpol.kumac for the given target/hcoil
C
      IMPLICIT NONE
      REAL Z
C
      INCLUDE 'inc/a_tpol.inc'
C
      REAL z0,z1,dz
      INTEGER iz
C
      z0=TARZ(1)
      z1=TARZ(MXTPOLZ)
      dz=(z1-z0)/(MXTPOLZ-1)

      iz=INT((Z-z0)/dz+1.5)
      TPOL_Z=0.
      IF(iz.GE.1.AND.iz.LE.MXTPOLZ) TPOL_Z=POLTARZ(iz)  ! target polarization
C      print *,iz
      RETURN
      END
