      REAL FUNCTION BOP_FLD(IRUN,BOP)
C
C---      Returns the Bz in Gauss for the given BOP (A) and for IRUN 
C
      IMPLICIT NONE
      INTEGER IRUN
      REAL BOP
C
      INTEGER isetup   ! =1 - target holder 2 (sliding), the beamline coil configuration
      REAL a(10),b(10)
      DATA a/-13.65, 9*0. /
      DATA b/ 19.62, 9*0. /
C
      BOP_FLD=0.
      isetup=0
      IF(IRUN.GT.11700) THEN
         isetup=1
      ENDIF
C
      IF(isetup.EQ.0) GO TO 999
C
      BOP_FLD=a(isetup)+b(isetup)*ABS(BOP)
C
 999  RETURN
      END
