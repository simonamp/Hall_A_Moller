      REAL FUNCTION GETENERG(I1,I2)
C
C=== Adds all energies from I1 to I2
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
C
      VECTOR IDD0(1)
      VECTOR PEDES(36)
      VECTOR CALIB(36)
C
      DATA ievv/0/
C
      id0=IDD0(1)
      ievv=ievv+1

      earm=0.
      DO i=I1,I2
        a=IADC(i)-PEDES(i)
        e=a*CALIB(i)
        earm=earm+e
      END DO
C
      GETENERG=earm
C
 999  CONTINUE
      END

