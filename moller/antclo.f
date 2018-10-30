      SUBROUTINE ANTCLO
C
C---     Initialize an CW NTUPLE for asymmetry
C
      IMPLICIT NONE
C
      INTEGER id,icycle
C
C     ------------------------------------------------------------------
C
      id=1
      CALL HCDIR('//asym',' ')
      CALL HROUT(id,icycle,' ')
C
      CALL HREND('asym')
      CLOSE (UNIT=2)
C
 999  CONTINUE
      END











