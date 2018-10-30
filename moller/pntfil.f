      SUBROUTINE PNTFIL
C
C---     Initialize an CW NTUPLE for run-wise polarization data
C
      IMPLICIT NONE
C
C      INTEGER IRUN,IOFS,INFO
C
C
C===      Counting rates/polarizations for a given run
C
C      INCLUDE 'inc/crunpol.inc'
C      INCLUDE 'inc/cruninf.inc'
C
      INTEGER id
C
C     ------------------------------------------------------------------
C
      id=1
      CALL HFNT(id)
C
 999  CONTINUE
      END











