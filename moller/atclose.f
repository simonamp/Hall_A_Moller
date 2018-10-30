      SUBROUTINE ATCLOSE(LUN)
C
C===    Close temporary file for asymmetry data
C
      IMPLICIT NONE
      INTEGER LUN
C
      CLOSE(UNIT=LUN)
C
      END
