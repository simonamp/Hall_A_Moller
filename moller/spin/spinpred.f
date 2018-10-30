      REAL FUNCTION SPINPRED(THETA)
C
C===    Spin predictions for the wien angle=THETA (degrees)
C
      IMPLICIT NONE
      REAL THETA
      DOUBLE PRECISION DSPINPRED
      DOUBLE PRECISION dd
C
C     ------------------------------------------------------------------
C
      dd=DSPINPRED(THETA)

      SPINPRED=REAL(dd)
C      write(6,*) THETA,dd
      END
C
      INCLUDE 'dspinpred.f'
