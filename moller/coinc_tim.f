      REAL FUNCTION COINC_TIM(C1,C2,C3,C4)
      IMPLICIT NONE
C
C---     Estimates the coincidence time from the accid. rate, and indiv. rates ...
C
      REAL C1,C2,C3,C4
C
      REAL a
C
      a=C4/((C1-C3)*(C2-C3)+C3**2)
      COINC_TIM=a
      END

