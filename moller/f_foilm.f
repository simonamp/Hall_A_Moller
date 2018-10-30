      REAL FUNCTION F_FOILM(H)
C
C ---    Foil magnetization
C
      IMPLICIT NONE
      REAL H
      COMMON/PAWPAR/ PAR(10)
      REAL PAR
      REAL h0
C
C      PAR(1)=8
C      PAR(2)=0.01
      h0=240.
      F_FOILM=PAR(1)*ATAN(PAR(2)*(H-PAR(3)))/
     +               ATAN(PAR(2)*(h0-PAR(3)))
      END

