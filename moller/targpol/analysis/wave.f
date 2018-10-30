      REAL FUNCTION WAVE(X)
      
C
      IMPLICIT NONE
      REAL X
      COMMON/PAWPAR/ PARA(10)
      REAL PARA
      VECTOR PAR(10)
      VECTOR IPFIT(1)
      INTEGER i
      REAL period,ap(10)
C
      period=1.
      DO i=1,4
         ap(i)=PARA(i)
         IF(IPFIT(1).LT.0) ap(i)=PAR(i)
      ENDDO
      period=ap(3)
      IF(IPFIT(1).EQ.2) ap(3)=PAR(3)
C      PRINT *,X,PARA(1),PARA(2),PARA(3),period
      WAVE=ap(1)*SIN((X-ap(2))*ap(3))+ap(4)
      END
