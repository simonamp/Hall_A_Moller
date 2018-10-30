      REAL FUNCTION WAVE(X)
      
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      COMMON/PAWPAR/ PARA(10)
      VECTOR PAR(10)
      VECTOR IPFIT(1)
      REAL period
C
      period=1.
      IF(IPFIT(1).EQ.1) THEN
        period=PAR(1)
      ELSEIF(IPFIT(1).EQ.2) THEN
        period=PARA(3)
C        WRITE(6,*) 'period=',period
      ENDIF
C      PRINT *,X,PARA(1),PARA(2),PARA(3),period
      WAVE=PARA(1)*SIN((X-PARA(2))*3.1415/180.*period)
      END
