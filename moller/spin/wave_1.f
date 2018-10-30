      REAL FUNCTION WAVE_1(X)
      
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      COMMON/PAWPAR/ PARA(10)
      VECTOR PAR(10)
      VECTOR IPFIT(2)
      REAL period,phase
C
      period=1.
      phase=PARA(2)
      IF(IPFIT(1).EQ.1) THEN
        period=PAR(3)
      ELSEIF(IPFIT(1).EQ.2) THEN
        period=PARA(3)
C        WRITE(6,*) 'period=',period
      ELSEIF(IPFIT(1).EQ.3) THEN
        phase=PAR(2)
C        WRITE(6,*) 'period=',period
      ENDIF
C      PRINT *,X,PARA(1),PARA(2),PARA(3),period
      WAVE_1=PARA(1)*COS((X-phase)*3.1415/180.*period)
C       write(6,*) x,para(1),para(2),period,wave_1
      END
