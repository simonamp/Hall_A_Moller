      REAL FUNCTION WAVE_OFS(X)
C
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      COMMON/PAWPAR/ PAR(6)
      VECTOR PARW(6)
      VECTOR JFIT(2)
      REAL para(6)
C
      IF(JFIT(1).EQ.0) THEN
         DO i=1,6
            para(i)=PAR(i)
         END DO
      ELSE
         DO i=1,6
            para(i)=PARW(i)
         END DO
      ENDIF
C
C      PRINT *,X,PAR(1),PAR(2)
      WAVE_OFS=para(1)*SIN((X-para(2))*3.1415/180.*para(4))+para(3)
      END
