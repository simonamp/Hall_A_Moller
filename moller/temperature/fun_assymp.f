      REAL FUNCTION FUN_ASSYMP(X)
C
C=== Function for the magnetization dependence
C    on the external field (saturation curve
C
      IMPLICIT NONE
      REAL X
      COMMON/PAWPAR/ PARA(10)
      REAL PAR
C
      VECTOR IFITCTRL(1)
      VECTOR PAR(10)
C
      REAL a(10),y
      INTEGER i
C
      DO i=1,10
         IF(IFITCTRL(1).EQ.0) THEN
            a(i)=PARA(i)
         ELSE
            a(i)=PAR(i)
         ENDIF
      ENDDO
C
      FUN_ASYYMP=a(1)-a(2)*EXP(-X*a(3))
      RETURN
      END
