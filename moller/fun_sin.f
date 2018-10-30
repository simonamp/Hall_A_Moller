      REAL FUNCTION FUN_SIN(X)
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
      FUN_SIN=a(1)*SIN(X*a(2)+a(3))
      RETURN
      END
