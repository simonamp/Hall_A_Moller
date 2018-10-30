      REAL FUNCTION FUN_MAGNET(X)
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
C      a(4)=PAR(4)
C
      y=X+a(1)**2
C      write(6,*) X,y
      FUN_MAGNET=a(3)*2./3.1416
     +      *ATAN(y*(a(2)**2+0.*a(4)**2/(y**2+a(5)**2)))
C      FUN_MAGNET=y*(a(2)**2+a(3)**2/(y**2+a(4)))
C      FUN_MAGNET=a(4)*2./3.1416*ATAN(y*(a(2)+a(3)**2*y**2))
C      FUN_MAGNET=y*(a(2)+a(3)**2*y**2)
C     +          +a(5)**2*y**5)
C      FUN_MAGNET=a(2)*2./3.1416*
C     +   ATAN(y*(a(3)**2+a(4)**2*exp(-(y**2)/2.*a(5))))
      RETURN
      END
