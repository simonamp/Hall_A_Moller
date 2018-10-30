      REAL FUNCTION FU_POL(X)
      REAL X
C
      VECTOR PARP(10)
      REAL p
      INTEGER i
C
      p=PARP(1)
      xx=1.
      DO i=2,4
         xx=xx*X
         p=p+xx*PARP(i)
      ENDDO
      FU_POL=p
C
      END
