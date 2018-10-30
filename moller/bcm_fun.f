      REAL FUNCTION BCM_FUN(X)
C
C--- Returns the current (arbit. units), X=BCM signal
C---   Assymptotic curve      
C
      IMPLICIT NONE
      REAL X
      REAL PARA
      COMMON/PAWPAR/ PARA(10)
      VECTOR PAR(10)
      VECTOR IPFIT(2)
      REAL apar(3)
      INTEGER i
C
      write(6,*) (par(i),para(i),i=1,3)
      IF(IPFIT(1).EQ.0) THEN
         DO i=1,3
            apar(i)=PARA(i)
         ENDDO
      ELSE
         DO i=1,3
            apar(i)=PAR(i)
         ENDDO
      ENDIF
C
      BCM_FUN=apar(1)*X*(1-EXP(-apar(2)*(X-apar(3))))
C       write(6,*) x,para(1),para(2),period,wave_1
      END
