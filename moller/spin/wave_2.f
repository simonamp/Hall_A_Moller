      REAL FUNCTION WAVE_2(X)
      
C
      IMPLICIT NONE
      REAL PARA
      COMMON/PAWPAR/ PARA(10)
      VECTOR PAR(10)
      VECTOR IPFIT(2)
      REAL period,apar(3)
      INTEGER i
C
      period=1.
      IF(IPFIT(1).EQ.0) THEN
         DO i=1,3
            apar(i)=PARA(i)
         ENDDO
      ELSE
         DO i=1,3
            apar(i)=PAR(i)
         ENDDO
      ENDIF
      IF(IPFIT(2).EQ.1) THEN
        period=PAR(3)
      ELSEIF(IPFIT(2).EQ.2) THEN
        period=PARA(3)
C        WRITE(6,*) 'period=',period
      ENDIF
C      PRINT *,X,PARA(1),PARA(2),PARA(3),period
      WAVE_2=apar(1)*COS((X-apar(2))*3.1415/180.*period)
C       write(6,*) x,para(1),para(2),period,wave_2
      END
