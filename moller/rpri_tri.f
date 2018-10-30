      SUBROUTINE RPRI_TRI(LOUT)
C
C ===   Prits the ADC peak positions from the COMMON 
C
C      Input: 
C             LOUT = logical unit to print (6, or >50)
C
      IMPLICIT NONE
      INTEGER  LOUT
C
      INCLUDE 'inc/cruntri.inc'
C
      INTEGER i,j,lun
C
C     ------------------------------------------------------------------
C
C
      lun=LOUT
      IF(lun.NE.6.AND.lun.LE.50) lun=6
C
      WRITE(lun,1000) IRUNT,NADCT
      IF(IRUNT.EQ.0.OR.NADCT.EQ.0) GO TO 999
C
      WRITE(lun,1100) 
      DO i=1,MIN(NADCT,MXAMPL)
         WRITE(lun,1200) i,(PEAKADC(j,i),EPEAKADC(j,i),j=1,3) 
      ENDDO
C
 999  CONTINUE
 1000 FORMAT(' === Trigger data: RUN=',I5,' number of peaks=',I2)
 1100 FORMAT(' # ',8X,'Hight (events)',5X,'Mean (chan)',9X
     +       ,'Width (chan)')
 1200 FORMAT(1X,I2,2X,F9.1,' +/- ',F5.1,2(2X,F7.1,' +/- ',F5.1))
      END




