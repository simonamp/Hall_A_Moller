      SUBROUTINE RPRI_POL(LOUT)
C
C ===   Prits the scaler/polarization information from the COMMON 
C
C      Input: 
C             LOUT = logical unit to print (6, or >50)
C
      IMPLICIT NONE
      INTEGER  LOUT
C
      INCLUDE 'inc/crunpol.inc'
C
      INTEGER i,j,lun,k
      REAL    scagat(MXSCAL),scaerr(MXSCAL)
      CHARACTER  cnam(20)*16,cnam1(16)*16,cnam2(10)*16
      DATA       cnam/'Left  Arm       '
     +               ,'Right Arm       '
     +               ,'Coincidence     '
     +               ,'Accidentals     '
     +               ,'BCM             '
     +               ,'Clock           '
     +               ,'LED             '
     +            ,13*'XXXX            '/
      DATA      cnam1/'Left  Arm       '
     +               ,'Right Arm       '
     +               ,'Coincidence     '
     +               ,'Accidentals     '
     +               ,'BCM             '
     +               ,'Clock           '
     +               ,'Left  Arm   sc2 '
     +               ,'Right Arm   sc2 '
     +               ,'Coincidence sc2 '
     +               ,'Accidentals sc2 '
     +             ,6*'XXXX            '/
      DATA      cnam2/'Coincidence     '
     +               ,'Left  Arm       '
     +               ,'Right Arm       '
     +               ,'Coincidence sc2 '
     +               ,'Left  Arm   sc2 '
     +               ,'Right Arm   sc2 '
     +             ,4*'XXXX            '/
C
C     ------------------------------------------------------------------
C
C
      lun=LOUT
      IF(lun.NE.6.AND.lun.LE.50) lun=6
C
      WRITE(lun,1000) IRUNR
 1000 FORMAT(' === Scaler  data: RUN=',I5)
      IF(IRUNR.EQ.0.OR.NCOUNTR.EQ.0.OR.KCOUNTR(1)+KCOUNTR(2).EQ.0) 
     +                                                       GO TO 999
C
      WRITE(lun,1100) (i,i=1,12)
 1100 FORMAT(5X,'Cycles: H+ All  H-     H+ Used H-   Lost:'
     +      ,12I5)
      WRITE(lun,1200) (KCOUNTR(i),i=1,NCOUNTR)
 1200 FORMAT(11X,I4,3X,I4,5X,I4,3X,I4,8X,12I5,6I9)
C
      DO i=1,MXSCAL
         scagat(i)=GATE
C         IF(i.GT.6) scagat(i)=1.
         scaerr(i)=0.
         IF(SCALER(1,i,2).GT.0.) scaerr(i)=SCALER(2,i,2)/SCALER(1,i,2)
      ENDDO
C      WRITE(lun,FMT='(8F14.5)') SCALER
      WRITE(lun,1300) 
      WRITE(lun,1400) (cnam(i),(SCALER(j,i,1),j=1,2)
     +            ,scagat(i)*SCALER(1,i,1),scaerr(i),i=1,NSCALER)
 1300 FORMAT(/8X,'Scaler',9X,'<rate/sec> +/- RMS'
     +      ,4X,'<rate/gate>',2X,'Relat.error')
 1400 FORMAT(5X,A16,1X,F9.1,' +/- ',F8.1,2X,F9.1,3X,F8.5)
C
      WRITE(lun,1500) 
      DO i=1,NASYMR
         WRITE(lun,1600) cnam1(i),((ASYMR(j,i,k),j=1,2),k=1,4)
      ENDDO
 1500 FORMAT(/8X,'Scaler: asymmetries',3X,'sum normalized',10X
     +      ,'cycle normalized',12X,'no BG')
 1600 FORMAT(5X,A16,1X,4(F10.6,' +/-',F9.6,3X))

      WRITE(lun,1700) 
      DO i=1,NPOLR
         WRITE(lun,1800) cnam2(i),(POLR(j,i),j=1,2)
      ENDDO
 1700 FORMAT(/8X,'Scaler: ',12X,'Polarization')
 1800 FORMAT(5X,A16,1X,4(F10.6,' +/-',F9.6,3X))
C
C      WRITE(lun,1300) ((POLR(j,i),j=1,2),i=1,NPOLR)
C
 999  CONTINUE
      END
