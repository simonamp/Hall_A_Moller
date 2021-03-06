      SUBROUTINE PRUN10(IRUN1,IRUN2)
C
C===     Printout the Moller run results on the screen and also in a file runs.tab
C===     The data are taken from a vector RESRUN
C===     Input: the run range IRUN1:IRUN2
C
      IMPLICIT NONE
      INTEGER IRUN1,IRUN2
      INCLUDE 'inc/v_run.inc'
C
      INTEGER i,j,irun,ir,lun(2),ilun,il
      INTEGER mxres
      PARAMETER (mxres=MXRESW/4)
C
      lun(1)=6
      lun(2)=1
C
      IF(IRUN1.GT.IRUN2.OR.IRUN1.LT.1.OR.IRUN2.GT.MXKRUN) THEN
         WRITE(6,*) ' *** Error: the input runs are out of '
     +        ,'range:',IRUN1,IRUN2
         GO TO 999
      ENDIF
      
C
      OPEN(UNIT=1,FILE='runs.tab',STATUS='UNKNOWN')
C
      DO il=1,2
         ilun=lun(il) 

         WRITE(ilun,1000) '    Asym BCM     coil Factor'
 1000    FORMAT(/10X,'Moller measurements: rates/sec and asymmetries'/
     +        2X,'run    Left   Right Coinc. Accid.    BCM      Clock  '
     +          ,'    Cor.Asymm        Polarization    angl '
     +          ,' An.Pow Pol.Targ'
     +          ,' PolarizationL/R',A29)
C     +          ,' Asym BCM  coil')
         DO irun=IRUN1,IRUN2
            ir=KRUNPNT(irun)
            IF(ir.GT.0) THEN
               WRITE(ilun,1200) irun,(INT(RESRUN(i,ir)),i=1,3)
     +              ,(RESRUN(i,ir),i=4,5),INT(RESRUN(7,ir))
     +              , RESRUN(3+mxres*2,ir),ERESRUN(3+mxres*2,ir)
     +              , RESRUN(3+mxres*3,ir),ERESRUN(3+mxres*3,ir) 
     +              ,(CONSRUN(i,ir),i=1,3)
     +              , RESRUN(1+mxres*3,ir),RESRUN(2+mxres*3,ir) 
     +              , RESRUN(5+mxres*2,ir),ERESRUN(5+mxres*2,ir) 
     +              , CONSRUN(6,ir)
     +              , CONSRUN(9,ir)
C     +              ,(INT(RESRUN(i,ir)),i=13,14), INT(RESRUN(6,ir))
 1200          FORMAT(I5,2I8,I7,F7.0,F10.0,I8,1X
     +           ,F7.4,'+/-',F7.4,1X,F7.4,'+/-',F7.4
     +           ,1X,F6.2,F8.4,1X,F7.4
     +           ,1X,F7.4,1X,F7.4,1X,2F8.5,F6.1
C     +           ,2I8,I7)
     +           ,F7.3)
            ENDIF
         END DO
      END DO
C
      CLOSE(UNIT=1)
C
 999  CONTINUE
      END



