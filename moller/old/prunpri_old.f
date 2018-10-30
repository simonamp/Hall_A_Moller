      SUBROUTINE PRUNPRI(IRUN1,IRUN2)
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
         WRITE(ilun,1000) 
 1000    FORMAT(/10X,'Moller measurements: rates/sec and asymmetries'/
     +        2X,'run    Left   Right Coinc. Accid.    BCM  Clock  '
     +          ,'    Raw.Asymm          Polarization     angl'
     +          ,' An.Pow Pol.Targ  '
     +          ,' Polarization L/R')
C     +          ,' Asym BCM  coil')
         DO irun=IRUN1,IRUN2
            ir=KRUNPNT(irun)
            IF(ir.GT.0) THEN
               WRITE(ilun,1200) irun,(INT(RESRUN(i,ir)),i=1,3)
     +              ,(RESRUN(i,ir),i=4,5),INT(RESRUN(6,ir))
     +              ,(RESRUN(i,ir),i=11,14)
     +              ,(RESRUN(i,ir),i=17,19)
     +              ,(RESRUN(i,ir),i=21,23,2)
     +              ,(RESRUN(i,ir),i=27,29)
 1200          FORMAT(1X,I4,2I8,I7,F7.0,F8.0,I7,2X
     +           ,F7.4,'+/-',F7.4,2X,F7.4,'+/-',F7.4
     +           ,2X,F6.1,F7.3,1X,F7.4
     +           ,2X,F7.4,2X,F7.4,1X,2F8.5,F5.1)
            ENDIF
         END DO
      END DO
C
      CLOSE(UNIT=1)
C
 999  CONTINUE
      END

