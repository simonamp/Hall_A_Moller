      SUBROUTINE AV_LRARMS(IRUN1,IRUN2,ICH1,ICH2)
C
C===     Take averages of the corrected asymmetry of channels ICH1-ICH2
C         for runs IRUN1-IRUN2
C===     The data are taken from a vector RESRUN
C
      IMPLICIT NONE
      INTEGER IRUN1,IRUN2,ICH1,ICH2
      INCLUDE 'inc/v_run.inc'
C
      INTEGER i,j,irun,ir,lun(2),ilun,il
      INTEGER mxres
      PARAMETER (mxres=MXRESW/4)
C
      INTEGER ich
      DOUBLE PRECISION dav,der,dw,dcl
C
      lun(1)=6
C
      IF(IRUN1.GT.IRUN2.OR.IRUN1.LT.1.OR.IRUN2.GT.MXKRUN) THEN
         WRITE(6,*) ' *** Error: the input runs are out of '
     +        ,'range:',IRUN1,IRUN2
         GO TO 999
      ENDIF
C
      IF(ICH1.GT.ICH2.OR.ICH1.LT.1.OR.ICH2.GT.12) THEN
         WRITE(6,*) ' *** Error: the input channels are out of '
     +        ,'range:',ICH1,ICH2
         GO TO 999
      ENDIF
      
C
      OPEN(UNIT=1,FILE='runs.tab',STATUS='UNKNOWN')
C
      DO il=1,1
         ilun=lun(il)
         dav=0.D0
         der=0.D0
         DO irun=IRUN1,IRUN2
            ir=KRUNPNT(irun)
            IF(ir.GT.0) THEN
               dcl=1.
               IF(CONSRUN(6,ir).GT.0.5) dcl=-1.
               DO ich=ICH1,ICH2
                  dw=ERESRUN(ich+2*mxres,ir)
                  dw=dw**2
                  dav=dav+RESRUN(ich+2*mxres,ir)/dw*dcl
                  der=der+1.D0/dw
C                  write(6,*) irun,ich,'  ',RESRUN(ich+2*mxres,ir)
C     +                        ,ERESRUN(ich+2*mxres,ir)
              ENDDO
            ENDIF
         ENDDO
         dav=dav/der
         der=1.D0/DSQRT(der)
C
      ENDDO
      WRITE(6,1000) dav,der
 1000 FORMAT(' Asymmetry: ',E14.4,' +/- ',E14.4)
C
 999  CONTINUE
      END



