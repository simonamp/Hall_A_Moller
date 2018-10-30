      INTEGER FUNCTION IRPUT_TRI(IRUN,NADC,LUN)
C
C ===   Writes the ADC peak positions to the COMMON and to a RZ file 
C
C ===   Output: = 0 - not done
C               > 0 = icycle - OK 
C
      IMPLICIT NONE
      INTEGER IRUN,NADC,LUN
C
      INTEGER       IQUEST
      COMMON/QUEST/ IQUEST(100)
C
      INCLUDE 'inc/v_lgfit.inc'
      INCLUDE 'inc/cruntri.inc'
C
      INTEGER IR_CDIR
C
      INTEGER i,j,iret,key(3),nrec,icycle
      REAL    buf(100)
C
C     ------------------------------------------------------------------
C
      IRPUT_TRI=0
C
      IRUNT=IRUN
      NADCMX=MXAMPL
      NADCT=NADC
C      WRITE(6,*) 'xxx',IRUNT,NADCMX,NADCT
C
      DO i=1,MXAMPL
         DO j=1,3
            PEAKADC(j,i)=0
            EPEAKADC(j,i)=0
            IF(i.LE.MXADC) THEN
               PEAKADC(j,i)=PARLG(j,i)
               EPEAKADC(j,i)=EPARLG(j,i)
            ENDIF
         ENDDO
      ENDDO
C
C---   Write to the RZ file
C
      IF(IR_CDIR(LUN,'RUNS').NE.0) THEN
         key(1)=IRUN
         CALL UCTOH('TRIG',key(2),4,4)
         CALL UCTOH('R',key(3),4,1)
         RADCMX=REAL(NADCMX)
         RADCT=REAL(NADCT)
         nrec=2+3*2*MXAMPL
C         WRITE(6,FMT='(3I12,3A12)') key,key,nrec
C         WRITE(6,*) RADCMX,RADCT
         icycle=0
C         CALL RZLDIR(' ',' ')
         CALL RZVOUT(RADCMX,nrec,key(1),icycle,' ')
         NADCMX=INT(RADCMX)
         NADCT=INT(RADCT)
         IF(IQUEST(1).NE.0) THEN
            WRITE(6,*) ' *** RPUT_TRI Error writing the KEY=',key(1)
C           WRITE(6,*) icycle,(IQUEST(i),i=1,20)
         ELSE
            IRPUT_TRI=icycle
         ENDIF
      ELSE
         WRITE(6,*) ' *** RPUT_TRI Error: no directory ',LUN,'RUNS'
      ENDIF
C
C      CALL RPRI_TRI(5770,6)
 999  CONTINUE
      END
C
      INCLUDE 'ir_cdir.f'
      INCLUDE 'rpri_tri.f'




