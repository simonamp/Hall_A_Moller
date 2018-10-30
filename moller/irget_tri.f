      INTEGER FUNCTION IRGET_TRI(IRUN,LUN)
C
C ===   Writes the ADC peak positions to the COMMON and to a RZ file 
C
C     Input: IRUN - run number
C            LUN  - logical unit
C     Output: = 0 - not found
C             > 0 = icycle - OK
C
      IMPLICIT NONE
      INTEGER IRUN,LUN
C
      INTEGER       IQUEST
      COMMON/QUEST/ IQUEST(100)
C
      INCLUDE 'inc/cruntri.inc'
C
      INTEGER IR_CDIR
      INTEGER IR_EXIST
C
      INTEGER namp,i,j,iret,key(3),nrec,icycle,nramp
C
      INTEGER    mxbuf
      PARAMETER (mxbuf=500)
      REAL       buf(mxbuf)
C
C     ------------------------------------------------------------------
C
C
      IRGET_TRI=0
C
      IRUNT=0
      NADCMX=0
      NADCT=0
C      
C---    Go to the proper directory
C
      IF(IR_CDIR(LUN,'RUNS').EQ.0) GO TO 999
C
C---    Get a proper record
C
      key(1)=IRUN
      CALL UCTOH('TRIG',key(2),4,4)
      CALL UCTOH('R',key(3),4,1)
      icycle=99999
C
      IF(IR_EXIST(key,3,icycle).EQ.0) GO TO 999
C      
      CALL RZVIN(buf,mxbuf,nrec,key(1),icycle,'CD')
      icycle=IQUEST(6)
C
      IRUNT=IRUN
      NADCMX=MXAMPL
      NADCT=INT(buf(2)+0.1)
      nramp=INT(buf(1)+0.1)
C
      DO i=1,MXAMPL
         DO j=1,3
            PEAKADC(j,i)=0
            EPEAKADC(j,i)=0
            IF(i.LE.nramp) THEN
               PEAKADC(j,i)=buf(2+j+3*(i-1))
               EPEAKADC(j,i)=buf(2+j+3*(i-1)+3*nramp)
            ENDIF
         ENDDO
      ENDDO
C
      IRGET_TRI=icycle
C
 999  CONTINUE
      END
C
      INCLUDE 'ir_cdir.f'
      INCLUDE 'ir_exist.f'




