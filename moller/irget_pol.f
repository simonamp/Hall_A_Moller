      INTEGER FUNCTION IRGET_POL(IRUN,LUN)
C
C ===   Reads the Polarization/Counting rates to the COMMON from a RZ file 
C
C ===   Input:  IRUN - run numbr
C               LUN  - of the RZ file
C ===   Output: = 0 - not done
C               > 0 = icycle - OK 
C
      IMPLICIT NONE
      INTEGER IRUN,LUN
C
      INTEGER       IQUEST
      COMMON/QUEST/ IQUEST(100)
C
      INCLUDE 'inc/crunpol.inc'
C
      INTEGER IR_CDIR,IR_EXIST
C
      INTEGER i,j,k,iret,key(3),nrec,icycle,ierr
C
C     ------------------------------------------------------------------
C
      IRGET_POL=0
C
      IRUNR=0
      NCOUNTR=0
      NSCALER=0
      NASYMR=0
      NPOLR=0
      NADDVR=0
C
      DO i=1,MXCNT
         KCOUNTR(i)=0
      ENDDO
      DO i=1,MXSCAL
         DO k=1,2
            SCALER(1,i,k)=0.
            SCALER(2,i,k)=0.
         ENDDO
      ENDDO
      DO i=1,MXPOL
         POLR(1,i)=0.
         POLR(2,i)=0.
      ENDDO
      DO i=1,MXASYM
         DO j=1,2
            DO k=1,4
               ASYMR(j,i,k)=0.
            ENDDO
         ENDDO
      ENDDO
      DO i=1,MXADD
         ADDVR(i)=0
      ENDDO
C
C---    Go to the proper directory
C
      IF(IR_CDIR(LUN,'RUNS').EQ.0) GO TO 999
C
C---    Get a proper record
C
      key(1)=IRUN
      CALL UCTOH('SCAL',key(2),4,4)
      CALL UCTOH('I',key(3),4,1)
      icycle=99999
C
      IF(IR_EXIST(key,3,icycle).EQ.0) GO TO 999
C      
      CALL RZVIN(IRUNR,MXINTE,nrec,key(1),icycle,'CD')
      icycle=IQUEST(6)
      
      CALL UCTOH('R',key(3),4,1)
      icycle=99999
C
      IF(IR_EXIST(key,3,icycle).EQ.0) GO TO 999
C      
      CALL RZVIN(SCALER,MXREAL,nrec,key(1),icycle,'CD')
      icycle=IQUEST(6)
      IRGET_POL=icycle
C
 999  CONTINUE
      END
C
      INCLUDE 'ir_cdir.f'
      INCLUDE 'ir_exist.f'
