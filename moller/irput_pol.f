      INTEGER FUNCTION IRPUT_POL(IRUN,LUN)
C
C ===   Writes the olarization/Counting rates to the COMMON and to a RZ file 
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
      INCLUDE 'inc/v_asym.inc'
      INCLUDE 'inc/crunpol.inc'
C
      INTEGER IR_CDIR
C
      INTEGER i,j,k,iret,key(3),nrec,icycle,ierr,icyc1
C
C     ------------------------------------------------------------------
C
      IRPUT_POL=0
C
      IRUNR=IRUN
      NCOUNTR=0
      NSCALER=0
      NASYMR=0
      NPOLR=0
      NADDVR=0
C
C===     Copy the vector of parameters to the CRUNPOL COMMON
C
      INCLUDE 'inc/v_asym_a.inc'
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
      IF(CSCAL(1,1).LT.1.) THEN
         WRITE(6,*) 'Polarization: no data'
         GO TO 999
      ENDIF
C
      IF(GATE.LT.0.0001) THEN
         WRITE(6,*) 'The gate is wrong ',gate
         GO TO 999
      ENDIF
C
      NSCALER=7
      DO i=1,NSCALER
         DO k=1,2
            SCALER(1,i,k)=CSCAL(i,k)
            SCALER(2,i,k)=ESCAL(i,k)
         ENDDO
      ENDDO
C
      NCOUNTR=16
      DO i=1,2
         KCOUNTR(i  )=NASYM(i)
         KCOUNTR(i+2)=NASYM(i+15)
      ENDDO
      DO i=1,NCOUNTR-2
         KCOUNTR(i+4)=NASYM(i+2)
      ENDDO
C
      NASYMR=10
      DO i=1,NASYMR
         DO k=1,4
            ASYMR(1,i,k)=CASYM(i,k)
            ASYMR(2,i,k)=EASYM(i,k)
         ENDDO
      ENDDO
C     
      NPOLR=6
      DO i=1,NPOLR
        POLR(1,i)=CPOLA(i,1)
        POLR(2,i)=EPOLA(i,1)
      ENDDO
C
C===     Calculate the rates/sec
C
      
C
C---   Write to the RZ file
C
      IF(IR_CDIR(LUN,'RUNS').NE.0) THEN
         key(1)=IRUN
         CALL UCTOH('SCAL',key(2),4,4)
         ierr=0
         icycle=0
         CALL UCTOH('I',key(3),4,1)
         nrec=MXINTE
C            CALL RZLDIR(' ',' ')
         CALL RZVOUT(IRUNR,nrec,key(1),icycle,' ')
         IF(IQUEST(1).NE.0) THEN
            WRITE(6,*) ' *** RPUT_TRI Error writing the KEY=',key
C           WRITE(6,*) icycle,(IQUEST(i),i=1,20)
            ierr=1
         ENDIF
         CALL UCTOH('R',key(3),4,1)
         nrec=MXREAL
         icycle=0
         CALL RZVOUT(SCALER,nrec,key(1),icycle,' ')
         IF(IQUEST(1).NE.0) THEN
            WRITE(6,*) ' *** RPUT_TRI Error writing the KEY=',key
C           WRITE(6,*) icycle,(IQUEST(i),i=1,20)
            ierr=1
         ENDIF
         IF(ierr.EQ.0) IRPUT_POL=icycle
C
      ELSE
         WRITE(6,*) ' *** RPUT_TRI Error: no directory ',LUN,'RUNS'
      ENDIF
C
      CALL RPRI_POL(6)
 999  CONTINUE
      END
C
      INCLUDE 'ir_cdir.f'
      INCLUDE 'rpri_pol.f'




