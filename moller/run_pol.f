      SUBROUTINE RUN_POL(IRUNIN,INFO)
C
C---     Copies the run polarization data from a vector to the COMMON 
C
      IMPLICIT NONE
C
      INTEGER IRUNIN,INFO
C
C
C===      Counting rates/polarizations for a given run
C
      INCLUDE 'inc/crunpol.inc'
C      INCLUDE 'inc/cruninf.inc'
C
      INCLUDE 'inc/v_run.inc'
C
      VECTOR KTAB(25)
C
      INTEGER i,ir
C
C     ------------------------------------------------------------------
C
      IRUN=IRUNIN
      IF(IRUN.LT.1.OR.IRUN.GT.MXKRUN) THEN
         WRITE(6,*) '  RUN_POL: wrong RUN=',IRUN
         GO TO 999
      ENDIF
      ir=KRUNPNT(IRUN)
      IF(ir.LE.0.OR.ir.GT.MXRRUN) THEN
         WRITE(6,*) '  RUN_POL: wrong address for RESRUN',ir
         GO TO 999
      ENDIF
C
      NCOUNTR=6
      DO i=1,NCOUNTR
         COUNTR(i)=RESRUN(KTAB(1+i),ir)
      END DO
C
      NPOLR=3
      POLR(1) =RESRUN(KTAB(10),ir)
      EPOLR(1)=RESRUN(KTAB(11),ir)
      POLR(2) =RESRUN(KTAB(15),ir)
      EPOLR(2)=0.
      POLR(3) =RESRUN(KTAB(16),ir)
      EPOLR(3)=0.
C
      DO i=1,2
         ASYMR(i)=RESRUN(KTAB(7+i),ir)
      END DO
C
      DO i=1,2
         BASYMR(i)=RESRUN(KTAB(16+i),ir)
      END DO
C
      NPOLPAR=4
      DO i=1,NPOLPAR
         POLPAR(i)=RESRUN(KTAB(11+i),ir)
      END DO
      POLPAR(4)=0.
C
      NADDVR=0
C
C
      WRITE(6,FMT='(A6,I6)') 'IRUN=',IRUN
C      WRITE(6,FMT='(10F12.1)') (COUNTR(i),i=1,NCOUNTR)
 999  CONTINUE
      END









