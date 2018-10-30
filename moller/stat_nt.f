      REAL FUNCTION STAT_NT(IR,R)
C
C===   Fills NTUPLE statistics (number of entries)
C
C ==   IR    - integer variable (optional) for example IRUN
C ==   R     - real    variable (optional) for example IRUN
C
C ==    OUTPUT:
C ==   NSTAT(1) - number of entries
C
      IMPLICIT NONE
      INTEGER IR
      REAL     R
      INCLUDE ?
C
      INCLUDE 'inc/v_evsel.inc'
C
      VECTOR NTSTAT(10)
      INTEGER ientry
      DATA    ientry/0/
C
C     ------------------------------------------------------------------
C
      ientry=ientry+1
C
      NTSTAT(1)=ientry
      STAT_NT=1.
C
      IF(ientry.GT.0.AND.ientry.LE.MXEVNT) THEN
         IEVSEL(ientry)=IDNEVT
         INTVAL(ientry)=IR
         RNTVAL(ientry)=R
      ENDIF
C
C      write(6,*) 'ev=',IDNEVT,IRUN,IR
      RETURN
      END
