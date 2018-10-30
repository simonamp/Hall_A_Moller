      REAL FUNCTION A_ANALYSE(ID0,IFL)
C
C ===  Analyze the SCALER ntuple (one quad per entry)
C ==   Fills: rates and asymmetries
C ==    ID0  > 0   - starting histogram number 
C ==    IFL  = 0
C         ID+i     - rates/33 ms window, 
C         IF+i+100 - asymmetries
C
      IMPLICIT NONE
      INTEGER ID,IFL
      INCLUDE ?
C
      LOGICAL  HEXIST
C
      INTEGER mxscal,mxasym
      PARAMETER (mxscal=17,mxasym=5)
      INTEGER isc,ksc(mxscal),ias,kas(mxasym),id
C
      DATA ifirst/1/
C
      A_ANALYSE=1.
      IF(ID0.LT.0) THEN
         WRITE(6,*) ' A_ANALYSE : ID is out of range ',ID
         GO TO 999
      ENDIF
C
      IF(ifirst.NE.0) THEN
         ifirst=0
         DO isc=1,mxscal
            id=ID0+i
            IF(HEXIST(id)) CALL HDELET(id)
      ENDIF
C
      IF(IFZUG.NE.0) GO TO 999  ! remove faulty trains (quads)
      IF(NELEM.LT.2) GO TO 999  ! at least
      IF(IVAR.LT.1.OR.IVAR.GT.NSCAL) THEN
         WRITE(6,*) ' *** Error: zug=',IZUG,' IVAR=',IVAR
     +              ,' is out of range ',1,NSCAL
         GO TO 999
      ENDIF
C
C
 999  CONTINUE
      END

