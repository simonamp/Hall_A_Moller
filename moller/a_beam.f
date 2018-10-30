      REAL FUNCTION A_BEAM(IEL)
C
C ===  Return the beam current for element IEL
C
      IMPLICIT NONE
      INTEGER IEL
      INCLUDE ?
C
      REAL offs,scal
C
      A_BEAM=0.
      IF(IEL.LT.1.OR.IEL.GT.NELEM) GO TO 999
C
      scal=1.
      offs=0.
      IF(izrun.LT.12805) THEN
         offs=0.
      ELSE IF(izrun.LT.14000) THEN
         offs=11.
      ELSE IF(izrun.GT.99999) THEN
         offs=0.  ! PREX
      ENDIF
C
      A_BEAM=(JCNT(5,IEL)-offs)*scal
C
 999  RETURN
C
      END

