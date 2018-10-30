      REAL FUNCTION A_INCREM(X)
C
C ===  Increments of a variable X, between the calls to this routine
C
      IMPLICIT NONE
      REAL X
C
      REAL x0,dx
      DATA x0/-9.E25/
C
      dx=0.
      IF(x0.GT.-8.9E25) THEN
C         write(6,*) x0,X
         dx=X-x0
      ENDIF
      x0=X
C
      A_INCREM=dx
C
 999  RETURN
      END

