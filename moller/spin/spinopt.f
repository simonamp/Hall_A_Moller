      REAL FUNCTION SPINOPT(X)
C
C===   Returns the longitudinal polarizations squared for 3 halls
C===    X - Wien angle
C
      IMPLICIT NONE
      REAL X
      REAL SPINPRED
C
      VECTOR VPREC(8)
      VECTOR NPASS(3)
C
      INTEGER i
      REAL res,pol
C
      res=0.
      DO i=1,3
         VPREC(2)=i
         VPREC(3)=NPASS(i)
         pol=SPINPRED(X)
         res=res+pol**2
      ENDDO
C
      SPINOPT=res
      RETURN
      END
C
      INCLUDE 'spinpred.f'
