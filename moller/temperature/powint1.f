      REAL FUNCTION POWINT1(X2)
C
C---     Integerate a function 
C
      IMPLICIT NONE
      REAL X1,X2
      INTEGER N
C
      REAL FUN1,SIMPSF
      EXTERNAL FUN1,SIMPSF
C 
      REAL s
C
      N=1000
      X1=0.
C
      s=SIMPSF(FUN1,X1,X2,N)
      POWINT1=s
C      WRITE(6,*) ' Integration X1,X2',X1,X2,s
C
      END
C
      REAL FUNCTION FUN1(X)
      IF(X.GT.0.001) THEN
         f=(1.-EXP(-X**2/2.))/X
      ELSE
         f=X**3/4.
      ENDIF
      FUN1=f
      END
C
      INCLUDE 'simpsf.f'
