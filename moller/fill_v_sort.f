      REAL FUNCTION FILL_V_SORT(X,IFL)
C  NOT FINISHED!
C ---       Fill a vector VTMP with the entries, in the increasing order
C           X - variable to fill
C           IFL=1 - fill, =0 - reset
C
      IMPLICIT NONE
      REAL X
      INTEGER IFL,NDIM
C
      VECTOR VTMP(1000)
C
      INTEGER i,j,mxv,nent
      REAL a,x0
      DATA nent/0/
      DATA x0/0./
C
      mxv=1000
      IF(IFL.EQ.0) THEN
         nent=0
         DO i=1,mxv
            VTMP=0.
         ENDDO
      ELSE
         IF(nent.GT.0) THEN
            IF(ABS(X-x0).LT.E-6) GO TO 999
         ENDIF
      ENDIF
C
 999  FILL_V_SORT=nent
      END

      
