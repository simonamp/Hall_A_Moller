      REAL FUNCTION A_MATCH(IVAR,IEL)
C
C ===  Return 1 if the results in 2 scalers match
C      IVAR - variable name, =0 - all first 4 variables
C      IEL  - element, =0 - all elements 
C
      IMPLICIT NONE
      INTEGER IVAR,IEL
      INCLUDE ?
      INTEGER iq,iq1,iq2,iv,iv1,iv2
      REAL var1,var2,varm,dv,toler,tol
C
      A_MATCH=0.
C
      toler=0.04
C
      iq1=1
      iq2=NELEM
      IF(IEL.GE.1.AND.IEL.LE.NELEM) THEN
         iq1=IEL
         iq2=iq1
      ENDIF
      iv1=1
      iv2=4
      IF(IVAR.GE.1.AND.IVAR.LE.4) THEN
         iv1=IVAR
         iv2=iv1
      ENDIF
C
      DO iq=iq1,iq2
         DO iv=iv1,iv2
            var1=JCNT(iv,iq)
            var2=JCNT(iv+16,iq)
            varm=MAX(ABS(var1),ABS(var2))
            dv=ABS(var1-var2)
            tol=varm*toler
            IF(iv.EQ.4) THEN
               tol=MAX(ABS(JCNT(3,iq)),ABS(JCNT(3+16,iq)))*toler
            ENDIF
            tol=MAX(tol,4.)
            IF(dv.GT.tol) GO TO 999
         ENDDO
      ENDDO
C
      A_MATCH=1.
C
 999  RETURN
C
      END

