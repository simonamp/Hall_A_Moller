      REAL FUNCTION SCALER(ID,ID1)
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
C
      VECTOR IDD0(1)
C
      DOUBLE PRECISION dc(4),dr(2)
C
      INTEGER jsca(64)
      DATA ievv/0/
      DATA iasym/0/
      DATA jsca/64*0/
C
      id0=IDD0(1)
      SCALER=1.
C
      IF(NSCA.EQ.0) GO TO 999
      ievv=ievv+1
C
      ierr=0
      DO i=1,16
         IF(ISCA(i).LT.jsca(i)) ierr=1
      END DO
      CALL HF1(ID,ievv+0.1,REAL(ISCA(1)-jsca(1)))
C
      IF(ierr.NE.0) THEN
         WRITE(6,1000) (ievv,jsca(i),ISCA(i),ISCA(i)-jsca(i),i=1,NSCA)
 1000    FORMAT(' error ',I4,3I11)
      ENDIF
C
      DO i=1,NSCA
         jsca(i)=ISCA(i)
      END DO
C
      IF(ISCA(2).GT.0.AND.ISCA(7).GT.0) THEN
         IF(MOD(ievv,10).EQ.0) THEN
            DO i=1,2
              dc(i)=ISCA(i)
              dc(i+2)=ISCA(i+5)
            END DO
            dr(1)=dc(1)/dc(2)
            dr(2)=dc(3)/dc(4)
            asym=(dr(1)-dr(2))/(dr(1)+dr(2))
            iasym=iasym+1
            CALL HF1(ID1,iasym+0.1,asym)
         ENDIF
      ENDIF
C
 999  CONTINUE
      END
