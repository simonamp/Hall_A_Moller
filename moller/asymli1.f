      REAL FUNCTION ASYMLI1(ID)
C
C===     Find the limits on the counting rates in order to set histogram limits 
C===     for the Asymmetry tasks
C
      IMPLICIT NONE
      INTEGER ID
      INCLUDE ?
C
      VECTOR LIMSCA(2,16)
C
      INTEGER jsca(64),jdif(64)
C
      INTEGER ievv,nsc,i
C
      DATA jsca/64*0/
      DATA ievv/0/
C
      ASYMLI1=1.
      nsc=MIN(NSCA,16)
C      write(6,*) 'nsc=',nsc
      GO TO 999
C
      IF(NSCA.LE.0) GO TO 999
C
      ievv=ievv+1
C      IF(ievv.EQ.1) THEN
C         DO i=1,nsc
C            LIMSCA(1,i)=9999999
C            LIMSCA(2,i)=0
C         END DO
C      ENDIF
C
      DO i=1,nsc
         jdif(i)=ISCA(i)-jsca(i) 
      END DO
C
C      IF(ievv.GE.5.AND.
C     +   jdif(3).GT.20.AND.jdif(12).GT.10.AND.
C     +   jdif(11).EQ.1) THEN
C         DO i=1,nsc
C            IF(jdif(i).GE.0.AND.jdif(i).LT.200000) THEN
C               LIMSCA(1,i)=MIN(LIMSCA(1,i),jdif(i))
C               LIMSCA(2,i)=MAX(LIMSCA(2,i),jdif(i))
C            ENDIF
C         END DO
CC         LIMSCA(1,1)=MIN(LIMSCA(1,1),LIMSCA(1,2))
CC         LIMSCA(2,1)=MAX(LIMSCA(1,1),LIMSCA(1,2))
CC         DO i=1,2
CC            LIMSCA(i,2)=LIMSCA(i,1)
CC         END DO
C      ENDIF
CC
C      DO i=1,nsc
C         jsca(i)=ISCA(i)
C      END DO
C
C
 999  CONTINUE
      RETURN
      END
