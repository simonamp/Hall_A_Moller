      REAL FUNCTION ASYMLIM(NOCUT)
C
C===     Find the limits on the counting rates in order to set histogram limits 
C===     for the Asymmetry tasks
C
      IMPLICIT NONE
      INTEGER NOCUT
      INCLUDE ?
C
      VECTOR LIMSCA(2,16)
C
      INTEGER jsca(64),jdif(64)
C
      INTEGER ievv,nsc,i,iticksc,iticksc0
C
      DATA jsca/64*0/
      DATA iticksc0/0/
      DATA ievv/0/
C
      ASYMLIM=1.
C      GO TO 999
      nsc=MIN(NSCA,16)
C
      IF(NSCA.LE.0) GO TO 999
C
      ievv=ievv+1
      IF(ievv.EQ.1) THEN
         DO i=1,nsc
            LIMSCA(1,i)=9999999
            LIMSCA(2,i)=0
         END DO
      ENDIF
C
      iticksc=ITICK
      DO i=1,nsc
         jdif(i)=ISCA(i)-jsca(i) 
      END DO
C
      IF(ievv.GE.5.AND.
     +   (NOCUT.NE.0.OR.
     +    (jdif(3).GT.20.AND.jdif(12).GT.10)).AND.
     +     IABS(iticksc-iticksc0-60).LE.5) THEN
         DO i=1,nsc
            IF(jdif(i).GE.0.AND.jdif(i).LT.4000000) THEN
               LIMSCA(1,i)=MIN(LIMSCA(1,i),jdif(i))
               LIMSCA(2,i)=MAX(LIMSCA(2,i),jdif(i))
C               IF(i.EQ.1) WRITE(6,*) i,jdif(i)
            ENDIF
         END DO
C         LIMSCA(1,1)=MIN(LIMSCA(1,1),LIMSCA(1,2))
C         LIMSCA(2,1)=MAX(LIMSCA(1,1),LIMSCA(1,2))
C         DO i=1,2
C            LIMSCA(i,2)=LIMSCA(i,1)
C         END DO
      ENDIF
      iticksc0=iticksc
C
      DO i=1,nsc
         jsca(i)=ISCA(i)
      END DO
C
C
 999  CONTINUE
      END




