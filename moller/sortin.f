      SUBROUTINE SORTIN(IA,N)
C
C===      Sorts ian integer array IA of length N
C
      INTEGER IA(N),N
      INTEGER i1,i2,ia1
C
      IF(N.LE.0) GO TO 999
C
      DO i1=1,N-1
         DO i2=i1+1,N
            IF(IA(i2).LT.IA(i1)) THEN
               ia1=IA(i1)
               IA(i1)=IA(i2)
               IA(i2)=ia1
            ENDIF
         ENDDO
      ENDDO
C
 999  CONTINUE
      END
