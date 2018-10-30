      INTEGER FUNCTION LENSTR(STR)
C---    Returns the length of a string, trailing blanks excluded
      IMPLICIT NONE
      CHARACTER STR*(*)
C
      INTEGER l,i,lens
C
      lens=LEN(STR)
      l=lens
C
      DO i=1,lens
         IF(ICHAR(STR(i:i)).EQ.0) THEN
            l=i-1
            GO TO 10
         ENDIF
      ENDDO
C
 10   lens=l
C
      l=0
      DO i=lens,1,-1
         IF(ICHAR(STR(i:i)).GT.32) THEN
            l=i
            GO TO 20
         ENDIF
      ENDDO
C
 20   LENSTR=l
      END

