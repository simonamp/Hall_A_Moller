      SUBROUTINE STR_TRIM(STR,N1,N2)
C
C---        Trim the string.
C        INPUT: STR
C       OUTPUT: N1,N2 - 1-st and last non-blank characters in STR
C
      IMPLICIT NONE
      CHARACTER STR*(*)
      INTEGER N1,N2
C
      INTEGER lstr,i
C
      lstr=LEN(STR)
      N1=0
      N2=0
C      write(6,*) 'n1,n2=',N1,N2,lstr
      DO i=1,lstr
         IF(STR(i:i).NE.' ') THEN
            IF(N1.EQ.0) N1=i
            N2=i
         ENDIF
      ENDDO
C
      RETURN
      END
