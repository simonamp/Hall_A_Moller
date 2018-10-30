 REAL FUNCTION AVPT(X)

      IMPLICIT NONE
      REAL X
      VECTOR NPT(16)
      VECTOR T5p(16)
      VECTOR T4p(16)
      VECTOR T3p(16)
      VECTOR T2p(16)
C
      INTEGER i
C
	DO i=1,16
	IF (ntp(i).LT.2) THEN avpt(i)=100.
	ELSE 
	avpt(i)=(T5p(i)+T4p(i)+T3p(i)+T2p(i))/npt(i)
	ENDIF
	ENDDO
C
      END
