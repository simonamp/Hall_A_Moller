 REAL FUNCTION AVSC(X)

      IMPLICIT NONE
      REAL X
      VECTOR NSC(16)
      VECTOR T5sc(16)
      VECTOR T4sc(16)
      VECTOR T3sc(16)
      VECTOR T2sc(16)
C
      INTEGER i
C
	DO i=1,16
	IF (nsc(i).LT.2) THEN avsc(i)=100.
	ELSE 
	avsc(i)=(T5sc(i)+T4sc(i)+T3sc(i)+T2sc(i))/nsc(i)
	ENDIF
	ENDDO
C
      END
