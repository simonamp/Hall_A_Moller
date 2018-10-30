      REAL FUNCTION CHNGPOL(P,ANGL1,ANGL2)
      IMPLICIT NONE
      REAL P,ANGL1,ANGL2
      REAL p1,pi1
C
      pi1=ACOS(0.)*2./180.
      p1=P*COS(ANGL1*pi1)/COS(ANGL2*pi1)
      WRITE(6,*) 'Pol=',p1
      CHNGPOL=p1
C
      END
