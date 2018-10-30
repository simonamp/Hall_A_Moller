      SUBROUTINE SIGNDEP
C
C---   Sign dependece of the polarization: 2 close runs
C
      INCLUDE ?
      IMPLICIT NONE
C
      VECTOR POLA(82)
C
      INTEGER i,iused
      REAL p1,pol,p2
C
      CALL HBOOK1(211,'Polarization plus-minus',100,-0.04,0.04,0.)
C
      p1=0.
      iused=0
      DO i=1,82
         pol=POLA(i)
         IF(pol*p1.LT.-0.05.AND.iused.EQ.0) THEN
            p2=pol+p1
C            IF(pol.LT.0.) p2=-p2
            CALL HF1(211,p2,1.)
            iused=1
         ELSE
            iused=0
         ENDIF
         WRITE(6,*) i,iused,p1
         p1=pol
      END DO
      END   
