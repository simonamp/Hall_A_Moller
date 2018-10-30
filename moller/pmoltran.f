      REAL FUNCTION PMOLTRAN(THETB)
      IMPLICIT NONE
C
C ===     Returns the value of asymmetry of the asymmetries measured at 2 nearly
C ===     complimentary target anges
C
      REAL THETB   !  beam spin angle (deg)
C
      VECTOR PMTRAN(10) 
C                    1  - beam   polarization
C                    2  - target polarization
C                    3  - 1-st target angle (deg)
C                    4  - 2-nd target angle (deg)
C                    5  - Analyzing power parallel
C                    6  - Analyzing power perp
C                    7  - 1-st target angle, used (deg)
C                    8  - 2-nd target angle, used (deg)
C
      REAL thb,tht(2),pobs(2),a,anp(2),pol(2),thtu(2)
      INTEGER i,j
C
      thb=THETB*3.1415/180.
      a=PMTRAN(1)*PMTRAN(2)
      DO j=1,2
         anp(j)=PMTRAN(4+j)
      ENDDO
      DO i=1,2
         tht(i)=PMTRAN(2+i)*3.1415/180.
         thtu(i)=PMTRAN(6+i)*3.1415/180.
         anp(i)=PMTRAN(4+i)
         pobs(i)=a*
     +     (anp(1)*COS(thb)*COS(tht(i))+anp(2)*SIN(thb)*SIN(tht(i)))
         pol(i)=pobs(i)/a/COS(thtu(i))/anp(1)
      ENDDO
      PMOLTRAN=pobs(1)-pobs(2)
      IF(pobs(1)+pobs(2).NE.0.) THEN
         PMOLTRAN=(pol(1)+pol(2))/2.-cos(thb)
      ENDIF
C      PMOLTRAN=pol(1)
      END
