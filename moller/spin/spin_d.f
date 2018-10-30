      SUBROUTINE SPIN_D(TH1,TH2,DT1,DT2,IH1,IH2,IP1,IP2,EPASS)
C
C===      Calculates the beam energy shift for a given angular difference
C         between 2 halls  
C===      Input: TH1      - the angle on the injector for polar=max in hall IH1
C                TH2      - the angle on the injector for polar=max in hall IH2
C                DT1      - the error of TH1 
C                DT2      - the error of TH2 
C                IH1      - the 1-st hall number
C                IH2      - the 2-nd hall number
C                IP1      - number of passes in hall IH1
C                IP2      - number of passes in hall IH2
C                EPASS    - the energy of one full pass
C
      IMPLICIT NONE
      REAL     TH1,TH2,DT1,DT2,EPASS
      INTEGER  IH1,IH2,IP1,IP2
C
      REAL     SPIN
      EXTERNAL SPIN
C
      VECTOR SPIND(4)
C
      REAL pi,th(2),ip(2),precpred(2),precdif(2),eshift(3),dt(2),p1
     +    ,deshift(3)
      INTEGER i,ih(2)
C
C     ------------------------------------------------------------------
C
      pi=ACOS(0.)*2.
C
      IF(IH1.LT.1.OR.IH1.GT.3.OR.IH2.LT.1.OR.IH2.GT.3.OR.
     +   IP1.LT.1.OR.IP1.GT.5.OR.IP2.LT.1.OR.IP2.GT.5.OR.
     +   EPASS.LT.0.1.OR.EPASS.GT.10.) THEN
         WRITE(6,*) ' SPIN_D: error in the input parameters '
         GO TO 999
      ENDIF
C
      th(1)=TH1
      th(2)=TH2
      dt(1)=DT1
      dt(2)=DT2
      ih(1)=IH1
      ih(2)=IH2
      ip(1)=IP1
      ip(2)=IP2
C
      SPIND(2)=EPASS/2.
      SPIND(4)=1.
C
      DO i=1,2
         SPIND(1)=REAL(ih(i))
         SPIND(3)=REAL(ip(i))
C
         precpred(i)=SPIN(0.)
         p1=precpred(i)+th(i)
         p1=p1-INT(p1/180.+0.5)*180.
C
C---         Difference between the measured precession and the expected one
C
         precdif(i)=-p1
         eshift(i)=precdif(i)/precpred(i)  ! energy shift based on one hall
         deshift(i)=dt(i)/precpred(i)  
C         
      END DO
C
C---      Energy shift based on 2 halls
C
      p1=precpred(1)-precpred(2)
      eshift(3)=0.
      deshift(3)=0.
      IF(ABS(p1).GT.0.) THEN
         eshift(3)=(precdif(1)-precdif(2))/p1
         deshift(3)=SQRT(dt(1)**2+dt(2)**2)/p1
      ENDIF
      WRITE(6,1000) (ih(i),precpred(i)/360.,precdif(i),dt(i),eshift(i)
     +             ,deshift(i),i=1,2)
 1000 FORMAT(' hall   spin_prec(/2pi) angle_sift(deg)      energy_shift'
     +      /(3X,I2,5X,F9.3,4X,F6.1,'+/-',F4.1,5X,F8.5,'+/-',F8.5))
      WRITE(6,1100) eshift(3),deshift(3)
 1100 FORMAT('  Energy shift from 2 halls: ',12X,F8.5,'+/-',F8.5)
C
 999  RETURN
      END
      INCLUDE 'spin.f'



