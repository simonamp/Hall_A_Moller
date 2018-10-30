      SUBROUTINE ANPOW_RF(ID,NORM)
C
C===     Radiative Moller analyzing power
C===     Fills a 2-dim hist ID (theta, en in CM) of the analyzing power table 
C===     NORM=0 - an. power as read, =1 - normalized to the non-radiative an.power
C
      IMPLICIT NONE
      INTEGER ID,NORM
      LOGICAL HEXIST
C
      CHARACTER fnam*132
      INTEGER i,lun,nl,nth,nen
      REAL th,en,ap,en0,ep0,raddeg,thr,eb,ecm,am,gam,bet,ecm1,pcm1
      INTEGER    mxtab
      PARAMETER (mxtab=2000)
      REAL thet(mxtab),ener(mxtab),anpow(mxtab)
C
      eb=45.       ! beam energy
      am=0.511E-3  ! e- mass
      ecm=SQRT(2.*eb*am+2.*am**2)
      gam=(eb+am)/ecm
      bet=sqrt(1-1./gam**2)
      ecm1=ecm/2.
      pcm1=SQRT(ecm1**2-am**2)
C
      raddeg=ACOS(0.)/90.
      lun=1
      fnam='anpow_rad.tab'
      OPEN(UNIT=lun,FILE=fnam,STATUS='OLD',ERR=991)
      nl=0
 10   READ(lun,*,ERR=992,END=100) th,en,ap
      nl=nl+1
      IF(nl.LE.mxtab) THEN
         IF(NORM.EQ.1) THEN
            thr=th*raddeg
            ap0=SIN(thr)**2*(7.+COS(thr)**2)/(3.+COS(thr)**2)**2
            ap=ap/ap0
            en0=gam*(ecm1+pcm1*bet*COS(thr))
            en=en/en0
         ENDIF
         thet(nl)=th
         ener(nl)=en
         anpow(nl)=ap
      ELSE
         WRITE(6,*) ' *** Error: too many input lines: ',nl,mxtab
      ENDIF
      GO TO 10
      CLOSE(lun)
C
 100  nth=1
      nen=1
      th=thet(1)
      en=ener(1)
      DO i=1,nl
         IF(ABS(thet(i)-th).GT.1.E-3) THEN
            nth=nth+1
            th=thet(i)
         ENDIF
      ENDDO         
C
      WRITE(6,1100) (i,thet(i),ener(i),anpow(i),i=1,nl)
 1100 FORMAT(1X,I4,3X,F6.1,2X,F8.3,2X,F8.4)
C
      RETURN

 991  WRITE(6,*) ' *** Error opening file'
      RETURN
 992  WRITE(6,*) ' *** Error reading file ',nl
      RETURN
C
      END
      
      

