      REAL FUNCTION CROSSEC(ID,NEV,THLIM,THAC)
      IMPLICIT NONE
      INTEGER  ID,NEV
      REAL     THLIM,THAC
C
C=== Calculates Moeller acceptance
C
C      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
C
      REAL RNDM
C
      INTEGER iev
      REAL th1,th2,cth1,cth2,cth,sth,sth2,asm,wgm,cthac,wgt,asym
C
      IF(ID.EQ.0) THEN
         WRITE(6,*) ' *** Error: ID=',ID
         GO TO 999
      ENDIF
C
C      CALL HBOOK1(ID,'Moeller cross-section'
C     +   ,100,90.-THLIM,90.+THLIM,0.)
      CALL HBOOK1(ID,'Moeller asymmetry',400,0.,1.,0.)
      CALL HBOOK1(ID+10,'Moeller across-sec',100,-1.,1.,0.)
C
      th1=(90.-THLIM)*3.1415/180.
      th2=(90.+THLIM)*3.1415/180.
      cth1=COS(th1)
      cth2=COS(th2)
      cthac=COS((90.-THLIM+THAC)*3.1415/180.)
      WRITE(6,*) 'cthac,cth1 ',cthac,cth1
C
      asm=0.
      wgm=0.
C
      DO iev=1,NEV
         cth=cth1+RNDM(iev)*(cth2-cth1)
         sth2=1.-cth**2
         sth=SQRT(sth2)
         wgt=((4.-sth2)/sth2)**2
         IF(ABS(cth).LT.0.) wgt=wgt*0.0
C
C---      Acceptance - linear drop
C
C ?
         IF(cthac.LT.cth1-.001) THEN
            IF(ABS(cth).GT.cthac) THEN
               wgt=wgt*(cth1-ABS(cth))/(cth1-cthac)
            ENDIF
         ENDIF
C
         asym=sth2*(8.-sth2)/(4.-sth2)**2
         CALL HF1(ID,asym,wgt)
         CALL HF1(ID+10,cth,wgt)
         asm=asm+asym*wgt
         wgm=wgm+wgt
      END DO
C
      CROSSEC=1.
C
 999  CONTINUE
      END



