      REAL FUNCTION ASYMMET(ID,NEVADC)
C
C===     Asymmetry/polarization calculation
C
      IMPLICIT NONE
      INTEGER ID,NEVADC
C      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
      DOUBLE PRECISION da(16),df(2),dsum2(16)
C
      VECTOR IDD0(1)
      VECTOR ICHIS(10)
      VECTOR NNLIM(2)
      VECTOR CASYM(50),EASYM(50),NASYM(50)
C
      IMPLICIT NONE
C
      INTEGER jsca(64),nsum(64),jdif(64)
C
      INTEGER id0
      INTEGER ievv,ieva,ihelic,ierr,ierr1,i,nsc,iflip,nflip,k,jinc
     +       ,jhel,ncyclusf,ierr2,itick1,itick2,iticksc,iticksc0
      REAL asym,err,fac,pol,epol
C
      DATA ievv/0/
      DATA ieva/0/
      DATA ihelic/-1/
      DATA jsca/64*0/
      DATA nsum/64*0/
      DATA ierr/0/
      DATA nflip/0/
      DATA iticksc0/0/
      DATA ncyclusf/0/
      DATA dsum2/16*0.D0/
C
C
      nsc=MIN(NSCA,16)
      id0=IDD0(1)
      ASYMMET=1.
C
      IF(ITRIG(1).NE.0) THEN
         ieva=ieva+1
         IF(ieva.EQ.1) THEN
            ihelic=ITRIG(6)
            itick1=ITICK
            itick2=ITICK
         ELSE
            itick2=ITICK
            IF(ihelic.NE.ITRIG(6).AND.ierr.EQ.0) THEN
               ierr=1
               NASYM(3)=NASYM(3)+1
C               WRITE(6,*) ' Hilicity flip at ev=',idnevt,ieva,ievv
C     +              ,ihelic,ITRIG(6)
            ENDIF
         ENDIF
      ENDIF
C
      IF(NSCA.LE.0) GO TO 999
      
      ievv=ievv+1
      NASYM(1)=ievv
C
      iflip=0
      ierr1=0
C
      iticksc=ITICK
      IF(iticksc0.NE.0) CALL HF1(ID+60,iticksc-iticksc0+.1,1.)
C
      IF(ievv.LT.5) ierr1=1
      DO i=1,nsc
         IF(ISCA(i).LT.jsca(i)) ierr1=2
      END DO
      IF(ierr1.EQ.2) NASYM(4)=NASYM(4)+1
      IF(ierr1.EQ.0.AND.
     +     ISCA(11)-jsca(11).NE.1) THEN
         ierr1=1
         NASYM(5)=NASYM(5)+1
      ENDIF
      IF(ierr1.EQ.0.AND.
     +   ISCA(3)-jsca(3).LT.1.AND.
     +   ISCA(8)-jsca(8).LT.1) THEN
         ierr1=1
         NASYM(6)=NASYM(6)+1
      ENDIF
      IF(ierr1.EQ.0.AND.
     +     ieva.NE.NEVADC.AND.NEVADC.GE.0) THEN
         ierr1=1
         NASYM(7)=NASYM(7)+1
      ENDIF
C
      IF(ierr1.EQ.0.AND.ieva.GT.0.AND.ihelic.NE.ITRIG(6)) THEN
         ierr1=1
         NASYM(8)=NASYM(8)+1
         WRITE(6,*) ' Helicity flip at ev',idnevt,ievv,ihelic,ITRIG(6)
      ENDIF
      ihelic=-1
C
C---     Check the helicity scheme: H+ should be opposite to ITRIG(6)
C
      IF(ierr1.EQ.0) THEN
         IF(ISCA(7)-jsca(7).GT.1.AND.
     +      ISCA(2)-jsca(2).GT.1) THEN
            ierr1=1
            NASYM(9)=NASYM(9)+1
            WRITE(6,*) ' *** Error: both windows are incremented '
         ENDIF
         IF(ISCA(2)+ISCA(7).LT.10) THEN
            ierr1=1
            NASYM(10)=NASYM(10)+1
         ENDIF
      ENDIF
C
      IF(ierr1.EQ.0) THEN
         IF(ITRIG(6).EQ.0.AND.ISCA(7)-jsca(7).GT.1.OR.
     +      ITRIG(6).EQ.1.AND.ISCA(2)-jsca(2).GT.1) THEN
            iflip=1
            nflip=nflip+1
         ENDIF
      ENDIF
C
      IF(ierr1.EQ.0) THEN
         jinc=0
         IF(ISCA(2)-jsca(2).EQ.0) jinc=5
         jhel=0
         IF(ITRIG(6).EQ.1) jhel=5
         NASYM(13+jhel/5)=NASYM(13+jhel/5)+1
C         jinc=0
C         jhel=0
C         IF(ISCA(2)-jsca(2).EQ.0) THEN
C            jinc=5
C            jhel=5
C         ENDIF
      ENDIF
C
      IF(ierr1.GT.1) THEN
         WRITE(6,*) 'Error in difference calculation iev=',ievv,NSCA
         WRITE(6,1000) (i,jsca(i),ISCA(i),ISCA(i)-jsca(i),i=1,nsc)
 1000    FORMAT(' error ',I4,3I11)
      ENDIF
C
      IF(ierr.EQ.0.AND.ierr1.EQ.0.AND.
     +   ievv.GE.NNLIM(1).AND.ievv.LE.NNLIM(2)) THEN
         DO i=1,nsc
            jdif(i)=ISCA(i)-jsca(i) 
         END DO
         DO i=1,5
            nsum(i+jhel)=nsum(i+jhel)+jdif(i+jinc)
            dsum2(i)=dsum2(i)+(DBLE(jdif(i+jinc)))**2
C            WRITE(6,*) 'i,jdef ',i,jdif(i+jinc)
         END DO
C
         DO i=1,10
            k=ICHIS(i)
C            CALL HF1(ID+k,ALOG10(IABS(jdif(k))+0.1),1.)
C            IF(MOD(isca(11),2).EQ.1) THEN
               CALL HF1(ID+k,IABS(jdif(k))+0.1,1.)
               CALL HF1(ID+k+30,ievv+.1,REAL(jdif(k)))
               IF(k.LT.6) CALL HF1(ID+k+40,ievv+.1
     +                    ,REAL(jdif(k)+jdif(k+5)))
C            ENDIF
         END DO
      ENDIF
C
      CALL HF1(ID+20,ievv+0.1,REAL(ISCA(11)-jsca(11)))
      CALL HF1(ID+21,ievv+0.1,ieva+.1)
C      IF(ieva.NE.10) WRITE(6,*) 'event',IDNEVT,' adc event ',ieva
C
      DO i=1,nsc
         jsca(i)=ISCA(i)
      END DO
C
      iticksc0=iticksc
C     
      IF(ierr.NE.0) NASYM(11)=NASYM(11)+1
      ierr2=MAX(ierr,ierr1)
      ierr=ierr1

      ieva=0

      IF(ierr2.NE.0) GO TO 999
      IF(ievv.LT.NNLIM(1).OR.ievv.GT.NNLIM(2)) GO TO 999
C
      DO i=1,nsc
         da(i)=nsum(i)
      END DO
C
      ncyclusf=ncyclusf+1
      NASYM(2)=ncyclusf
      DO i=1,5
        CASYM(i)=(da(i)+da(i+5))/DBLE(ncyclusf)
        err=dsum2(i)/DBLE(ncyclusf)-CASYM(i)**2
        IF(err.LT.0.) THEN
           IF(i.NE.2) WRITE(6,2100) ievv,i,CASYM(i),err
 2100      FORMAT(' *** Error in calculating the error:',2I6,2F12.2)
           err=0.
        ENDIF
        err=SQRT(err)
        EASYM(i)=err
C        write(6,*) i,dsum2(i),CASYM(i),err,ncyclusf
      END DO 
C
      asym=0.
      IF(da(3).GT.0.D0.AND.
     +   da(8).GT.0.D0.AND.
     +   da(2).GT.0.D0.AND.
     +   da(7).GT.0.D0) THEN
C         df(1)=(da(1)-0)/da(2)
C         df(2)=(da(6)-0)/da(7)
         df(1)=(da(3)-da(4))/da(2)
         df(2)=(da(8)-da(9))/da(7)
         asym=REAL((df(1)-df(2))/(df(1)+df(2)))
         err=1./SQRT(REAL(da(3)+da(8)))
C         fac=1/0.0842/0.73/COS(35.*3.1415/180.)   ! cold supermendur -35 degree
         fac=1/0.0820/0.73/COS(45.*3.1415/180.)   ! room supermendur -35 degree
         pol=asym*fac
         epol=err*fac
         CASYM(21)=asym
         EASYM(21)=err
         CASYM(22)=pol
         EASYM(22)=epol
      ELSE
         WRITE(6,*) 'Error in asymmetry calculation iev=',ievv
         WRITE(6,1000) (i,jsca(i),ISCA(i),ISCA(i)-jsca(i),i=1,nsc)
         asym=0.
      ENDIF
      IF(ABS(asym).LT.2.) THEN
C         IF(MOD(ievv,2).EQ.0) THEN
           CALL HF1(ID,ievv+0.1,asym)
C           CALL HF1(ID,ievv-1+0.1,asym)
C         ENDIF
C         ASYMMET=asym
      ELSE
         WRITE(6,*) 'Error: asym=',asym
         WRITE(6,1000) (i,jsca(i),ISCA(i),ISCA(i)-jsca(i),i=1,nsc)
      ENDIF
C
      IF(MOD(ievv,500).EQ.0.AND.ierr.EQ.0) THEN
         IF(nflip.NE.0) THEN
            WRITE(6,*) ' *** Warning: Helicity sync. wave flip '
         ENDIF
         WRITE(6,2000) ievv,asym,err,pol,epol
 2000    FORMAT(' Cycle=',I6,'  Asymmetry=',F11.7,' +/- ',F11.7
     +          ,5X,'Polarization=',F8.4,' +/-',F8.4)
C         WRITE(6,*) 'NASYM',NASYM(13),NASYM(14)
      ENDIF
C      ASYMMET=ievv
C
 999  CONTINUE
      END
