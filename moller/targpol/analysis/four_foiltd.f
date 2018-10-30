      PROGRAM FOUR_FOIL
C
C --- Fourier analysis of the foil magnetic spectrum
C
      IMPLICIT NONE
      INTEGER    mxp,mxps,mxf
      PARAMETER (mxp=80000,mxps=80000,mxf=10000)
      REAL vin(mxp),fk(mxf)
C      DOUBLE PRECISION dv1(mxps),dv2(mxps),dfourk(0:mxf,2)
      DOUBLE PRECISION dpi,dbin,df,dx,dom,dom0,dfus,dfuc,dres,dnorm,dgap
      INTEGER lun,np,np1,i,j,jf,k,m,k1,k2,m1,m2,mgap
      REAL pi,f,a,x,dt
C
      INTEGER mpow,mfla,npwin,mpwin(20),mslwin,nused
      DOUBLE COMPLEX      cdv1(mxps),cdv2(mxps),cdv10(mxps),cdv20(mxps)
     +                   ,cdvw(mxps),cdvw0(mxps),cdnor,cdcur1,cdcur2
     +                   ,cdv3(mxps)
      DOUBLE PRECISION     dv0(mxps),dv1(mxps),dv2(mxps),dvw(mxps)
     +                    ,dvw0(mxps),dv10(mxps),dv3(mxps)
      EQUIVALENCE         (cdv1,dv1),(cdv2,dv2),(cdvw,dvw),(cdv10,dv10)
     +                   ,(cdv3,dv3)
      REAL apow,sabs(mxps),sphase(mxps),av,rms,speclim
      DOUBLE PRECISION dwinsl(20),dwinbeg(20),dmx,dmn,dav,drms
C
      lun=21
      np=0
 10   READ(lun,*,END=20) a
      np=np+1
      vin(np)=a
      GO TO 10
C
 20   CONTINUE
C
      apow=LOG(REAL(np))/LOG(2.)
      mpow=INT(apow)
      np1=np
      IF(2**mpow.LT.np) THEN
         mpow=mpow+1
         np1=2**mpow
         IF(np1.GT.mxps) THEN
            WRITE(6,*) ' Too short array vin(',mxps,') for ',np1
     +                ,' points'
            GO TO 999
         ENDIF
         DO i=np+1,np1
            vin(i)=0.
         ENDDO
      ENDIF
      WRITE(6,*) ' Number of points ',np1,'   binary power '
     +          ,mpow,np,2**mpow
C
      DO i=1,mxps
         cdv1(i)=DCMPLX(0.D0,0.D0)
         cdv2(i)=DCMPLX(0.D0,0.D0)
         cdv10(i)=DCMPLX(0.D0,0.D0)
         cdv20(i)=DCMPLX(0.D0,0.D0)
         cdvw(i)=DCMPLX(0.D0,0.D0)
         cdvw0(i)=DCMPLX(0.D0,0.D0)
      ENDDO
C
      k1=1
      k2=np1
C      k2=np
C      k1=k2-2**mpow+1
C      np1=k2-k1+1
      WRITE(6,*) ' Number of points =',np1,k1,k2,np
C
      j=0
      DO i=k1,k2
         j=j+1
         dv0(j)=vin(i)
         dv10(j)=dv0(j)
      ENDDO
C
C---     Define the window function and the transform for it
C
      npwin=9
      mslwin=20
      mpwin(1)=1
      mpwin(2)=mpwin(1)+mslwin
      mpwin(3)=20000
      mpwin(4)=mpwin(3)+mslwin
      mpwin(5)=30000
      mpwin(6)=mpwin(5)+mslwin
      mpwin(8)=np
      mpwin(7)=mpwin(8)-mslwin
      mpwin(9)=np1
C
      dmn=0.0D0
      dmx=1.0D0
      dwinbeg(1)=dmn
      dwinbeg(2)=dmx
      dwinbeg(3)=dmx
      dwinbeg(4)=dmn
      dwinbeg(5)=dmn
      dwinbeg(6)=dmx
      dwinbeg(7)=dmx
      dwinbeg(8)=dmn
      dwinbeg(9)=dmn
      DO i=1,npwin-1
         dwinsl(i)=(dwinbeg(i+1)-dwinbeg(i))/DBLE(mpwin(i+1)-mpwin(i))
         DO j=mpwin(i),mpwin(i+1)
            dvw(j)=dwinbeg(i)+DBLE(j-mpwin(i))*dwinsl(i)
C            dvw(j)=1.D0
            dvw0(j)=dvw(j)
C            IF(j.EQ.mpwin(i)) write(6,*) i,j,dvw(j)
         ENDDO
C         write(6,*) i,mpwin(i),dwinbeg(i),dwinsl(i)
      ENDDO
C      DO j=1,np1
C         dvw(j)=0.D0
C         IF(j.EQ.1) dvw(j)=1.D0
C         dvw0(j)=dvw(j)
C      ENDDO
C
C---      Multiply the input spectrum by the window
C
      DO j=1,np1
         dv1(j)=dv0(j)*dvw(j)
      ENDDO
C
C---      DFT (FFT) transform of the window
C
      mfla=-mpow
      CALL DRFSTFT(mfla,cdvw)
      DO i=np1/2+2,np1
         cdvw(i)=DCONJG(cdvw(np1-i+2))
      ENDDO
      DO i=1,np1
         cdvw0(i)=cdvw(i)
      ENDDO
      WRITE(6,*) ' DFT x->f Window done: cdvw0'
C
      mfla=-mpow
      CALL DRFSTFT(mfla,cdv10)
      DO i=np1/2+2,np1
         cdv10(i)=DCONJG(cdv10(np1-i+2))
      ENDDO
      cdcur1=0.D0
      DO i=1,np1
         cdcur1=cdcur1+cdv10(i)
      ENDDO
      WRITE(6,*) ' DFT x->f raw signal done: cdv10'
      WRITE(6,*) 'Sum cdv1',cdcur1
C
C---      DFT (FFT) transform of the windowed spectrum
C
      mfla=-mpow
      CALL DRFSTFT(mfla,cdv1)
      DO i=np1/2+2,np1
         cdv1(i)=DCONJG(cdv1(np1-i+2))
      ENDDO
      WRITE(6,*) ' DFT x->f windowed signal done: cdv1'
      GO TO 100
C
C---     Deconvolute the Fourier transform (Sum(signal*window))
C
      DO j=1,np1/2+1
         cdcur1=0.D0
         DO i=1,np1
            k=j-i+1
            IF(k.LT.1) k=k+np1
            IF(k.GT.np1) k=k-np1
            IF(k.LT.1.OR.k.GT.np1) write(6,*) 'Err',j,i,k
            cdcur1=cdcur1+cdv10(i)*cdvw0(k)
         ENDDO
         cdv20(j)=cdcur1
      ENDDO
C      go to 100
C
      DO j=1,np1/2+1
C         cdv2(j)=cdv1(j)*cdnor
         cdv2(j)=cdv1(j)
         DO i=1,j-1
            cdv2(j)=cdv2(j)-cdv2(i)*cdvw0(j-i+1)
         ENDDO
         cdv2(j)=cdv2(j)/cdvw0(1)
C         cdv20(j)=cdv2(j)
C         if(j.EQ.1) write(6,*) cdv2(1),cdvw0(1),cdnor
      ENDDO
C
      mfla=mpow
      CALL DRFSTFT(mfla,cdv2)

 100  continue
C
C---   Find the average and RMS of the spectrum
C
      dav=0.D0
      drms=0.D0
      DO i=1,np1/2+1
         sabs(i)=CDABS(cdv10(i))
         sphase(i)=ATAN2(DIMAG(cdv10(i)),DREAL(cdv10(i)))*180./3.1416
         dav=dav+DBLE(sabs(i))
         drms=drms+(DBLE(sabs(i)))**2
      ENDDO
      dav=dav/(np1/2+1)
      av=dav
      rms=SQRT(drms/(np1/2+1)-dav**2)
C      write(6,*) av,rms
      speclim=av+12.*rms
      nused=0
      DO i=1,np1/2+1
         IF(i.GT.100.AND.sabs(i).GT.speclim) THEN
            cdv3(i)=cdv10(i)
            nused=nused+1
            write(6,*) ' selected ',i,sabs(i),sphase(i),cdv10(i)
         ELSE
            cdv3(i)=DCMPLX(0.D0,0.D0)
         ENDIF
      ENDDO
      mfla=mpow
      CALL DRFSTFT(mfla,cdv3)
      WRITE(6,*) ' DFT f->x windowed signal done: cdv3, points=',nused
     +     ,speclim

C
C      mfla=-mpow
C      CALL DRFSTFT(mfla,cdv1)
C
C      mfla=mpow
C      CALL DRFSTFT(mfla,cdv1)
C
C      DO i=1,10
C         WRITE(6,*) rx(i)
C      ENDDO 
C
C      DO i=1,np1/2
C         WRITE(6,*) i,cx(i)
C      ENDDO 

C      WRITE(6,FMT=
C     +   '(I9,2F12.8,D16.6,5X,2D16.6,3X,2D16.6,3X,2D16.6,3X,2D16.6)') 
C     +    (i,dv0(i),dv2(i)
C     +     ,dv2(i)-dv0(i),cdv10(i),cdvw0(i),cdv1(i),cdv20(i),i=1,np1)

C      WRITE(6,FMT='(I9,2F12.8,D16.6,5X,2D16.6)') (i,dvw(i),dvw0(i)
C     +     ,dvw(i)-dvw0(i),cdvw0(i),i=1,np1)
C      WRITE(6,FMT='(I9,2F12.8,D16.6)') (i,dv1(i),dv2(i),dv2(i)-dv1(i)
C     +     ,i=1,np1)
C
C---     Fourier series
C
C
C      WRITE(22,FMT='(I7,E14.5,5X,2E13.5)') 
C     +         (jf,fk(jf),(REAL(dfourk(jf,i)),i=1,2),jf=0,mxf)
C       WRITE(23,FMT='(E13.5)') 
C     +         (REAL(dv2(i)),i=1,np1)
 900  CONTINUE
      WRITE(22,FMT='(E13.5)') 
     +         (CDABS(cdv10(i)),i=1,np1)
      WRITE(23,FMT='(E13.5)') 
     +         (CDABS(cdvw0(i)),i=1,np1)
      WRITE(24,FMT='(E13.5)') 
     +         (CDABS(cdv1(i)),i=1,np1)
      WRITE(25,FMT='(E13.5)') 
     +         (dv3(i),i=1,np1)
      WRITE(26,FMT='(E13.5)') 
     +         (sphase(i),i=1,np1)
C
 999  CONTINUE
C
      END
C
C      INCLUDE 'rfstft.F'
C      INCLUDE 'cfstft.F'
