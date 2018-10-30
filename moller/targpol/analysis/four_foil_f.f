      PROGRAM FOUR_FOIL_T
C
C --- Fourier analysis of the foil magnetic spectrum
C --- Takes the full spectrum: (60000 points)
C ---  a) makes Fast Fourier transforms (FFT) of
C ---     1) first 2**14=16384 bins
C ---     2) last  2**15=32768 bins
C      b) from 2), takes about 15 highest peaks in freq. domain
C         and select those which exist in 1)
C      c) for selected frequencies only - make FT ("by hand") for the full spectrum
C         (ares around the peaks - zeroed)   
C
      IMPLICIT NONE
      INTEGER    mxp,mxps,mxf,mxb
      PARAMETER (mxp=80000,mxps=80000,mxf=10000,mxb=3)
      REAL vin(mxp),fk(mxf)
      DOUBLE PRECISION dpi,dbin,df,dx,dom,dom0,dfus,dfuc,dres,dnorm,dgap
      INTEGER lun,np,np1,i,j,jf,k,m,k1,k2,m1,m2,mgap
      REAL pi,f,a,x,dt
C
      INTEGER mpow,mfla,npwin,mpwin(20),mslwin,nused,ibg,ifull
     +       ,mpows(mxb),npp(mxb),kpp(2,mxb),isel,kf,ks1,ks2,ns
      INTEGER ifsel(mxps),ifseln(mxps),ifselor(mxps,mxb)
      DOUBLE COMPLEX    cdv0(mxps),cdv1(mxps),cdvbg(mxps,mxb)
     +                 ,cdv1z(mxps)
     +                 ,cdnor,cdcur1,cdcur2,cdom,cdv2(mxps),ctmp
      DOUBLE PRECISION   dvin(mxps),dv0(mxps),dv1(mxps)
     +                  ,dvbg1(mxps),dvbg2(mxps),dvbg3(mxps)
     +                  ,dv0z(mxps),dv1z(mxps),dv2(mxps)   
      EQUIVALENCE      (cdv0,dv0),(cdvbg(1,1),dvbg1),(cdvbg(1,2),dvbg2)
     +                 ,(cdvbg(1,3),dvbg3)
     +                 ,(cdv1z,dv1z),(cdv1,dv1),(cdv2,dv2)
      REAL apow,sabsbg(mxps,mxb),sphasbg(mxps,mxb),bgav(mxb),bgrms(mxb)
     +    ,speclim(mxb),cutrms(mxb),sabs(mxps),sphas(mxps)
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
      dpi=2.D0*DACOS(0.D0)
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
      mpows(1)=14  ! left BG
      mpows(2)=15  ! right BG
      DO i=1,2
         npp(i)=2**mpows(i)
      ENDDO
      kpp(1,1)=1            ! left  BG 1-st chan
      kpp(2,1)=npp(1)       ! left  BG last chan
      kpp(1,2)=np-npp(2)+1  ! right BG 1-st chan
      kpp(2,2)=np           ! right BG last chan
      cutrms(1)=8.          ! cuts to select significant frequencies
      cutrms(2)=8.
      npp(3)=np1
      kpp(1,3)=1            ! full  BG 1-st chan
      kpp(2,3)=npp(3)       ! full  BG last chan
      cutrms(3)=20.
      mpows(3)=mpow
C
      DO i=1,mxps
         cdv0(i)=DCMPLX(0.D0,0.D0)
         cdv1z(i)=DCMPLX(0.D0,0.D0)
         cdv2(i)=DCMPLX(0.D0,0.D0)
         DO ibg=1,2
            sabsbg(i,ibg)=0.
            sphasbg(i,ibg)=0.
            cdvbg(i,ibg)=DCMPLX(0.D0,0.D0)
            ifselor(i,ibg)=0
         ENDDO
         ifsel(i)=0
      ENDDO
C
      DO i=1,np1
         dvin(i)=vin(i)
         dv0(i)=dvin(i)
         dv0z(i)=dvin(i)
         dv1z(i)=dvin(i)
      ENDDO
C
C---   Spectrum with zeroed peak area
C
      DO i=kpp(2,1)+1,kpp(1,2)-1
         dv1z(i)=0.
      ENDDO
C
      j=0
      DO i=kpp(1,1),kpp(2,1)
         j=j+1
         dvbg1(j)=dvin(i)
      ENDDO
      j=0
      DO i=kpp(1,2),kpp(2,2)
         j=j+1
         dvbg2(j)=dvin(i)
      ENDDO
      j=0
      DO i=kpp(1,3),kpp(2,3)
         j=j+1
         dvbg3(j)=dv1z(i)
      ENDDO
C
C---      DFT (FFT) transform of the left and right BG
C
      DO ibg=1,3
         mfla=-mpows(ibg)
         CALL DRFSTFT(mfla,cdvbg(1,ibg))
         DO i=npp(ibg)/2+2,npp(ibg)
            cdvbg(i,ibg)=DCONJG(cdvbg(npp(ibg)-i+2,ibg))
         ENDDO
      ENDDO
C
      mfla=-mpows(3)
      CALL DRFSTFT(mfla,cdv1z)
C
      WRITE(6,*) ' DFT x->f left/right: cdv01,cdv02'
C
C---   Analyze the spectra and select the most significant amplitudes
C
C---   Find the average and RMS of the spectrum
C
      DO ibg=1,3
         kf=2**(mpow-mpows(ibg))  ! frequency conversion BG --> full spectrum
         dav=0.D0
         drms=0.D0
         DO i=1,npp(ibg)/2+1
            ifull=i*kf
            ifselor(ifull,ibg)=i
            sabsbg(ifull,ibg)=CDABS(cdvbg(i,ibg))
            sphasbg(ifull,ibg)=
     +                 ATAN2(DIMAG(cdvbg(i,ibg)),DREAL(cdvbg(i,ibg)))
            dav=dav+DBLE(sabsbg(ifull,ibg))
            drms=drms+(DBLE(sabsbg(ifull,ibg)))**2
         ENDDO
         dav=dav/(npp(ibg)/2+1)
         bgav(ibg)=dav
         bgrms(ibg)=SQRT(drms/(npp(ibg)/2+1)-dav**2)
         speclim(ibg)=bgav(ibg)+cutrms(ibg)*bgrms(ibg)
      ENDDO
C
      WRITE(6,*) ' Spectrum amplitude limits  for BG :',speclim
C
      nused=0
      DO i=1,np1/2+1
         IF(ifselor(i,1).GT.0.AND.ifselor(i,2).GT.0) THEN
            IF(sabsbg(i,1).GT.speclim(1).OR.
     +         sabsbg(i,2).GT.speclim(2)) THEN
               nused=nused+1
               ifseln(nused)=i
               ifsel(i)=nused
            ENDIF

C               isel=0
C               m1=i+30
C               m2=i-30
C               write(6,*) ' Select ',i,sabsbg(i,2)
C               DO j=i-30,i+30
C                write(6,*) ' Try ',j,sabsbg(j,1)
C                  IF(sabsbg(j,2).GT.speclim(2)) THEN
C                     isel=1
C                     m1=MIN(m1,j)
C                     m2=MAX(m2,j)
C                     IF(ifsel(j).EQ.0.AND.(j.EQ.i.OR.i.GT.100)) THEN
C                        nused=nused+1
C                        ifseln(nused)=j
C                        ifsel(j)=nused
C                       write(6,*) ' Selected ',j,sabsbg(j,1),sabsbg(j,2)
C                    ENDIF
C                  ENDIF
C               ENDDO
C               isel=1
C               m1=i
C               m2=i
C               IF(isel.NE.0) THEN
C                  DO j=m1,m2
C                     nused=nused+1
C                     ifseln(nused)=j
C                     ifsel(i)=nused
C                     write(6,*) ' Selected ',j,sabsbg(j,1),sabsbg(j,2)
C                  ENDDO
C               ENDIF
         ENDIF
      ENDDO
C
      WRITE(6,*) ' Number of used spectrum points: ',nused 
      IF(nused.LT.2) GO TO 900
C
C ---  FT for the selected frequencies, for the full spectrum, peak area zeroed
C
      ns=npp(1)
      ks1=kpp(1,1)
      ks2=kpp(2,1)

      ns=np1
      ks1=1
      ks2=np1
      dom=2.D0*dpi/DBLE(ns)
      cdom=DCMPLX(0.D0,dom)
      DO k=1,nused
         i=ifseln(k)
         ctmp=DCMPLX(0.D0,0.D0)
         m=ifselor(i,1)
C         m=i
         DO j=1,ns
            ctmp=ctmp+dv0z(j)*CDEXP(-cdom*(j-1)*(m-1))
         ENDDO
         cdv1(m)=ctmp/ns
C         cdv1(m)=cdvbg(m,1)
         sabs(i)=CDABS(cdv1z(m))
         sphas(i)=ATAN2(DIMAG(cdv1z(m)),DREAL(cdv1z(m)))
C         WRITE(6,*) ' Final spectrum ',i,sabs(i),sphas(i)
      ENDDO
C
C ---  Reconstruct the full spectrum
C 
      mfla=mpows(1)
C      mfla=mpow
      CALL DRFSTFT(mfla,cdv1)
      WRITE(6,*) ' DFT f->x zeroed spectrum '
C
      k=0
      DO i=1,np1/2+1
         IF(sabsbg(i,3).GT.speclim(3)) THEN
C                      IF(ifsel(i).GT.0) THEN
C                        cdv2(i)=(cdvbg(i,1)+cdvbg(i,2))/2.D0
            cdv2(i)=cdv1z(i)
            k=k+1
         ENDIF
      ENDDO
      mfla=mpow
      CALL DRFSTFT(mfla,cdv2)
      WRITE(6,*) ' DFT f->x averaged BG spectrum , points=',k
C
 900  CONTINUE
      WRITE(22,FMT='(E13.5)') 
     +         (sabsbg(i,1),i=1,np1)
      WRITE(23,FMT='(E13.5)') 
     +         (sabsbg(i,2),i=1,np1)
      WRITE(24,FMT='(E13.5)') 
     +         (sabs(i),i=1,np1)
      WRITE(25,FMT='(E13.5)') 
     +         (dv1(i),i=1,np1)
      WRITE(26,FMT='(2I6,2E13.5)') 
     +         (k,ifseln(k),sabs(ifseln(k)),sabs(ifseln(k)),k=1,nused)
      WRITE(27,FMT='(E13.5)') 
     +         (dv2(i),i=1,np1)
      WRITE(28,FMT='(E13.5)') 
     +         (CDABS(cdv1z(i)),i=1,np1)
      WRITE(29,FMT='(E13.5)') 
     +         (sabsbg(i,3),i=1,np1)
C
 999  CONTINUE
C
      END
C
C      INCLUDE 'rfstft.F'
C      INCLUDE 'cfstft.F'
