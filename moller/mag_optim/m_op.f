      SUBROUTINE M_OP(B1,B3)
C
C ===     Magnet optimization for Moller
C ---     INPUT: m_opt.in
C
      IMPLICIT NONE
      REAL B1,B2
C
      COMMON/CMPAR/ PAR_MAG(8,4),PAR_COL(5,7),DTOLER(3)
      REAL PAR_MAG,PAR_COL,DTOLER
C
      VECTOR p_mag(10)
C
      REAL    e0,x1,x2,y1,y2,z1,z2,dc(3)
      INTEGER id,ifl,nx,ny
C
      INTEGER lrec,ista,ifirst
      DATA ifirst/1/
C
C     ------------------------------------------------------------------
C
      IF(ifirst.EQ.1) THEN
         ifirst=0
         OPEN(1,FILE='m_opt.in',STATUS='OLD',IOSTAT=ista)
         IF(ista.NE.0) THEN
            WRITE(6,*) ' *** Error openinig m_opt.in'
            GO TO 999
         ENDIF
C
         READ(1,*,IOSTAT=ista) e0,id,ifl,nx,ny,x1,x2,y1,y2,z1,z2,dc
         IF(ista.NE.0) THEN
            WRITE(6,*) ' *** Error reading m_opt.in'
            GO TO 999
         ENDIF
         CLOSE(1)
C
         CALL M_OINI(dc(1))
      ENDIF
      e0=p_mag(1)
C
      CALL M_OPT3(e0,id,ifl,nx,ny,x1,x2,y1,y2,z1,z2)
C
 999  CONTINUE
C
      END
C
      SUBROUTINE M_OINI(DC)
C     -----------------------------------------------------------------
      IMPLICIT NONE
      REAL DC(3)
C
C      DC(3) - tolarances on the collimators for the 90 degrees scattering
C
      COMMON/CMPAR/ PAR_MAG(8,4),PAR_COL(5,7),DTOLER(3)
      REAL PAR_MAG,PAR_COL,DTOLER
      vector par_mag1(8,4),par_col1(5,7)
C
      INTEGER i,j
      REAL a2
C
      REAL p1(8,4),p2(5,7)
      DATA p1/100.,  10.16,  45.05,  0.    ,  1.020, 3*0.
     +       ,208.5, 10.16,  36.22,  0.    ,  1.762, 3*0.
     +       ,274.2, 10.16,  36.22,  0.    ,  0.907, 3*0.
     +       ,422.8, 11.0 , 164.5 ,  0.    ,  6.024, 3*0./
      DATA p2/100.,  0.,     4.78,   0.    ,  4.78
     +       ,208.5, 0.,     4.78,   0.    ,  4.78
     +       ,274.2, 0.,     4.78,   0.    ,  4.78
     +       ,324.,  3.,     4.9,   -1.    ,  1.
     +       ,520.,  3.,     5.3,  -24.    ,  2.
     +       ,696.2, 2.0,    6.0,  -62.5   ,  0.
     +       ,700.9, 2.0,    6.0,  -99.    ,-39.0/
C     +       ,696.2, 2.,     6.0,  -62.5   ,  0.
C     +       ,700.9, 2.,     6.0,  -99.    ,-39.0/
C
      DO i=1,4
         a2=p1(2,i)/2.
         IF(i.EQ.4) a2=1.
         p1(4,i)=p1(3,i)/a2*3.E-4
      ENDDO
C
      DO i=1,4
         DO j=1,8
            PAR_MAG(j,i)=p1(j,i)
            par_mag1(j,i)=PAR_MAG(j,i)
         ENDDO
      ENDDO
      DO i=1,7
         DO j=1,5
            PAR_COL(j,i)=p2(j,i)
         ENDDO
      ENDDO
C
C---     Tolerance
C
      DO i=1,3
         DTOLER(i)=DC(i)
      ENDDO
C      DO i=4,7
C         PAR_COL(2,i)=PAR_COL(2,i)+DC(2)
C         PAR_COL(3,i)=PAR_COL(3,i)-DC(3)
C      ENDDO
C
      RETURN
      END
C
      SUBROUTINE M_OPT3(E,ID,IPRI,NBINX,NBINY
     +                 ,BMN1,BMX1,BMN2,BMX2,BMN3,BMX3)
C
C===    Magnet optimization
C
C      E   - beam energy
C      ID  - histogram offset
C      IPRI - print
C
      IMPLICIT NONE
      REAL E,BMN1,BMX1,BMN2,BMX2,BMN3,BMX3
      INTEGER ID,NBINX,NBINY,IPRI
C
      COMMON/CMPAR/ PAR_MAG(8,4),PAR_COL(5,7),DTOLER(3)
      REAL PAR_MAG,PAR_COL,DTOLER
C
C      VECTOR PZCO(8),PXCO(8)
C
      COMMON/CMOPT/ EBEAM,PCM,PI
      REAL          EBEAM,PCM,PI
C
      REAL p0,thetmx,ctmx,b,a1,a2,bmnx(2,3),bst(3)
     +    ,bcur(3),ctcur,ctcur0,ctcur1
     +    ,zco(8),xco(8),yco(8),xsl(8),ysl(8)
     +    ,pl,pt,hm(4),rr,dr,dsl
     +    ,ecm,z,x,sl,zst,qsl,thet,phi,phimx,phisum
     +    ,cteff,wgt,slac,ctmax,hm3der,hm3,dhm3
     +    ,db3l,db3r,h3ac(2),h3acur(2),sigs,ct1max,hmx(3),bstep,ddif(3)
     +    ,h3lim(2),ct2max
      INTEGER i,j,i1,i2,i3,nstep1,nstep2,ithet,ip,miss,im,nct,ict
     +       ,ista,itry,itry1,istop,istep,ist1,nb2
C
      INTEGER    mxstep
      PARAMETER (mxstep=11)
      REAL      h3step(mxstep),ctstep(mxstep)
C
C     ------------------------------------------------------------------
C
      PI=ACOS(0.)*2.
C
      EBEAM=E
      p0=E/2.
      ecm=SQRT(2.*0.000511*E)
      PCM=ecm/2.
C

C===    Dipole settings
C
      a1=(PAR_COL(1,7)-PAR_MAG(1,4))/PAR_COL(5,7)
      a1=SQRT(1.+a1**2)
      a2=(PAR_COL(1,6)-PAR_MAG(1,4))/PAR_COL(4,6)
      a2=SQRT(1.+a2**2)
      b=-2.*p0/(a1+a2)/PAR_MAG(4,4)
      ctmx=(a1-a2)/(a1+a2)
      thetmx=ACOS(ctmx)
      WRITE(6,*) 'Dipole kGs=',b,ctmx,thetmx*180./PI-90.
C
      DO i=1,3
         bcur(i)=0.
      END DO
C
      DO i=1,2
         h3lim(i)=bmnx(i,3)*PAR_MAG(4,3)
      ENDDO
C      WRITE(6,*) bmnx(1,3),bmnx(2,3),h3lim
C      
C
      ct1max=0.
      bcur(2)=0.                !!
      bcur(3)=0.
C
      DO i=1,3
         hm(i)=bcur(i)*PAR_MAG(4,i)
      END DO
C
      CALL B3OPT(hm,h3ac(1),slac,h3lim(1),IPRI)
C
            IF(IPRI.GT.1) WRITE(6,*) 'slac=',slac
     +         ,hm(3)/PAR_MAG(4,3),(h3ac(i)/PAR_MAG(4,3),i=1,2)
            IF(slac.LE.0.) GO TO 599 
            IF(hm(3).LT.h3lim(1).OR.hm(3).GT.h3lim(2).OR.
     +         hm(3)+h3ac(1).LT.h3lim(1)*1.00001.OR.
     +         hm(3)+h3ac(2).GT.h3lim(2)*1.00001) THEN
               WRITE(6,3060) 
     +              (hm(i)/PAR_MAG(4,i),i=1,3)
     +             ,(h3ac(i)/PAR_MAG(4,3),i=1,2)
     +             ,(h3lim(i)/PAR_MAG(4,3),i=1,2)
 3060          FORMAT('Wrong lim from B3OPT',10F10.5) 
            ENDIF
C
C---          Optimize the Q3
C
            ista=0
            itry=0
            DO i=1,2
               h3acur(i)=h3ac(i)
            ENDDO
C
            CALL QPOINT(hm,1.E-6,0,1,ctmax,IPRI)
            IF(ctmax.LE.0.) THEN
               IF(IPRI.GT.0) WRITE(6,1700) 
     +           itry,ctmax,slac,(hm(i)/PAR_MAG(4,3),i=1,3),i1,i2
 1700          FORMAT(' *** Drop at itry=',I3,' ctmax,slac=',2E10.3
     +               ,' B=',3F8.4,' i1,i2=',2I4)
               GO TO 599
            ENDIF
C
 200        itry=itry+1
C
            hm3der=(h3acur(2)-h3acur(1))/REAL(mxstep-1)
            hm3=hm(3)
            istep=(mxstep-1)/2+1
            ctmax=0.
            ist1=0
C            hm3der=0.002*PAR_MAG(4,3)
C
            DO i=1,mxstep
               hm(3)=hm3+h3acur(2)-(i-1)*hm3der
               h3step(i)=hm(3)
               CALL QPOINT(hm,1.E-6,0,1,ctcur,IPRI)
               IF(IPRI.GT.2) THEN
                  WRITE(6,FMT='(A9,2I4,2F11.8,F11.7)') ' itry,ist'
     +             ,itry,i,hm(3)/PAR_MAG(4,3),hm3der/PAR_MAG(4,3),ctcur
               ENDIF
               ctstep(i)=ctcur
               IF(i.GT.1.AND.i.LT.mxstep) THEN
                  IF(ctcur.GT.ctmax+1.E-6) THEN
                     ctmax=ctcur
                     istep=i
                     ist1=0
                  ELSE IF(ctcur.GT.ctmax-1.1E-6) THEN
                     ist1=i-istep
                  ENDIF
               ENDIF
            ENDDO
C
            istep=istep+ist1/2
            hm(3)=h3step(istep)
            ctmax=ctstep(istep)
            IF(IPRI.GT.1) THEN
               WRITE(6,FMT='(A12,3I3,4F9.4,I4,3F11.7)') 
     +                 ' i1,i2,itry',i1,i2,itry
     +                 ,(hm(i)/PAR_MAG(4,i),i=1,3)
     +                 ,hm3der/PAR_MAG(4,3),istep
     +                 ,(ctstep(istep-2+i),i=1,3)
            ENDIF
C
            IF(itry.LT.2) THEN
               sigs=0.
               IF(istep.EQ.2) sigs=h3acur(1) 
               IF(istep.EQ.mxstep-1) sigs=h3acur(2)
               IF(ABS(sigs).GT.1.E-20) THEN
                  hm(3)=hm3+sigs*0.1
                  GO TO 200
               ENDIF
            ENDIF
C
            IF(itry.LT.3.OR.
     +        (hm3der/PAR_MAG(4,3).GT.0.002.AND.
     +        (ABS(ctstep(istep-1)-ctmax).GT.1.E-4).OR.
     +        (ABS(ctstep(istep+1)-ctmax).GT.1.E-4))) THEN
               IF(itry.LT.15) THEN
                  h3acur(1)=MAX(-hm3der*1.5,h3lim(1)-hm(3))
                  h3acur(2)=MIN( hm3der*1.5,h3lim(2)-hm(3))
C                  h3acur(1)=-hm3der*1.5
C                  h3acur(2)= hm3der*1.5
                  GO TO 200
               ELSE
                  WRITE(6,3160) itry
     +                 ,(hm(i)/PAR_MAG(4,i),i=1,3)
     +                 ,hm3der/PAR_MAG(4,3),ctmax
     +                 ,(  h3ac(i)/PAR_MAG(4,3),i=1,2)
     +                 ,(h3acur(i)/PAR_MAG(4,3),i=1,2)
 3160             FORMAT(' Try=',I3,3F8.3,E12.3,F8.4,4F10.5)
                  GO TO 599
               ENDIF
            ENDIF
C
            CALL QPOINT(hm,1.E-6,0,0,ctmax,IPRI)
C
            IF(IPRI.GT.0) THEN
               WRITE(6,3200) itry
     +                 ,(hm(i)/PAR_MAG(4,i),i=1,3)
     +                 ,hm3der/PAR_MAG(4,3),istep
     +                 ,(ctstep(istep-2+i),i=1,3)
 3200          FORMAT(' End ',I3,3F8.4,2X,F8.4,2X,I3,3F11.7)
            ENDIF
C               
            bcur(3)=hm(3)/PAR_MAG(4,3)
C            WRITE(6,*) ' try=',itry,itry1,' b(3)=',bcur(3),ctmax
C
            IF(ctmax.GT.ct1max) THEN
               ct1max=ctmax
                DO i=1,3
                  hmx(i)=hm(i)
               ENDDO
            ENDIF
C
            CALL HF2( 1+ID,bcur(1),bcur(3),ctmax)
            CALL HF2( 2+ID,bcur(1),bcur(2),bcur(3))
            CALL HF2( 5+ID,bcur(1),bcur(2)+bcur(3),ctmax)
            CALL HF2( 6+ID,bcur(1),bcur(2)+bcur(3),bcur(2))
            CALL HF2( 7+ID,ctmax,bcur(1)+bcur(2)+bcur(3),1.)
            CALL HF2( 9+ID,bcur(1),bcur(2)
     +                    ,bcur(1)+bcur(2)+bcur(3))
            CALL HF2(10+ID,bcur(1),bcur(2)
     +           ,ABS(bcur(1))+ABS(bcur(2))+ABS(bcur(3)))
C
C---            Phi acceptance for theta<14 degrees
C
            GO TO 599
C
            phisum=0.
            IF(ABS(ctmax).GT.0.01) THEN
               cteff=ABS(ctmax)
               nct=cteff/0.01
C               WRITE(6,*) ctcur1,cteff,nct
               DO i=2,3
                  hm(i)=bcur(i)*PAR_MAG(4,i)
               END DO
C
               DO ict=1,nct
                  ctcur=(ict-0.5)*0.01
                  pl=p0*(1.-ctcur)
                  pt=PCM*SQRT(1.-ctcur**2)
                  zco(1)=0.
                  yco(1)=0.
                  phi=0.01
                  ysl(1)=pt*phi/pl
C
                  DO im=1,3
                     zco(im+1)=PAR_MAG(1,im)
                     yco(im+1)=yco(im)+ysl(im)*(zco(im+1)-zco(im))
                     ysl(im+1)=ysl(im)+hm(im)/pl*yco(im+1)
                  END DO
                  DO im=4,4
                     zco(im+1)=PAR_COL(1,im)
                     ysl(im+1)=ysl(im)
                     yco(im+1)=yco(im)+ysl(im)*(zco(im+1)-zco(im))
                  END DO
                  phimx=PAR_COL(4,4)/yco(5)*phi
                  phisum=phisum+phimx
               END DO
               phisum=phisum/PI*0.01/nct
C               WRITE(6,*) 'i1,i2 phisum=',i1,i2,phisum
               CALL HF2(3+ID,bcur(1),bcur(2),phisum)
            ENDIF   
C
 599        CONTINUE
C
C         ENDDO
         IF(MOD(i1,10).EQ.1) WRITE(6,FMT='(I5,'' out of '',I5)') 
     +                                   i1,nstep1
C
         IF(ct1max.GT.0.) THEN
C
            DO i=2,3
               hm(i)=hmx(i)
               bcur(i)=hm(i)/PAR_MAG(4,i)
            ENDDO
C
            IF(NBINY.GT.0) THEN
               DO i=1,3
                  DO i2=1,nb2
                     b=0.01*(i2-0.5-nb2/2)
                     hmx(i)=hm(i)+b*PAR_MAG(4,i)
                     CALL QPOINT(hmx,1.E-6,0,0,ctmax,IPRI)
                     IF(ABS(ctmax).GT.0.001) THEN
                        CALL HF2(ID+26+i,bcur(1),b,ctmax)
                     ENDIF
                  ENDDO
                  hmx(i)=hm(i)
               ENDDO
            ENDIF
C
            CALL QPOINT(hmx,1.E-6,0,0,ctmax,IPRI)
            IF(ABS(ctmax).LT.0.001) THEN
               WRITE(6,FMT='('' Problem '',3F7.3,3F10.6)')
     +            (hmx(i)/PAR_MAG(4,i),i=1,3),ctmax
            ENDIF
C
            bstep=0.02
            DO j=2,3
               hmx(j)=hm(j)-bstep*PAR_MAG(4,j)
               CALL QPOINT(hmx,1.E-6,0,0,ct2max,IPRI)
C               WRITE(6,FMT='(I2,3F13.8,3F10.6)') j
C     +           ,(hmx(i)/PAR_MAG(4,i),i=1,3),ct2max
               hmx(j)=hm(j)+bstep*PAR_MAG(4,j)
               CALL QPOINT(hmx,1.E-6,0,0,ct1max,IPRI)
C               WRITE(6,FMT='(I2,3F13.8,3F10.6)') j
C     +           ,(hmx(i)/PAR_MAG(4,i),i=1,3),ct1max
               hmx(j)=hm(j)
               ddif(j)=(ct1max-ct2max)/2./bstep
               IF(ABS(ct1max).GT.0.001.AND.ABS(ct2max).GT.0.001) THEN
                  CALL HF1(ID+22+j,bcur(1),ddif(j))
               ENDIF
C               WRITE(6,FMT='(I2,3F7.3,4F10.6)') j 
C     +          ,(hmx(i)/PAR_MAG(4,i),i=1,3),ctmax,ct1max,ct2max,ddif(j)
            ENDDO
            CALL QPOINT(hmx,1.E-6,0,0,ctmax,IPRI)
            CALL HF1(ID+19,bcur(1),bcur(1))
            CALL HF1(ID+20,bcur(1),90.-ACOS(ctmax)*180./PI)
            CALL HF1(ID+21,bcur(1),bcur(2))
            CALL HF1(ID+22,bcur(1),bcur(3))
            CALL HF1(ID+23,bcur(1),bcur(1)+bcur(2)+bcur(3))
            CALL HF1(ID+26,bcur(1)
     +              ,ABS(bcur(1))+ABS(bcur(2))+ABS(bcur(3)))
         ENDIF
C
      ENDDO
C
 999  CONTINUE
      END
C
      SUBROUTINE QPOINT(HM,TOLER,KFL,KTOL,CTOUT,IPRI)
C
C     *****************************************************************
C     *                                                               *
C     *             Calculate COS(thet) acceptance                    *   
C     *                                                               *
C     *****************************************************************
C
C      Input:
C      HM(1:4)  - mag fields
C      TOLER    - tolerance for COS(theta) acceptance  
C      KFL      = 0 - both particles
C               = 1 - backward particle
C               = 2 - forward particle
C      KTOL     - >0 - include the tolerance boundary for 90 deg scatt
C      CTOUT    - acceptance in COS(theta)
C
      IMPLICIT NONE
      REAL HM(4),TOLER,CTOUT
      INTEGER KFL,KTOL,IPRI
C
C
      COMMON/CMPAR/ PAR_MAG(8,4),PAR_COL(5,7),DTOLER(3)
      REAL PAR_MAG,PAR_COL,DTOLER
C      VECTOR PZCO(8),PXCO(8)
C
      COMMON/CMOPT/ EBEAM,PCM,PI
      REAL          EBEAM,PCM,PI
C
      REAL p0,ctcur,ctcur0,ctcur1
     +    ,zco(8),xco(8),yco(8),xsl(8),ysl(8)
     +    ,pl,pt,rr,dr,dsl
     +    ,z,z1,x,sl,zst,qsl,xmx,y,dtol(3)
C
      INTEGER i,ithet,ip,miss,im,nsplq,i1,i2
C
C     ------------------------------------------------------------------
C
      nsplq=20
C
      p0=EBEAM/2.
C
C---  Find the max theta to pass
C
      IF(IPRI.GT.2) THEN
         WRITE(6,1100) (HM(im)/PAR_MAG(4,im),im=1,3)
 1100    FORMAT(' QPOINT: B(1-3)=',3F8.4)
      ENDIF
C
      i1=1
      i2=2
C      
      IF(KFL.EQ.1) THEN
         i2=1
      ELSE IF(KFL.EQ.2) THEN
         i1=2
      ENDIF
C
      ithet=0
      ctcur1=0.
      ctcur0=1.
C
 400  ithet=ithet+1
C      WRITE(6,*)  'ithet',ithet
      IF(ithet.EQ.1) then
         ctcur=0.
      ELSE
         ctcur=(ctcur0+ctcur1)/2.
      ENDIF
C
      DO i=1,3
         dtol(i)=0.
      ENDDO
C      IF(KTOL.GT.0.AND.ABS(ctcur).LT.1.E-6) THEN
      IF(KTOL.GT.0) THEN
         DO i=1,3
            dtol(i)=DTOLER(i)   
         ENDDO
      ENDIF
C     
C---  Try two electrons
C
      miss=0
      DO ip=i1,i2
         DO i=1,8
            xco(i)=0.
            xsl(i)=0.
         END DO
         pl=p0*(1.+ctcur*(2*ip-3))
         pt=PCM*SQRT(1.-ctcur**2)
         zco(1)=0.
         xco(1)=0.
         xsl(1)=pt/pl
C 
         x=xco(1)
         z=zco(1)
C
         DO im=1,3
            zco(im+1)=PAR_MAG(1,im)
            xco(im+1)=x+xsl(im)*(zco(im+1)-z)
C     
C---  Split the quad into thin slices
C
            z1=PAR_MAG(1,im)-PAR_MAG(3,im)/2.
            x=x+xsl(im)*(z1-z)
            z=z1
            zst=PAR_MAG(3,im)/nsplq/2.
            sl=xsl(im)
            qsl=HM(im)/pl/REAL(nsplq)
            xmx=0.
            DO i=1,nsplq
               rr=x+zst*sl
               dsl=-qsl*rr
               sl=sl+dsl
               x=rr+zst*sl
               xmx=MAX(xmx,x)
               z=z+zst*2.
            END DO
C     
            xsl(im+1)=sl
C     
            IF(xmx.GT.PAR_COL(3,im)-dtol(1)) THEN
               miss=im+10*ip
               GO TO 599
            ENDIF
         END DO
         DO im=4,7
            zco(im+1)=PAR_COL(1,im)
            xsl(im+1)=xsl(im)
            xco(im+1)=x+xsl(im)*(zco(im+1)-z)
            x=xco(im+1)
            z=zco(im+1)
            IF(xco(im+1).LT.PAR_COL(2,im)+dtol(2).OR.
     +         xco(im+1).GT.PAR_COL(3,im)-dtol(3)) THEN
               miss=im+10*ip
               GO TO 599
            ENDIF
         END DO
 500     CONTINUE
C         WRITE(6,*) 'particle'
         IF(IPRI.GT.3) THEN
            WRITE(6,2000) miss,ip,ithet,ctcur,xco
C     +                  ,pl,pt,zco,xco,xsl
C     +                  ,PAR_COL(2,4),PAR_COL(3,4)
         ENDIF
 2000    FORMAT('   miss,ip',3I4,F9.6,1X,8F9.4)
C     +              /5X,8F10.1/5X,8F10.2/5X,8F10.6
C     +              /2F8.2)
      END DO
C
 599  CONTINUE
C
C      DO im=1,8
C         PZCO(im)=zco(im)
C         PXCO(im)=xco(im)
C      END DO
C     
      IF(miss.NE.0) THEN
         IF(ctcur.LT.ctcur0) ctcur0=ctcur 
      ELSE
         IF(ctcur.GT.ctcur1) ctcur1=ctcur 
      ENDIF
C
C---  Continue the theta loop?
C
C
      IF(IPRI.GT.2) THEN
         WRITE(6,FMT='(A6,I3,1X,A6,3F11.7)') 
     +               ' ithet=',ithet,' cptur=',ctcur,ctcur1,ctcur0
      ENDIF
      IF(ctcur0.GT.ctcur1.AND.
     +     ithet.LT.50000.AND.
     +     ABS(ctcur1-ctcur0).GT.TOLER) GO TO 400
C     
C---  Find the max COS(theta) for Q2-Q3 asymmetry
C     
C     WRITE(6,*) ' i3=',i3,bcur(1),bcur(2),bcur(3),b23asy
C     +                     ,ctcur1,ctmx23,b23asymx
C
      IF(IPRI.GT.1) THEN
         WRITE(6,1200) (HM(im)/PAR_MAG(4,im),im=1,3)
     +             ,ithet,ctcur
 1200    FORMAT(' QPOINT: B(1-3)=',3F8.4,5X,'iter=',I5,5X
     +         ,'COS(th)=',F10.7)
      ENDIF
      CTOUT=ctcur1
C
      END
C
      SUBROUTINE B3OPT(HM,HM3AC,SLACEPT,H3LIM,IPRI)
C
C     *****************************************************************
C     *                                                               *
C     *            Optimize B3                                        *   
C     *                                                               *
C     *****************************************************************
C
C      INPUT:  HM(1:2)  - mag fields
C              H3LIM(2) - limits on H3 (B3) values
C      OUTPUT: HM(3)
C              HM3AC(2) - acceptance in HM(3)
C              SLACEPT  - acceptance in slope (<=0 - no acceptance
C
      IMPLICIT NONE
      REAL HM(4),SLACEPT,HM3AC(2),H3LIM(2)
      INTEGER  IPRI
C
C
      COMMON/CMPAR/ PAR_MAG(8,4),PAR_COL(5,7),DTOLER(3)
      REAL PAR_MAG,PAR_COL,DTOLER
C
      COMMON/CMOPT/ EBEAM,PCM,PI
      REAL          EBEAM,PCM,PI
C
      REAL p0,ctcur
     +    ,zco(8),xco(8),yco(8),xsl(8),ysl(8)
     +    ,pl,pt,rr,dr
     +    ,z,z1,x,sl,zst,qsl,xmx
     +    ,cteff,slmin,slmax,dhm3,slope1,slope2,dsl,hm3,h3min,h3max
      INTEGER i,ithet,ip,im,nsplq,itry,lost3,ih3lim
C
C     ------------------------------------------------------------------
C
      nsplq=20
      SLACEPT=-1.
      HM(3)=0.
C
      p0=EBEAM/2.
C
C---  Find the max theta to pass
C
      ctcur=0.
      DO i=1,8
         xco(i)=0.
         xsl(i)=0.
      END DO
      pl=p0*(1.+ctcur)
      pt=PCM*SQRT(1.-ctcur**2)
      zco(1)=0.
      xco(1)=0.
      xsl(1)=pt/pl
      itry=0
      ih3lim=0
C
 100  itry=itry+1
      lost3=0
C
      IF(IPRI.GT.1) THEN
         WRITE(6,*) 'B3OPT: itry,B=',itry,(HM(i)/PAR_MAG(4,i),i=1,3)
     +             ,xsl(1)
      ENDIF
      x=xco(1)
      z=zco(1)
      DO im=1,3
         zco(im+1)=PAR_MAG(1,im)
         xco(im+1)=x+xsl(im)*(zco(im+1)-z)
C     
C---  Split the quad into thin slices
C
         z1=PAR_MAG(1,im)-PAR_MAG(3,im)/2.
         x=x+xsl(im)*(z1-z)
         z=z1
         zst=PAR_MAG(3,im)/nsplq/2.
         sl=xsl(im)
         qsl=HM(im)/pl/REAL(nsplq)
         xmx=0.
         DO i=1,nsplq
            rr=x+zst*sl
            dsl=-qsl*rr
            sl=sl+dsl
            x=rr+zst*sl
            xmx=MAX(xmx,x)
            z=z+zst*2.
         END DO
C
C         WRITE(6,*) 'x,z',x,z,qsl
         xsl(im+1)=sl
C     
C         WRITE(6,*) 'im',im,xmx,PAR_COL(3,im)
         IF(xmx.GT.PAR_COL(3,im)-DTOLER(1)) THEN
            IF(im.LT.3.OR.itry.GT.1) THEN
C               IF(im.EQ.3)
               IF(IPRI.GT.1) THEN
                  WRITE(6,*) 'im,XMX',im,xmx,PAR_COL(3,im),DTOLER(1)
               ENDIF
               GO TO 999
            ENDIF
         ENDIF
      END DO
C
C---  Find the angular acceptance
C
      slmin=-1.
      slmax=1.
      DO im=4,7
         zco(im+1)=PAR_COL(1,im)
         xsl(im+1)=xsl(im)
         xco(im+1)=x+xsl(im)*(zco(im+1)-z)
         x=xco(im+1)
         z=zco(im+1)
         slope1=(PAR_COL(2,im)+DTOLER(2)-xco(4))/(zco(im+1)-zco(4))
         slmin=MAX(slmin,slope1)
         slope2=(PAR_COL(3,im)-DTOLER(3)-xco(4))/(zco(im+1)-zco(4))
         slmax=MIN(slmax,slope2)
         IF(IPRI.GT.1) THEN
            WRITE(6,3050) im,xco(4),xco(im+1)
     +        ,slope1,slope2,slmin,slmax
 3050       FORMAT('im,xco',I4,2F9.4,2X,2F10.5,2X,2F10.5)
         ENDIF
C         WRITE(6,*) im,PAR_COL(2,im)-xco(4),PAR_COL(3,im)-xco(4)
C     +        ,zco(im+1)-zco(4),slope1,slope2,slmin,slmax
      END DO
C
      SLACEPT=slmax-slmin
      IF(IPRI.GT.1) THEN
         WRITE(6,3100) 'zco',zco
         WRITE(6,3100) 'xco',xco,slmin,slmax
C         WRITE(6,3100) 'xsl',xsl
      ENDIF
 3100 FORMAT(1X,A4,8F10.5,' slimn/max ',2F8.5)
      IF(SLACEPT.LE.0.) THEN
         IF(itry.GT.1.AND.ih3lim.NE.0) 
     +          WRITE(6,*) ' Strange.. miss at try=',itry,HM
         GO TO 999
      ENDIF
C
      h3max=HM(3)-(slmin-xsl(4))*pl/xco(4)
      h3min=HM(3)-(slmax-xsl(4))*pl/xco(4)
      IF(h3max.LT.H3LIM(1).OR.h3min.GT.H3LIM(2)) THEN
C         WRITE(6,*) 'h3min,max',h3min,h3max,H3LIM
         GO TO 999
      ENDIF
      h3min=MAX(h3min,H3LIM(1))
      h3max=MIN(h3max,H3LIM(2))
C
      dsl=(slmax+slmin)/2.-xsl(4)
      dhm3=-dsl*pl/xco(4)
      hm3=HM(3)+dhm3
      IF(hm3.LT.H3LIM(1)) THEN
         ih3lim=1
         hm3=H3LIM(1)
      ELSE IF(hm3.GT.H3LIM(2)) THEN
         ih3lim=2
         hm3=H3LIM(2)
      ELSE
         ih3lim=0
      ENDIF
      dhm3=hm3-HM(3)
      HM3AC(1)=h3min-hm3
      HM3AC(2)=h3max-hm3
C      HM3AC=SLACEPT/2.*pl/xco(4)
      HM(3)=hm3
         
      IF(itry.GT.10) WRITE(6,3150) itry,dsl,dhm3
     +              ,(HM(i)/PAR_MAG(4,i),i=1,3)
 3150 FORMAT(' B3OPT try=',I3,2E12.3,2X,'B=',3F8.3)
C
C      WRITE(6,*) 'OK',itry,SLACEPT,dsl,dhm3,HM(3)
      IF(IPRI.GT.1) THEN
         WRITE(6,*) 'B3OPT try',itry,SLACEPT,HM(3)/PAR_MAG(4,3),dsl
      ENDIF
C
C---   Next iterration
C
      IF(ABS(dsl).GT.1.E-4.AND.itry.LT.20.AND.
     +   (itry.LT.3.OR.ih3lim.EQ.0)) GO TO 100   ! try again (fine tuning)
C
      IF(IPRI.GT.1) THEN
         WRITE(6,*) 'B3OPT OK ',itry,SLACEPT,HM(3)/PAR_MAG(4,3)
      ENDIF
C
      RETURN
C
 999  CONTINUE
      SLACEPT=-1.
C
      END
