      REAL FUNCTION ACC_B1B3(B1,B3)
C
C ===     Magnet optimization for Moller
C ---     INPUT: m_opt.in
C
      IMPLICIT NONE
      REAL B1,B3
C
      COMMON/CMPAR/ PAR_MAG(8,4),PAR_COL(5,7),DTOLER(3)
      REAL PAR_MAG,PAR_COL,DTOLER
C
      COMMON/CMOPT/ EBEAM,PCM,PI
      REAL          EBEAM,PCM,PI
C
      VECTOR p_mag(10)
C
      REAL    e0,x1,x2,y1,y2,z1,z2,dc(3)
      INTEGER id,ifl,nx,ny
C
      REAL hm(4),toler,ctout
      INTEGER kfl,ktol,ipri
C
      INTEGER lrec,ista,ifirst
      DATA ifirst/1/
C
C     ------------------------------------------------------------------
C
      IF(ifirst.EQ.1) THEN
         ifirst=0
C         OPEN(1,FILE='m_opt.in',STATUS='OLD',IOSTAT=ista)
C         IF(ista.NE.0) THEN
C            WRITE(6,*) ' *** Error openinig m_opt.in'
C            GO TO 999
C         ENDIF
C
C         READ(1,*,IOSTAT=ista) e0,id,ifl,nx,ny,x1,x2,y1,y2,z1,z2,dc
C         IF(ista.NE.0) THEN
C            WRITE(6,*) ' *** Error reading m_opt.in'
C            GO TO 999
C         ENDIF
C         CLOSE(1)
C
         CALL M_OINI(dc(1))
         PI=ACOS(0.)*2.
      ENDIF
C      B1=2.0
      e0=p_mag(1)
      toler=p_mag(2)
      kfl=INT(p_mag(3)+.1)
      ktol=INT(p_mag(4)+.1)
      ipri=INT(p_mag(5)+.1)
C
      EBEAM=e0
      PCM=SQRT(2.*0.000511*EBEAM)/2.
C
      hm(1)=B1*PAR_MAG(4,1)
      hm(2)=0.
      hm(3)=B3*PAR_MAG(4,3)
C
      CALL QPOINT(hm,toler,kfl,ktol,ctout,ipri)
C      CALL M_OPT3(e0,id,ifl,nx,ny,x1,x2,y1,y2,z1,z2)
      ACC_B1B3=ctout
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
