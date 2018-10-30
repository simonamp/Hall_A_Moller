      REAL FUNCTION CAL_XYPOS(NSIZ,IAD1,METH,IXYP)
C
C---   Calorimeter: X and Y positions of the cluster in LG 
C                   Cluster =3x3 modules
C                   The calorimeter is rectangular NSIZ*NSIZ, starting with IADC(IAD1) 
C                   METH=0 - baricenters
C                        1 - LOG
C                   IXYP =1 - X
C                         2 - Y
C                   
      IMPLICIT NONE
      INTEGER NSIZ,IAD1,METH,IXYP
      INCLUDE ?
C
      VECTOR PEDES(36)
      VECTOR CALIB(36)
C
      INTEGER ix,iy,ixy,i,j,iok,imx,imxxy(2)
C
      REAL    amp,emx,tot,exy,ampxy(2,10),ampl,totl
      REAL  amall(4,4)
C
      CAL_XYPOS=-20.
      IF(NADC.LE.0) GO TO 999
C
      IF(NSIZ.LT.1.OR.IAD1.LT.1.OR.
     +   IXYP.LT.1.OR.IXYP.GT.2) THEN
         WRITE(6,*) ' CAL_ADCSUM: Wrong parameters ',NSIZ,IAD1,IXYP
         GO TO 999
      ENDIF
C
      DO ixy=1,NSIZ
         DO i=1,2
            ampxy(i,ixy)=0.
         ENDDO
      ENDDO
C
      j=IAD1-1
      emx=0.
      DO iy=1,NSIZ
         DO ix=1,NSIZ
            j=j+1
            amp=MAX(IADC(j)-PEDES(j),0.)*CALIB(j)
            amall(ix,iy)=amp
            IF(amp.GT.emx) THEN
               emx=amp
               imxxy(1)=ix
               imxxy(2)=iy
            ENDIF
            ampxy(1,ix)=ampxy(1,ix)+amp
            ampxy(2,iy)=ampxy(2,iy)+amp
         ENDDO
      ENDDO
C
C
      IF(emx.LT.0.05) GO TO 999
C
      imx=imxxy(IXYP)
      IF(imx.EQ.1.OR.imx.EQ.NSIZ) THEN
         CAL_XYPOS=REAL(imx)-20.
C
         GO TO 999
C
      ENDIF
C
      IF(METH.EQ.0) THEN
         tot=0.
         exy=0.
         DO ixy=imx-1,imx+1
            amp=ampxy(IXYP,ixy)
            tot=tot+amp
            exy=exy+amp*REAL(ixy)
         ENDDO
         exy=exy/tot
      ELSEIF(METH.EQ.1) THEN
         tot=0.
         totl=-2.*ALOG(2.)*2
         exy=0.
         DO ixy=imx-1,imx+1
            amp=ampxy(IXYP,ixy)
            tot=tot+amp
            IF(ixy.NE.imx) THEN
               ampl=ALOG(amp+1.E-20)
               totl=totl-ampl
               exy=exy+ampl*REAL(ixy-imx)
            ENDIF
         ENDDO
         totl=totl+2.*ALOG(tot)
         exy=exy/2./totl+imx
      ENDIF
C
C      WRITE(6,*) 'iev=',IDNEVT 
C      DO i=1,4
C         WRITE(6,2100) (amall(j,i),j=1,4),ampxy(2,i)
C      ENDDO
 2100 FORMAT(' amp   ',4F7.3,2X,F7.3)
 2200 FORMAT(' sum ',A1,' ',4F7.3)
C      WRITE(6,2200) 'x',(ampxy(1,i),i=1,4) 
C
      IF(tot.GT.15000.) THEN
         WRITE(6,*) imxxy 
         WRITE(6,*) (ampxy(1,i),i=1,4) 
         WRITE(6,*) (ampxy(2,i),i=1,4)
      ENDIF 
C
      CAL_XYPOS=exy
C
 999  CONTINUE
      END






