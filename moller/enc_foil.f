      SUBROUTINE ENC_FOIL(IZRUN,HELM,JENC,XY,ITAR,XYC,POL)
C
C----        For the target holder 2: sliding frame, 5 foils
C ===  Returns the target parameters using:
C      IZRUN   - run number
C      HELM    - Helmoltz coil current
C      JENC(2) - 2 encoders (VtoF) from the scalers, in 33ms window
C
C  OUTPUT: 
C ==   XY(2)  - absolute coordinates (from target 5), cm
C      ITARG  - target number (6 -empty)
C      XYC(2) - relative coordinates  with respect to the current target center
C      POL    - target polarization at this point
C ==     IXY = 1 - X
C            = 2 - Y(Z)
C        IFL = 0 - absolute coordinate (from target 5) in cm
C            = 1 - relative coordinate with respect to the current target center
C            = 2 - returns the target number
C      Returns the signal average per quad
C
      IMPLICIT NONE
      INTEGER IRUN,JENC(2),ITAR
      REAL    HELM,XY(2),XYC(2),POL
C
      INTEGER mxtar
      PARAMETER (mxtar=6)
      INTEGER iq,i,it1,it2,itar
      REAL enc(2),xytcm(2,mxtar),xyten(2,mxtar),xtdis,xytsl(2),xyorcm(2)
     +    ,xyoren(2),xy(2),yen1,yen2,ycm1,ycm2
C
C     ------------------------------------------------------------------
C
      ITAR=0
      POL=1.E-6
C
      IF(IZRUN.LE.0) GO TO 999
C
      it1=5 ! zero point (convention)
      it2=3 ! ref. measurement
      xtdis=-8.89 ! distance between target centers (cm)
      xytcm(1,it1)=0.
      xyorcm(1)=xytcm(1,it1)
      xyorcm(2)=3.15 ! target center in Z
C
C ---     History of measuremnts (by scanning across the foil, see foil_center.kumac)
C
      xyten(1,it1)=3374.
      xyten(1,it2)=7478.
      IF(IZRUN.LT.12220) THEN
         xyten(1,it1)=3365.
         xyten(1,it2)=7475.
      ELSEIF(IZRUN.LT.12258) THEN
         xyten(1,it1)=3402.
         xyten(1,it2)=7578.
      ELSEIF(IZRUN.LT.12374) THEN
         xyten(1,it1)=3417.
         xyten(1,it2)=7581.
      ELSE
         xyten(1,it1)=3464.
         xyten(1,it2)=7627.
      ENDIF
C
      xyoren(1)=xyten(1,it1)
      DO i=1,mxtar
        xytcm(1,i)=xytcm(1,it1)+xtdis*(i-it1)
        xytcm(2,i)=xyorcm(2)
      ENDDO
      xytsl(1)=(xytcm(1,it2)-xytcm(1,it1))/(xyten(1,it2)-xyten(1,it1))
C
      DO i=1,mxtar
        xyten(1,i)=xyten(1,it1)+xtdis*(i-it1)/xytsl(1)
      ENDDO
C      WRITE(6,FMT='(6F10.2)') (xytcm(1,i),i=1,mxtar)
C      WRITE(6,FMT='(6F10.1)') (xyten(1,i),i=1,mxtar)
C
      IF(IZRUN.LT.12220) THEN
        ycm1=0.1
        ycm2=5.55
        yen1=252400./30.
        yen2=46600./30.
      ELSE
        ycm1=0.15
        ycm2=5.55
        yen1=256430./30.
        yen2=47150./30.
      ENDIF
      xytsl(2)=(ycm2-ycm1)/(yen2-yen1)
      xyoren(2)=yen1+(xyorcm(2)-ycm1)/xytsl(2)
C
      nel=NELEM
C
      DO i=1,2
         enc(i)=JENC(i)
         XY(i)=(enc(i)-xyoren(i))*xytsl(i)
      ENDDO
C
      ITAR=it1-INT((XY(i)-xyorcm(1)+ABS(xtdis)/2.)/ABS(xtdis))
      IF(ITAR.GT.0.AND.ITAR.LE.mxtar) THEN
         DO i=1,2
            XYC(i)=XY(i)-xytcm(1,itar)
         ENDDO
      ENDIF
C
C---   Convert to the foil coordinate
C
      XY(2) =-XY(2)
      XYC(2)=-XYC(2)
C
C
 999  RETURN
C
      END

