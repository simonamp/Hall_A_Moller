      REAL FUNCTION CAL_CALIB(NCAL,IADC1,ICH1,ICH2,ITIM1,ITIM2,ICORR,ID)
C
C---   Calorimeter: calibration
C                   ID - histogram start up
C                   ITIM1,ITIM2 - time limits for the hodoscope
C                   IADC1 - 1st ADC number
C
      IMPLICIT NONE
      INTEGER NCAL,ICH1,ICH2,ITIM1,ITIM2,ICORR,ID,IADC1
      INCLUDE ?
C
      VECTOR CALIBTMP(36)
      VECTOR PEDES(36)
      VECTOR ICHLIM(2,5)
      VECTOR ILGLIM(2,5)
C
      INTEGER i,nh,ih(16),jh,i1,i2,irow,icol,imx,irowmx

      INTEGER    mxcal
      PARAMETER (mxcal=25)
      REAL    enlg(mxcal)
      REAL    enpar,emax,etot,ebeam
C
      CAL_CALIB=0.
      IF(NADC.LE.0) GO TO 999 
      IF(NTDC.EQ.0) GO TO 999
C
      nh=0
      DO i=1,16
         ih(i)=0
      ENDDO
C
      i1=ICH1+16
      i2=ICH2+16
C
      DO i=1,NTDC
         IF(ITCH(i).GE.i1.AND.ITCH(i).LE.i2) THEN
            IF(ITIM(i).GE.ITIM1.AND.ITIM(i).LE.ITIM2) THEN
               nh=nh+1
               ih(nh)=ITCH(i)-16
            ENDIF
         ENDIF
      ENDDO
C
C---       1 Sept 1999: for the left column the hodoscope was hardly hit 
C
      IF(IRUN.GT.7570.AND.IRUN.LT.7619.AND.nh.EQ.0) THEN
         IF(IADC1.GT.12) THEN
            IF(IADC(0+IADC1)
     +           +IADC(4+IADC1)*0+IADC(8+IADC1)*0
     +           +IADC(12+IADC1).GT.150) THEN
               IF(IADC(1).GT.150.AND.IADC(2).GT.150.AND.
     +            IHIT(4).NE.0) THEN
                  nh=nh+1
                  ih(nh)=0
               ELSEIF(IADC(4).GT.150.AND.IHIT(4).NE.0) THEN
                  nh=nh+1
                  ih(nh)=18
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      IF(nh.GT.1) GO TO 999
C
C---     Find the module
C
C      WRITE(6,*) 'NCAL..',NCAL,IADC1,ICH1,ICH2,ITIM1,ITIM2,ICORR,ID
      emax=0.
      etot=0.
      imx=0
      DO i=1,NCAL
         enlg(i)=(IADC(IADC1-1+i)-PEDES(IADC1-1+i))*CALIBTMP(IADC1-1+i)
         IF(enlg(i).GT.emax) THEN
            imx=i
            emax=enlg(i)
         ENDIF
         etot=etot+enlg(i)
      ENDDO
C
      IF(emax.LT.0.4) GO TO 999
C
      irowmx=0
      DO i=1,5
         IF(imx.GE.ILGLIM(1,i).AND.imx.LE.ILGLIM(2,i)) irowmx=i
      ENDDO
C
      IF(irowmx.EQ.0) GO TO 999
C
C--        In the period 8200-8300 the hodoscope did not cover the top row of LG
C
      IF(IRUN.GT.8200.AND.IRUN.LT.8300) THEN
         IF(irowmx.EQ.1) THEN
C            write(6,*) imx,irowmx,nh,ih(1)
            IF(nh.EQ.0) THEN
               nh=nh+1
               ih(nh)=-5  !  roughly the center of the LG module
            ELSE
               GO TO 999
            ENDIF
         ENDIF
      ENDIF
C
      IF(nh.NE.1) GO TO 999
C
C---     Find the row
C
      irow=0
      jh=ih(1)
      DO i=1,4
         IF(jh.GE.ICHLIM(1,i).AND.jh.LE.ICHLIM(2,i)) irow=i
      ENDDO
C      WRITE(6,*) jh,irow
C
      IF(irowmx.NE.irow) GO TO 999
C
C---     beam energy
C
      IF(IRUN.LT.7570) THEN
         ebeam=3.4
         enpar=ebeam/2.
C         IF(ICORR.NE.0) enpar=enpar*26.8/(26.8-(jh-6.7)*0.5)
         IF(ICORR.NE.0) enpar=enpar*26.8/(26.8-(jh-9.25)*0.5)
      ELSE IF(IRUN.GT.7570.AND.IRUN.LT.7619) THEN
         ebeam=4.108
         enpar=ebeam/2.
         IF(ICORR.NE.0) enpar=enpar*26.8/(26.8-(jh-7.)*0.5)
      ELSE IF(IRUN.GT.8200.AND.IRUN.LT.8300) THEN
         ebeam=4.803   
         enpar=ebeam/2.
         IF(ICORR.NE.0) enpar=enpar*26.8/(26.8+(jh-5.)*0.5)
      ENDIF
C
      IF(IADC1.LT.13) THEN
         enpar=ebeam-enpar
      ENDIF
C
      etot=etot/enpar
C
      IF(irowmx.NE.irow) THEN
CC           IF(INT((imx-1)/3+1).NE.irow) THEN
C          WRITE(6,*) 'Mismatch ',idnevt,imx,irow,irowmx,jh
C     +             ,(enlg(i),i=1,NCAL)
C         GO TO 999
      ENDIF
C      WRITE(6,*) 'Mismatch ',idnevt,imx,irow,irowmx,jh
C
      CALL HF1(ID+imx,etot,1.)
      CALL HF1(ID+100+imx,(etot-emax/enpar)/etot,1.)
      CALL HF1(ID+200+imx,enpar,1.)
      CALL HF2(ID+300+imx,etot,jh+0.1,1.)
      CALL HFILL(ID+400,(jh-0.5)*0.5,etot,1.)
      CALL HFILL(ID+400+imx,(jh-0.5)*0.5,etot,1.)
      CALL HF1(ID+500+imx,IADC(IADC1-1+imx)-PEDES(IADC1-1+imx),1.)
C
      CAL_CALIB=etot
C
 999  CONTINUE
      END
