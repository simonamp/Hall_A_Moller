      REAL FUNCTION CAL_ADCSUM(NSIZ,IAD1,I1,NX,NY,ICAL,IPRI)
C
C---   Calorimeter: Sum of NX*NY ADC-ped values, starting with the element I1
C                   The calorimeter is rectangular NSIZ*NSIZ, starting with IADC(IAD1) 
C                   ICAL>0 - calculate the energy (use CALIB)
C                   IPRI=1 - prints the events
C                   
      IMPLICIT NONE
      INTEGER NSIZ,IAD1,I1,NX,NY,ICAL,IPRI
      INCLUDE ?
C
      VECTOR PEDES(36)
      VECTOR CALIB(36)
C
      INTEGER ix,iy,ix1,iy1,ix2,iy2,i,j
C
      REAL    amp,tot,ampm(10,10)
C
      CAL_ADCSUM=0.
      IF(NADC.LE.0) GO TO 999
C
      IF(NSIZ.LT.1.OR.I1.LT.1.OR.I1.GT.NSIZ**2.OR.
     +  NX.LT.1.OR.NY.LT.1) THEN
         WRITE(6,*) ' CAL_ADCSUM: Wrong parameters ',NSIZ,I1,NX,NY
         GO TO 999
      ENDIF
C
      iy1=(I1-1)/NSIZ+1
      ix1=I1-(iy1-1)*NSIZ
C
      IF(IPRI.NE.0) THEN
         DO iy=1,NSIZ
            DO ix=1,NSIZ
               ampm(ix,iy)=0.
            ENDDO
         ENDDO
      ENDIF
C
C---     Find the max module
C
      tot=0
      DO iy=1,NY
         iy2=iy1+iy-1
         IF(iy2.LE.NSIZ) THEN
            DO ix=1,NX
               ix2=ix1+ix-1
               IF(ix2.LE.NSIZ) THEN
                  j=IAD1-1+(iy2-1)*NSIZ+ix2
                  amp=IADC(j)-PEDES(j)
                  IF(ICAL.NE.0) amp=amp*CALIB(j)
                  tot=tot+amp
                  ampm(ix2,iy2)=amp
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
      CAL_ADCSUM=tot
      IF(IPRI.GT.0) THEN
         WRITE(6,1100) IDNEVT,tot
 1100    FORMAT(/' Event',I8,'  Full energy=',F7.3)
         DO iy=1,NSIZ
            WRITE(6,1200) (ampm(ix,iy),ix=1,NSIZ)
 1200       FORMAT(5X,4F7.3)
         ENDDO
      ENDIF
C
 999  CONTINUE
      END


