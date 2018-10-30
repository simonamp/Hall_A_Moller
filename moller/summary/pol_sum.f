      SUBROUTINE POL_SUM
C
C===     Reads a table (linked to input.tab) with polarization data 
C===                                   (see  happex_1_sum_2.tab) and:
C===      - combines the 20 and 160 degrees measurements
C         - estimates the beam attenuation factor using - the laser attenuation, 
C                              slit size and the beam current 
C
      IMPLICIT NONE
C
      REAL ATT_LASER,ATT_SLIT
C
      INTEGER    mxent,mxmea
      PARAMETER (mxent=200,mxmea=mxent/2)
C
      INTEGER i,j,k,iost,ient,nent,imea,nmea,kmea,ini
      REAL  avp(2),eavp(2)
C
      INTEGER idate(3,mxent),irune(2,mxent),ihwave(mxent),lstae(2,mxent)
      INTEGER idat(3,mxmea),irun(mxmea),ihwav(mxmea),lsta(2,mxmea)
      REAL    ange(2,mxent),pole(2,mxent),epole(2,mxent),beame(mxent)
     +       ,attene(3,mxent),chi2e(mxent)
      REAL    pol(3,mxmea),epol(3,mxmea),beam(mxmea),atten(3,mxmea)
     +       ,chi2(mxent),atfac(mxmea)
C
      COMMON/CNTUP/ idat,irun,ihwav,lsta
     +             ,pol,epol,beam,atten,chi2,atfac
C
      INTEGER lrec,id
C
C     ------------------------------------------------------------------
C
      OPEN(UNIT=1,FILE='input.tab',STATUS='OLD',IOSTAT=iost)
      IF(iost.NE.0) THEN
         WRITE(6,*) ' *** Error: no file input.tab'
         GO TO 999
      ENDIF
      REWIND 1
C
      nent=0
 10   i=nent+1
      READ(1,*,END=20) (idate(j,i),j=1,3),(irune(j,i),j=1,2)
     +                  ,(ange(j,i),pole(j,i),epole(j,i),j=1,2) 
     +                  ,chi2e(i),beame(i) 
     +                  ,attene(1,i),k,attene(3,i)
     +                  ,ihwave(i),(lstae(j,i),j=1,2)
      nent=i
      attene(2,i)=k
      IF(nent.LT.mxent) GO TO 10
 20   CONTINUE
      CLOSE(UNIT=1)
C
C      WRITE(6,*) nent,' lines read'
      nmea=0
      kmea=0
      DO ient=1,nent+1
C         WRITE(6,*) ient,nmea
         ini=0
         IF(nmea.EQ.0.OR.ient.EQ.nent+1) THEN
            ini=1
         ELSE IF(nmea.GT.0) THEN
            DO i=1,3
               IF(idate(i,ient).NE.idat(i,nmea)) ini=i+1
            ENDDO
         ENDIF
         IF(ini.NE.0) THEN
            IF(kmea.GT.0) THEN
               DO i=1,2
                  pol(i,nmea)=avp(i)/eavp(i)
                  epol(i,nmea)=SQRT(1./eavp(i))
               ENDDO
               write(6,*) 'i=',i,ini,kmea,nmea
               epol(3,nmea)=1./SQRT(1./epol(1,nmea)**2+
     +                              1./epol(2,nmea)**2)
               pol(3,nmea)=(pol(1,nmea)/epol(1,nmea)**2+
     +                      pol(2,nmea)/epol(2,nmea)**2)*
     +           epol(3,nmea)**2
C
C---              The errors are not independent!
C
               epol(3,nmea)=epol(3,nmea)*SQRT(2.)
C
               atfac(nmea)=1.
               IF(atten(2,nmea).GT.1..AND.
     +            atten(3,nmea).GT.1.) THEN
                  atfac(nmea)=beam(nmea)/0.44
     +                 *ATT_LASER(500.)/ATT_LASER(atten(2,nmea))
     +                 *ATT_SLIT(15.4)/ATT_SLIT(atten(3,nmea))
               ENDIF
            ENDIF
C
            IF(ient.LE.nent) THEN
               kmea=0
               DO i=1,2
                  avp(i)=0.
                  eavp(i)=0.
               ENDDO
               nmea=nmea+1
C
               DO i=1,3
                  idat(i,nmea)=idate(i,ient)
               ENDDO
               irun(nmea)=irune(1,ient)
               ihwav(nmea)=ihwave(ient)
               beam(nmea)=beame(ient)
               chi2(nmea)=chi2e(ient)
               DO i=1,3
                  atten(i,nmea)=attene(i,ient)
               ENDDO
               DO i=1,2
                  lsta(i,nmea)=lstae(i,ient)
               ENDDO
            ENDIF
         ENDIF
C
         IF(ient.LE.nent) THEN
            kmea=kmea+1
            DO i=1,2
               avp(i)=avp(i)+pole(i,ient)/epole(i,ient)**2
               eavp(i)=eavp(i)+1./epole(i,ient)**2
            ENDDO
         ENDIF
C

      ENDDO
C
      WRITE(6,1500) 
 1500 FORMAT(3X,'m   d   h',3X,'1-st run',3X,'Pol 1',12X,'Pol 2',12X
     +      ,'Pol aver',5X,'chi2   uA   Las:power atten slit'
     +      ,5X,'loss  1/2wave B   C')
      DO i=1,nmea
         WRITE(6,2000) (idat(j,i),j=1,3),irun(i)
     +                 ,(pol(j,i),epol(j,i),j=1,3) 
     +                 ,chi2(i),beam(i) 
     +                 ,(atten(j,i),j=1,3),atfac(i)
     +                 ,ihwav(i),(lsta(j,i),j=1,2)
 2000    FORMAT(3(2X,I2),3X,I5,3(2X,F6.2,' +/-',F5.2),2X,F4.1,F7.3
     +        ,4X,F6.1,F6.0,F6.2,1X,F7.2,3X,I1,3X,2I4)
      ENDDO
C
      lrec=1024
C      CALL HROPEN(1,'SUMPOL','sum_pol.nt','N',lrec,iost)
      IF(iost.NE.0) THEN
         WRITE(6,*) ' *** Error: cannot open the ntuple'
         GO TO 999
      ENDIF
C
      id=1
C      CALL HBNT(id,'Polarization summary',' ')
C      CALL HBNAME(id,'pol', idat(1,1)    ,'idat(3)')
C      CALL HBNAME(id,'pol', idat(1,1)    ,'idat(3)')
C
 999  RETURN
      END
C
      INCLUDE 'att_laser.f'
      INCLUDE 'att_slit.f'
