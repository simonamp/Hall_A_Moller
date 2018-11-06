      PROGRAM RAWREAD 
C
C===  Reads a RAW coda file from the unit 1 
C
C
      IMPLICIT NONE
C
C===      Define coda I/O routines
C
      EXTERNAL EVOPEN,EVREAD,ECWRITE,EVCLOSE,EVIOCTL
      INTEGER  EVOPEN,EVREAD,ECWRITE,EVCLOSE,EVIOCTL
C
      INTEGER    mxlenb,mxeve,mxhbook
      PARAMETER (mxlenb=8192,mxeve=10000000,mxhbook=200000)
C
      INTEGER mhbook
      COMMON/PAWC/ mhbook(mxhbook)
C
      INCLUDE 'inc/cevcoda.inc'
      INCLUDE 'inc/crntup.inc'
      INCLUDE 'inc/crflag.inc'
C
      INTEGER*4 ibuf(mxlenb)
      INTEGER  istat,ihandle,lenbuf,iost,nrec,neve,igo,nwor
      INTEGER i,icur,lenn,ip1,ip2
C
      CHARACTER cbuf(mxlenb)*4
      EQUIVALENCE (ibuf,cbuf)
C     
      CHARACTER fnam*32
C
      INTEGER*4 i4,j4,k4
      INTEGER*2 i2(2),j2(2),k2(2)
      BYTE      i1(4),j1(4),k1(4)
      EQUIVALENCE (i4,i2(1),i1(1)),(j4,j2(1),j1(1)),(k4,k2(1),k1(1))
C
      IWREVENT=1
C
      fnam='raw.dat'//CHAR(0)
      OPEN(UNIT=1,FILE=fnam,STATUS='OLD',IOSTAT=iost)
      IF(iost.NE.0) THEN
        WRITE(6,*) '*** ERROR=',iost,' opening file=',fnam
        GO TO 999
      ENDIF
      CLOSE(UNIT=1)
C
      istat=EVOPEN(fnam,'R',ihandle)
C      WRITE(6,*) 'istat,ihandle=',istat,ihandle
      IF(istat.NE.0) THEN
        WRITE(6,*) '*** ERROR EVOPEN=',istat,' opening file=',fnam
        GO TO 999
      ENDIF
C
C---   Open NTUPLE
C
      CALL HLIMIT(mxhbook)
      CALL RNTUPIN(iost)
      IF(iost.NE.0) GO TO 999
C
C==    Check the low-high endian
C
      i1(1)=1
      ILOWEND=0
      IF(i4.EQ.1) THEN
         DO i=1,2
            J2ORD(i)=i
         END DO
         DO i=1,4
            J1ORD(i)=i
         END DO
      ELSE
         ILOWEND=1
         DO i=1,2
            J2ORD(i)=3-i
         END DO
         DO i=1,4
            J1ORD(i)=5-i
         END DO
      ENDIF
C      WRITE(6,*), 'i4=',i4
C
      nrec=0
      neve=0
      igo=1
C
      IRUNCUR=0
      ITIMST=0
      ITIMCUR=0
C
 10   CONTINUE
      lenbuf=mxlenb
      istat=EVREAD(ihandle,ibuf(1),lenbuf)
      IF(istat.NE.0) THEN
        WRITE(6,*) '*** ERROR EVREAD=',istat,' at record=',nrec+1
        igo=0
        GO TO 990
      ENDIF
C
      LENEVE=0
      IEVTYP=0
      IEVNUM=0
      IEVCLAS=0
      IEVSTAT=0
      NFRAG=0
C
      CALL REVEINI
C
      nrec=nrec+1
      nwor=ibuf(1)+1
      IF(nrec.LT.20) THEN
C        WRITE(6,*) 'Event ',nrec
C        WRITE(6,FMT='(8(2X,Z9))') (ibuf(i),i=1,nwor)
C        IF(nrec.LT.6) WRITE(10,*) (cbuf(i),i=5,nwor)
C        IF(nrec.LT.6) WRITE(6,FMT='(32A4)') (ibuf(i),i=5,nwor)
      ENDIF
C
      IF(nwor.LT.1) THEN
        WRITE(6,3000) nrec,nwor
        GO TO 99
      ENDIF
C
      LENEVE=nwor
      i4=ibuf(2)
      j4=0
      j2(J2ORD(1))=i2(J2ORD(2))
      IEVTYP=j4
      j4=0
      j1(J1ORD(1))=i1(J1ORD(2))
      IEVDTYP=j4
C
C---     Check the tag
C     
      k4=0
      k1(J1ORD(1))=i1(J1ORD(1))
C-      WRITE(6,*) 'k4=',k4
C-      WRITE(6,FMT='(Z3)') k1(J1ORD(1))
        IF(k4.NE.X'CC') THEN
        WRITE(6,3100) nrec,i1(J1ORD(1))
        GO TO 99
      ENDIF
C
      icur=2
C
C---     Check tha data type
C
      IF(IEVDTYP.EQ.1) THEN
C
C---       Special event
C
         IF(IEVTYP.EQ.16) THEN
C---          Sync
            ITIMCUR=ibuf(icur+1)
            icur=icur+4
C
         ELSE IF(IEVTYP.EQ.17) THEN
C---          Prestart
            ITIMCUR=ibuf(icur+1)
            IRUNCUR=ibuf(icur+2)
            icur=icur+3
C
         ELSE IF(IEVTYP.EQ.18) THEN
C---          Go
            ITIMST=ibuf(icur+1)
            ITIMCUR=ibuf(icur+1)
            icur=icur+3
C
         ELSE IF(IEVTYP.EQ.19) THEN
C---          Pause
            ITIMCUR=ibuf(icur+1)
            icur=icur+3
C
         ELSE IF(IEVTYP.EQ.20) THEN
C---          End
            ITIMCUR=ibuf(icur+1)
            icur=icur+3
         ENDIF
C
      ELSE IF(IEVDTYP.EQ.16) THEN
C
C---       Our service event
C
         IF(IEVTYP.GT.16) THEN
            WRITE(6,3150) nrec,IEVTYP
            GO TO 99
         ENDIF
C
C---       Physics event
C
         lenn=ibuf(icur+1)
         i4=ibuf(icur+2)
         j4=0
         j1(J1ORD(1))=i1(J2ORD(1))
         ip1=j4
C
C---           Event builder ID bank?
C
         IF(lenn.GT.3.AND.i2(J2ORD(2)).EQ.X'C000'.AND.ip1.EQ.0) THEN
            IEVNUM=ibuf(icur+3)
            IEVCLAS=ibuf(icur+4)
            IEVSTAT=ibuf(icur+5)
            icur=icur+lenn+1
         ENDIF
C
C---           Readout the controller data bank (may be several)?
C
 30      IF(icur+2.GT.LENEVE) GO TO 99
C
         lenn=ibuf(icur+1)
         IF(icur+1+lenn.GT.LENEVE) THEN
            WRITE(6,3200) nrec,icur,lenn,LENEVE
            GO TO 99
         ENDIF
C
         i4=ibuf(icur+2)
         j4=0
         j1(J1ORD(1))=i1(J1ORD(2))
         ip1=j4
         j4=0
         j2(J2ORD(1))=i2(J2ORD(2))
         ip2=j4
         IF(ip1.NE.1) THEN
            WRITE(6,3300) nrec,icur,lenn,i4
            GO TO 99
         ENDIF
C
         NFRAG=NFRAG+1
         IFRAG=NFRAG
         LENFRAG(IFRAG)=lenn
         IROCFRAG(IFRAG)=IAND(ip2,31)
C
         icur=icur+2
         lenn=lenn-1      ! length of the data
         IF(lenn.GT.0) THEN
C
            IF(IROCFRAG(IFRAG).EQ.7.OR.
     +         IROCFRAG(IFRAG).EQ.16) CALL MOLDECEV(lenn,ibuf(icur+1))
C
            icur=icur+lenn
C
         ENDIF
C
         GO TO 30     ! in case there are another ROC data
C
      ENDIF
C
 99   CONTINUE
C
      CALL RNTUPEV
C
      IF(nrec.LT.1) THEN
        WRITE(6,1300) nrec,(ibuf(i),i=1,ibuf(1)+1)
        WRITE(6,1400) nrec,IRUNCUR,ITIMST,ITIMCUR,LENEVE,IEVDTYP
     +               ,IEVTYP,IEVNUM,IEVCLAS,IEVSTAT
     +               ,(IROCFRAG(i),LENFRAG(i),i=1,NFRAG)
      ENDIF
C
      IF(igo.NE.0.AND.nrec.LT.mxeve) GO TO 10
C
 990  CONTINUE
      WRITE(6,1200) nrec,neve
      CALL RNTUPCL
 999  CONTINUE
 1100 FORMAT(/' rec=',I8,' rec length=',I6
     +     ,' i4,i2,i1,irectyp ',Z10,2Z6,4Z5,4X,Z6
     +     /(2X,8Z10))
 1200 FORMAT(//' === END: read records ',I7,' events ',I6)
 1300 FORMAT(/' Event=',I7,' Data:'/(8Z9))
 1400 FORMAT(/' Event=',I7,' run=',I6,' time=',2I11,'  len=',I5
     +      ,' typ=',2I4,' ev num=',I7,' clas,stat=',2I8
     +      ,/(10X,'ROC=',I2,'  len=',I5))
 3000 FORMAT(' *** ERROR: event=',I7,' has a wrong lenght=',I5)
 3100 FORMAT(' *** ERROR: event=',I7,' has a wrong tag=',Z3
     +      ,' (should be CC)')
 3150 FORMAT(' *** ERROR: event=',I7,' unknown event type ',I6)
 3200 FORMAT(' *** ERROR: event=',I7,' length mismatch ',4I7)
 3300 FORMAT(' *** ERROR: event=',I7,' unknown bank, ...=',2I5,Z9)
      END
C
      SUBROUTINE RNTUPIN(IRET)
C
C===     Opens NTUPLE
C
      IMPLICIT NONE
      INTEGER  IRET
C
      INCLUDE 'inc/cevcoda.inc'
      INCLUDE 'inc/crntup.inc'
      INCLUDE 'inc/crntup1.inc'
      INCLUDE 'inc/crflag.inc'
C
      INTEGER  lrec,istat,id
      CHARACTER clena*2,clent*3,clentw*4
C
      lrec=1024
      IRET=0
      CALL HROPEN(2,'adctdc','adctdc.nt','N',lrec,istat)
      IF(istat.NE.0) THEN
         WRITE(6,*)'*** ERROR in RNTUPIN opening NTUPLE, istat=',istat
         IRET=1
         GO TO 999
      ENDIF
C
      id=1
      CALL HBNT(id,'Moller data',' ')
      WRITE(clena,FMT='(I2)') MXTRIG
C--      CALL HBNAME(id,'run',IRUNCUR,'irun[1,16000]')
      CALL HBNAME(id,'run',IRUNCUR,'irun[1,25000]')
      CALL HBNAME(id,'run',IEVDTYP,'idtyp[0,255]')
      CALL HBNAME(id,'run',IEVTYP,'ievtyp[0,255]')
      CALL HBNAME(id,'run',IRETC,'iret')
      CALL HBNAME(id,'run',ITRIG(1),'itrig('//clena//')[0,1]')
      CALL HBNAME(id,'run',ITIMTICK,'itick')
C
      WRITE(clena,FMT='(I2)') MXADC
      WRITE(clent,FMT='(I3)') MXTDC
      WRITE(clentw,FMT='(I4)') MXTDCW
      write(6,*) MXTDC,clent,MXTDCW,clentw
      CALL HBNAME(id,'adc',NADC,'nadc[0,'//clena//']')
      CALL HBNAME(id,'tdc',NTDC,'ntdc[0,'//clentw//']')
      IF(IWREVENT.GT.0) THEN
         CALL HBNAME(id,'adc',IADC,'iadc(nadc)[0,4095]')
         CALL HBNAME(id,'tdc',ITCH,'itch(ntdc)[0,'//clent//']')
         CALL HBNAME(id,'tdc',ITIM,'itim(ntdc)[0,60000]')
         CALL HBNAME(id,'tdc',ITED,'ited(ntdc)[0,1]')
      ENDIF
C
      WRITE(clena,FMT='(I2)') MXHCHA
      CALL HBNAME(id,'tdc',NHCHA,'nhcha[0,'//clena//']')
      CALL HBNAME(id,'tdc',IHIT,'ihit(nhcha)[0,1000]')
C
      WRITE(clena,FMT='(I2)') MXSCAL
      CALL HBNAME(id,'scal',NSCA,'nsca[0,'//clena//']')
      CALL HBNAME(id,'scal',ISCA,'isca(nsca)')
C
 999  CONTINUE
      RETURN
      END
C
      SUBROUTINE REVEINI
C     ******************************************************************
C     *                                                                *
C     *      Initialize an       event                                 *
C     *                                                                *
C     ******************************************************************
C
      IMPLICIT NONE
C
      INCLUDE 'inc/crntup.inc'
      INCLUDE 'inc/crntup1.inc'
      INTEGER j
C     
      NADC=0
      NTDC=0
      NSCA=0
      IRETC=0
      NHCHA=0
      ITIMTICK=0
      DO j=1,MXTRIG
         ITRIG(j)=0
      END DO
C
      RETURN
      END
C
      SUBROUTINE MOLDECEV(NWOR,IBUF)
C     ******************************************************************
C     *                                                                *
C     *      Decodes a "Moeller" event                                 *
C     *                                                                *
C     ******************************************************************
C
      IMPLICIT NONE
      INTEGER  NWOR,IBUF(NWOR)
C
      INCLUDE 'inc/cevcoda.inc'
      INCLUDE 'inc/crntup.inc'
      INCLUDE 'inc/crntup1.inc'
      INCLUDE 'inc/crflag.inc'
C
      INTEGER  i,icur,n,j,k
C
      INTEGER*4 i4
      INTEGER*2 i2(2),j2
      BYTE      i1(4)
      EQUIVALENCE (i4,i2(1),i1(1))
C
      INTEGER ipr
      SAVE    ipr
      DATA    ipr/0/
C
      icur=0
C
 10   IF(icur.GE.NWOR) GO TO 100
C
      icur=icur+1
      ITYP=IBUF(icur)
      IF(ITYP.GE.35.AND.ITYP.LE.37) THEN
C
C---      ADC/ hits
C
        n=IBUF(icur+1)
        IF(NWOR.LT.icur+1+n) GO TO 100
        IF(n.GT.MXADC) THEN
           WRITE(6,*) '*** ERROR: too many ADC hits:',n
           GO TO 100
        ENDIF
        NADC=n
        icur=icur+1
        DO i=1,NADC
           IADC(i)=IBUF(icur+i)
        END DO
        icur=icur+NADC
C
C---      TDC hits
C
        IF(NWOR.LT.icur+1) THEN
           WRITE(6,*) '*** ERROR: no TDC data',NWOR,icur+1
           GO TO 100
        ENDIF
        n=IBUF(icur+1)
        IF(NWOR.LT.icur+1+n) THEN
           WRITE(6,*) '*** ERROR: wrong length ',NWOR,icur+1+n
           GO TO 100
        ENDIF
        IF(n.GT.MXTDCW) THEN
           WRITE(6,*) '*** ERROR: too many TDC hits:',n
           GO TO 100
        ENDIF
        NTDC=n
        icur=icur+1
        DO i=1,NTDC
           i4=IBUF(icur+i)
           ITIM(i)=i2(J2ORD(1))
C           IF(i.EQ.1) WRITE(6,FMT='(4Z10)') i4,i2(J2ORD(1)),i2(J2ORD(2))
           j2=i2(J2ORD(2))
           i4=0
           i2(J2ORD(1))=j2
           ITED(i)=IAND(i4,1)
           ITCH(i)=(i4-ITED(i))/2+1
        END DO
        icur=icur+NTDC
C
        IF(ITYP.EQ.37) THEN
C
C---      2-nd TDC hits
C
          IF(NWOR.LT.icur+1) THEN
             WRITE(6,*) '*** ERROR: no TDC data',NWOR,icur+1
             GO TO 100
          ENDIF
          n=IBUF(icur+1)
          IF(NWOR.LT.icur+1+n) THEN
             WRITE(6,*) '*** ERROR: wrong length ',NWOR,icur+1+n
             GO TO 100
          ENDIF
          IF(n.GT.MXTDCW) THEN
             WRITE(6,*) '*** ERROR: too many TDC hits:',n
             GO TO 100
          ENDIF
          icur=icur+1
          DO i=1,n
             i4=IBUF(icur+i)
             ITIM(i+NTDC)=i2(J2ORD(1))
C     IF(i.EQ.1) WRITE(6,FMT='(4Z10)') i4,i2(J2ORD(1)),i2(J2ORD(2))
             j2=i2(J2ORD(2))
             i4=0
             i2(J2ORD(1))=j2
             ITED(i+NTDC)=IAND(i4,1)
             ITCH(i+NTDC)=(i4-ITED(i+NTDC))/2+1+32
          END DO
          NTDC=NTDC+n
          icur=icur+n
C
        ENDIF
C
C---     Fill the hits
C
        IF(NTDC.GT.0) THEN
           NHCHA=8
           DO i=1,NHCHA
              IHIT(i)=0
           END DO
           DO i=1,NTDC
              j=ITCH(i)
              IF(j.GE.1.AND.j.LE.5) THEN
                 IF(IABS(ITIM(i)-163).LT.20) IHIT(j)=ITIM(i)
              ENDIF
              IF(j.EQ.17) IHIT(1)=ITIM(i) 
              IF(j.EQ.18.AND.IABS(ITIM(i)-103).LT.10) IHIT(6)=ITIM(i) 
              IF(j.EQ.19.AND.IABS(ITIM(i)-103).LT.10) IHIT(7)=ITIM(i) 
              IF(j.EQ.20.AND.IABS(ITIM(i)-125).LT.10) IHIT(8)=ITIM(i) 
           ENDDO
        ENDIF
C
C---      Status record
C
        IF(NWOR.LT.icur+1) THEN
           WRITE(6,*) '*** ERROR: no status record',NWOR,icur+1
           GO TO 100
        ENDIF
        n=IBUF(icur+1)
        IF(NWOR.LT.icur+1+n) THEN
           WRITE(6,*) '*** ERROR: wrong length ',NWOR,icur+1+n
           GO TO 100
        ENDIF
        icur=icur+1
        IRETC=0
        IF(n.GT.0) THEN
          i=1
          k=IBUF(icur+1)
          DO j=1,MXTRIG
             IF(IAND(k,i).GT.0) ITRIG(j)=1
C             WRITE(6,*) ' j,i,ITRIG(j)',j,i,ITRIG(j)
             i=i*2
          END DO
C          ITRIG(6)=1-ITRIG(6)
          IF(n.GE.3) ITIMTICK=IBUF(icur+3)
          IF(n.GT.1) IRETC=IBUF(icur+n)
          icur=icur+n
        ENDIF
C
      ELSE IF(ITYP.EQ.32) THEN
C
C---      Scalers
C
         NSCA=IBUF(icur+1)
         DO i=1,NSCA
            ISCA(i)=IBUF(icur+1+i)
         END DO
         icur=icur+1+NSCA
C         WRITE(6,*) 'ITYP=',ITYP
C         WRITE(6,FMT='(8Z10)') (IBUF(i),i=1,NWOR)
C
      ENDIF
C
      GO TO 10
C
 100  CONTINUE
C
      IF(NSCA.GT.0) THEN
C         WRITE(6,2000) NADC,(IADC(i),i=1,NADC)
C         WRITE(6,2100) NTDC,(i,ITCH(i),ITIM(i),ITED(i),i=1,NTDC)
C         WRITE(6,2200) NSCA,(ISCA(i),i=1,NSCA)
      ENDIF
C
 999  CONTINUE
 2000 FORMAT(' NADC=',I2,/(12I6))
 2100 FORMAT(' NTDC=',I3,/(2X,I3,2X,I3,I5,2X,I2))
 2200 FORMAT(' NSCA=',I2,/(5I11))
      RETURN
      END
C
      SUBROUTINE RNTUPEV
C     ******************************************************************
C     *                                                                *
C     *      Fills the NTUPLE                                          *
C     *                                                                *
C     ******************************************************************
C
      IMPLICIT NONE
C
      INCLUDE 'inc/cevcoda.inc'
      INCLUDE 'inc/crntup.inc'
      INCLUDE 'inc/crntup1.inc'
      INCLUDE 'inc/crflag.inc'
C
      INTEGER id
C
      id=1
C
C         WRITE(6,*) ' ADC,TDC,SCA ',NADC,NTDC,NSCA
      IF(NADC.GT.0.OR.NTDC.GT.0.OR.NSCA.GT.0) THEN
         CALL HFNT(id)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE RNTUPCL
C
C===     Close NTUPLE
C
      IMPLICIT NONE
      INTEGER id,icycle
C
      id=1
      CALL HCDIR('//adctdc',' ')
      CALL HROUT(id,icycle,' ')
      CALL HREND('adctdc')
      CLOSE(UNIT=2)
C
      RETURN
      END
