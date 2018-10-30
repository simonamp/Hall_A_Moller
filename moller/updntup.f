      PROGRAM UPDNTUP 
C
C===  Reads the 1-st ntuple and adds some information
C
C
      IMPLICIT NONE
C
      INTEGER    mxlenb,mxeve,mxhbook
      PARAMETER (mxlenb=8192,mxeve=10000000,mxhbook=200000)
C
      INTEGER mhbook
      COMMON/PAWC/ mhbook(mxhbook)
C
      INCLUDE 'inc/crntup.inc'
C
      INTEGER iost
C
C---   Open NTUPLE
C
      CALL HLIMIT(mxhbook)
C
      CALL RNTUPIN(1,1,iost)
      IF(iost.NE.0) GO TO 999
C
      CALL RNTUPIN(2,2,iost)
      IF(iost.NE.0) GO TO 999
C
C---    Read and fill the NTUPLE
C
      CALL RNTPROC(1,2)
C
C---    Close the NTUPLE
C
      CALL RNTUPCL
C
 999  CONTINUE
      END
C
      SUBROUTINE RNTUPIN(LUN,IO,IRET)
C
C     ******************************************************************
C     *                                                                *
C     *   Books a CWN ntuple for the ADCTDC data                       *
C     *   INPUT :  IO = 1 - input                                      *
C     *               = 2 - output                                     *
C     *           LUN - logical unit                                   *
C     *   OUTPUT:  IRET =0 - OK                                        *
C     *               = 0 - error                                      * 
C     *                                                                *
C     ******************************************************************
C
      IMPLICIT NONE
      INTEGER  LUN,IO,IRET
C
      INCLUDE 'inc/crntup.inc'
      INCLUDE 'inc/crntup1.inc'
C
      INTEGER  lrec,istat,id
      CHARACTER clena*2,clent*2,clentw*3
C
C     ------------------------------------------------------------------
C
      IRET=0
      IF(LUN.LE.0) THEN
        WRITE(6,*)'*** ERROR in RNTUPIN - wrong LUN=',LUN
        IRET=1
        GO TO 999
      ENDIF
      IF(IO.LE.0.OR.IO.GT.2) THEN
        WRITE(6,*)'*** ERROR in RNTUPIN - wrong IO=',IO
        IRET=1
        GO TO 999
      ENDIF
C
      IF(IO.EQ.1) THEN
C
C---       Input
C
        lrec=0
        CALL HROPEN(LUN,'adcin','in.nt',' ',lrec,istat)
        IF(istat.NE.0) THEN
           WRITE(6,*)'*** ERROR in RNTUPIN opening NTUPLE LUN=',LUN
     +               ,' istat=',istat
           IRET=1
           GO TO 999
        ENDIF
        id=1
        CALL HRIN(id,9999,0)
        WRITE(6,*)'=== Mark 1'
C
        CALL HBNAME(id,' ',0,'$CLEAR')
C
        CALL HBNAME(id,'run',IRUN,'$SET:irun')
        CALL HBNAME(id,'run',IFLA,'$SET:ifla')
        CALL HBNAME(id,'run',ITYP,'$SET:ityp')
        CALL HBNAME(id,'run',IRETC,'$SET:iret')
C
        WRITE(clena,FMT='(I2)') MXADC
        WRITE(clent,FMT='(I2)') MXTDC
        WRITE(clentw,FMT='(I3)') MXTDCW
        CALL HBNAME(id,'adc',NADC,'$SET:nadc')
        CALL HBNAME(id,'adc',IADC,'$SET:iadc')
        CALL HBNAME(id,'tdc',NTDC,'$SET:ntdc')
        CALL HBNAME(id,'tdc',ITCH,'$SET:itch')
        CALL HBNAME(id,'tdc',ITIM,'$SET:itim')
        CALL HBNAME(id,'tdc',ITED,'$SET:ited')
        WRITE(6,*)'=== Mark 2'
C
      ELSE IF(IO.EQ.2) THEN
C
C---       Output
C
        lrec=1024
        CALL HROPEN(LUN,'adcout','out.nt','N',lrec,istat)
        IF(istat.NE.0) THEN
           WRITE(6,*)'*** ERROR in RNTUPIN opening NTUPLE LUN=',LUN
     +               ,' istat=',istat
           IRET=1
           GO TO 999
        ENDIF
        WRITE(6,*)'=== Mark 3'
C
        CALL HCDIR('//adcout',' ')
        id=1
        CALL HBNT(id,'ADCTDC data',' ')
        CALL HBNAME(id,'run',IRUN,'irun')
        CALL HBNAME(id,'run',IFLA,'ifla')
        CALL HBNAME(id,'run',ITYP,'ityp')
        CALL HBNAME(id,'run',IRETC,'iret')
C
        WRITE(clena,FMT='(I2)') MXADC
        WRITE(clent,FMT='(I2)') MXTDC
        WRITE(clentw,FMT='(I3)') MXTDCW
        CALL HBNAME(id,'adc',NADC,'nadc[0,'//clena//']')
        CALL HBNAME(id,'adc',IADC,'iadc(nadc)[0,4095]')
        CALL HBNAME(id,'tdc',NTDC,'ntdc[0,'//clentw//']')
        CALL HBNAME(id,'tdc',ITCH,'itch(ntdc)[0,'//clent//']')
        CALL HBNAME(id,'tdc',ITIM,'itim(ntdc)[0,60000]')
        CALL HBNAME(id,'tdc',ITED,'ited(ntdc)[0,1]')
C
        NHCHA=8
        WRITE(clena,FMT='(I2)') MXHCHA
        CALL HBNAME(id,'tdc',NHCHA,'[0,'//clena//']')
        CALL HBNAME(id,'tdc',IHIT,'ihit(NHCHA)[0,1000]')
        WRITE(6,*)'=== Mark 4'
C
      ENDIF
C
 999  CONTINUE
      RETURN
      END
C
      SUBROUTINE RNTPROC(LUNIN,LUNOUT)
C
C     ******************************************************************
C     *                                                                *
C     *   Reads and rewrites the NTUPLE for the ADCTDC data            *
C     *   INPUT :  LUNIN - input NTUPLE                                *
C     *            LUNOUT - output                                     *
C     *                                                                *
C     ******************************************************************
C
C
      IMPLICIT NONE
      INTEGER  LUNIN,LUNOUT
C
      INCLUDE 'inc/crntup.inc'
      INCLUDE 'inc/crntup1.inc'
C
      INTEGER  i,id,ierr,iev,nent
C
      INTEGER ipr
      SAVE    ipr
      DATA    ipr/0/
C
C     ------------------------------------------------------------------
C
C
      CALL HCDIR('//adcin',' ')
      id=1
      CALL HNOENT(id,nent)
      WRITE(6,*) '=== INPUT Ntuple: number of entries=',nent
C
      DO iev=1,nent
C
        DO i=1,MXHCHA
           IHIT(i)=0
        END DO
C
        CALL HCDIR('//adcin',' ')
        id=1
        CALL HGNT(id,iev,ierr)
        IF(ierr.NE.0) THEN
          WRITE(6,*) '=== Read error at event=',iev
          GO TO 999
        ENDIF
C
C
        IF(ipr.LT.20) THEN
          ipr=ipr+1
          WRITE(6,1000) iev,NADC,NTDC,IADC
 1000     FORMAT(' iev,NADC,NTDC,IADC=',I8,2X,2I3,4X,12I5)
        ENDIF
C
        CALL HCDIR('//adcout',' ')
        id=1
        CALL HFNT(id)
C
      END DO
C
C      WRITE(6,2000) IRUN,IFLA,ITYP
C      WRITE(6,2100) NADC,(IADC(i),i=1,NADC)
C      WRITE(6,2200) NTDC,(i,ITCH(i),ITIM(i),ITED(i),i=1,NTDC)
C
 100  CONTINUE
C
 999  CONTINUE
 2000 FORMAT(' IRUN,IFLA,ITYP ',3I8)
 2100 FORMAT(' NADC=',I2,/(12I6))
 2200 FORMAT(' NTDC=',I3,/(2X,I3,2X,I3,I5,2X,I2))
      RETURN
      END
C
      SUBROUTINE RNTUPCL(LUN)
C
C===     Close NTUPLE
C
      IMPLICIT NONE
      INTEGER LUN
      INTEGER id,icycle
C
      id=1
      CALL HCDIR('//adcout',' ')
      CALL HROUT(id,icycle,' ')
      CALL HREND('adcout')
      CLOSE(UNIT=LUN)
C
      RETURN
      END
