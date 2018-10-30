      SUBROUTINE GET_EPI
C
C---     Reads the epics data from the file run.info (for a given run)
C
      IMPLICIT NONE
C
      INCLUDE 'inc/cruninf.inc'
C
      INTEGER i,j,iost,nr
      REAL    atm(20)
C
      INTEGER    mxvar
      PARAMETER (mxvar=100)
      CHARACTER  cvar(mxvar)*24
      INTEGER    itvar(mxvar)
C
      INTEGER idat,ithr,idel,iplu,nbcm,nsld
     +       ,ir,iatm(20),nw,lwor(20),n,i,j
C
      CHARACTER cline1*132,cline*132,cwor(20)*64,cmonth(12)*3,cty*1
     +         ,cvcur*24
      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug'
     +           ,'Sep','Oct','Nov','Dec'/
C
C     ------------------------------------------------------------------
C
C---     Read the variable names and types
      DO i=1,mxvar
         itvar(i)=0
      ENDDO
      OPEN(UNIT=1,FILE='epics.list',STATUS='OLD',IOSTAT=iost)
      REWIND 1
      IF(iost.NE.0) THEN
         WRITE(6,*) ' Error opening epics.list'
         GO TO 999
      ENDIF
C
      nr=0
 10   READ(1,1000,END=59) j,cty,cvcur
 1000 FORMAT(I3,1X,A1,1X,A24)
      nr=nr+1
      IF(j.GT.0.AND.j.LE.mxvar) THEN
         IF(cty.EQ.'f') itvar(j)=1 
         IF(cty.EQ.'c') itvar(j)=2 
      ELSE
         WRITE(6,*) 'Error: variable number is out of range:',j 
      ENDIF
C
      IF(INDEX(cline1,'Run Numb').GT.0) THEN
         READ(cline1(13:132),*) ir
         IRUNI=ir
      ELSE IF(INDEX(cline1,'Date').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))

      OPEN(UNIT=1,FILE='run.info',STATUS='OLD',IOSTAT=iost)
      REWIND 1
      IF(iost.NE.0) THEN
         WRITE(6,*) ' Error opening run.info'
         GO TO 999
      ENDIF
C      WRITE(6,FMT='(A12,I6,I7,4I3,F12.2)') 'run,dat,ener'
C     +                ,IRUNI,IDATI,ENERI
C
      IRUNI=0
C
      idat=0
      ithr=0
      idel=0
      iplu=0
      nbcm=0
      nsld=0
      NPLUI=0
      NDELAYI=0
      NTHREI=0
C
      ITARGI=0
      DO i=1,5
         IDATI(i)=0
      ENDDO
      WIDTHI=0.
      ENERI=0.
      DO i=1,2
         BCMI(i)=0.
         SLDI(i)=0.
         POSTAI(i)=0.
         DO j=1,2
            BPMI(j,i)=0.
         ENDDO
      ENDDO   
      DO i=1,4
         AMAGNI(i)=0
      ENDDO
      
      nr=0
 10   READ(1,1000,END=999) cline1
 1000 FORMAT(A132)
C      WRITE(6,FMT='(A132)') cline1
C
      IF(INDEX(cline1,'Run Numb').GT.0) THEN
         READ(cline1(13:132),*) ir
         IRUNI=ir
      ELSE IF(INDEX(cline1,'Date').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GE.8) THEN
           j=0
           DO i=1,12
             IF(cwor(4)(1:3).EQ.cmonth(i)) j=i
           ENDDO
           iatm(2)=j
           READ(cwor(5)(1:2),*) iatm(3) 
           READ(cwor(6)(4:5),*) iatm(5) 
           READ(cwor(6)(1:2),*) iatm(4) 
           READ(cwor(8)(1:4),*) iatm(1) 
C           WRITE(6,*) (iatm(i),i=1,8)
           DO i=1,5
              IDATR(i)=iatm(i)
           END DO
        ENDIF
      ELSE IF(INDEX(cline1,'TD8000').GT.0) THEN
         ithr=1
      ELSE IF(INDEX(cline1,'y4518').GT.0) THEN
         idel=1
      ELSE IF(INDEX(cline1,'y2365').GT.0) THEN
         iplu=1
      ELSE IF(INDEX(cline1,'channel').EQ.1) THEN
         IF(iplu.EQ.1) iplu=2
      ELSE IF(INDEX(cline1,'width').EQ.1.AND.ithr.EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),WIDTHI)
      ELSE IF(INDEX(cline1,'threshold').EQ.1.AND.ithr.EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         NTHREI=MIN(nw-1,MXTHR)
         IF(NTHREI.GT.0) THEN
            DO i=1,NTHREI
               READ(cwor(i+1)(1:lwor(i+1)),*) ITHREI(i)
            ENDDO
         ENDIF
         ithr=0
      ELSE IF(INDEX(cline1,'delay(ns)').EQ.1.AND.idel.EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         NDELAYI=MIN(nw-1,MXDEL)
         IF(NDELAYI.GT.1) THEN
            DO i=1,NDELAYI
               READ(cwor(i+1)(1:lwor(i+1)),*) IDELAYI(i)
            ENDDO
         ENDIF
         idel=0
C
      ELSE IF(cline1(1:1).EQ.' '.AND.iplu.EQ.2) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.4) THEN
            IF(NPLUI.LT.MXPLU) NPLUI=NPLUI+1
            DO i=1,3
               READ(cwor(i+1)(1:lwor(i+1)),*) IPLUI(i,NPLUI)
            ENDDO
         ELSE
            iplu=0
         ENDIF
      ELSE IF(INDEX(cline1,'_energy').GT.0) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),ENERI)
C
      ELSE IF(INDEX(cline1,'IPM1H0').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) THEN
            j=1
            IF(INDEX(cwor(1),'1H03').GT.0) j=2
            i=0
            IF(INDEX(cwor(1),'XPOS').GT.0) i=1
            IF(INDEX(cwor(1),'YPOS').GT.0) i=2
            IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),BPMI(i,j))
         ENDIF
      ELSE IF(INDEX(cline1,'SLD1H').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) THEN
            IF(nsld.LT.2) nsld=nsld+1
            CALL READFL(cwor(2)(1:lwor(2)),SLDI(nsld))
         ENDIF
      ELSE IF(INDEX(cline1,'hac_bcm').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) THEN
            IF(nbcm.LT.2) nbcm=nbcm+1
            CALL READFL(cwor(2)(1:lwor(2)),BCMI(nbcm))
         ENDIF
      ELSE IF(INDEX(cline1,'MQM1H02M').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNI(1))
      ELSE IF(INDEX(cline1,'MQO1H03M').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNI(2))
      ELSE IF(INDEX(cline1,'MQO1H03AM').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNI(3))
      ELSE IF(INDEX(cline1,'MMA1H01M').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNI(4))
      ELSE IF(INDEX(cline1,'HAmol_lin.RBV').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),POSTAI(1))
      ELSE IF(INDEX(cline1,'HAmol_rot.RBV').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),POSTAI(2))
      ELSE IF(INDEX(cline1,'HAmol_linSTAT').EQ.1) THEN
         IF(INDEX(cline1,'Bottom').GT.0) THEN
            ITARGR=1
         ELSE IF(INDEX(cline1,'Top').GT.0) THEN
            ITARGR=2
         ENDIF
      ELSE
C         WRITE(6,*) 'iplu',iplu
      ENDIF
C
      GO TO 10
 999  CONTINUE
C
      WRITE(6,FMT='(A12,I6,I7,4I3,F12.2)') 'run,dat,ener'
     +                ,IRUNI,IDATI,ENERI
C      WRITE(6,FMT='(A8,24I5)') 'delay',(IDELAYI(i),i=1,NDELAYI)
C      WRITE(6,FMT='(A8,24I5)') 'thres',(ITHREI(i),i=1,NTHREI)
C      DO i=1,NPLUI
C         WRITE(6,*) 'plu',(IPLUI(j,i),j=1,3)
C      ENDDO
C      WRITE(6,FMT='(A18,13F8.2)') 'bpm,bcm,sld,mag'
C     +        ,BPMI,BCMI,SLDI,AMAGNI,WIDTHI
C      WRITE(6,FMT='(A6,I5,4F10.4)') 'itar..'
C     +        ,ITARGI,POSTAI
      CLOSE(UNIT=1)
      END
C
      SUBROUTINE WSPLIT(CLIN,NWMX,CWOR,NW,LWOR)
C
C---     Split a line CLIN into words CWOR
C
      IMPLICIT NONE
      CHARACTER CLIN*(*),CWOR(*)*(*)
      INTEGER NWMX,NW,LWOR(*)
C
      INTEGER il,il1,lenl,lenw,lw
C
      NW=0
      lenl=LEN(CLIN)
      lenw=LEN(CWOR(1))
C
      il=1
C
 10   IF(CLIN(il:il).NE.' ') THEN
C
         IF(NW.LT.NWMX) THEN
            NW=NW+1
         ENDIF
         CWOR(NW)=' '
         lw=INDEX(CLIN(il:lenl),' ')-1
         IF(lw.LE.0) lw=lenl-il+1
         CWOR(NW)=CLIN(il:il+lw-1)
         il=il+lw
         LWOR(NW)=lw
C         WRITE(6,*) NW,LWOR(NW),' ',CWOR(NW)
      ELSE
         il=il+1
      ENDIF

      IF(il.LT.lenl) GO TO 10
C
      RETURN
      END

C
      SUBROUTINE READFL(CWOR,A)
C
C---     Read a real variable
C
      IMPLICIT NONE
      CHARACTER CWOR*(*)
      REAL A
C
      INTEGER ia
C
      IF(INDEX(CWOR,'.').GT.0) THEN
         READ(CWOR,*) A
      ELSE
         READ(CWOR,*) ia
         A=ia
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE READIN(CWOR,IA)
C
C---     Read an integer variable
C
      IMPLICIT NONE
      CHARACTER CWOR*(*)
      INTEGER IA
C
      REAL a
C
      IF(INDEX(CWOR,'.').GT.0) THEN
         READ(CWOR,*) a
         IA=a
      ELSE
         READ(CWOR,*) IA
      ENDIF
C
      RETURN
      END
