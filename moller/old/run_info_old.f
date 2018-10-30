      SUBROUTINE RUN_INFO
C
C---     Reads the run info *.set file and retrieves the data
C
      IMPLICIT NONE
C
      INCLUDE 'inc/cruninf.inc'
C
      INTEGER i,j,iost,nr
      REAL    atm(20)
C
      INTEGER idat,ithr,idel,iplu,nbcm,nsld
     +       ,ir,iatm(20),nw,lwor(20),n
C
      CHARACTER cline1*132,cline*132,cwor(20)*64,cmonth(12)*3
      DATA cmonth/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug'
     +           ,'Sep','Oct','Nov','Dec'/
C
C     ------------------------------------------------------------------
C
      OPEN(UNIT=1,FILE='run.info',STATUS='OLD',IOSTAT=iost)
      REWIND 1
      IF(iost.NE.0) THEN
         WRITE(6,*) ' Error opening run.info'
         GO TO 999
      ENDIF
C      WRITE(6,FMT='(A12,I6,I7,4I3,F12.2)') 'run,dat,ener'
C     +                ,IRUNR,IDATR,ENERR
C
      IRUNR=0
C
      idat=0
      ithr=0
      idel=0
      iplu=0
      nbcm=0
      nsld=0
      NPLUR=0
      NDELAYR=0
      NTHRER=0
C
      ITARGR=0
      DO i=1,5
         IDATR(i)=0
      ENDDO
      WIDTHR=0.
      ENERR=0.
      DO i=1,2
         BCMR(i)=0.
         SLDR(i)=0.
         POSTAR(i)=0.
         DO j=1,2
            BPMR(j,i)=0.
         ENDDO
      ENDDO   
      DO i=1,4
         AMAGNR(i)=0
      ENDDO
      
      nr=0
 10   READ(1,1000,END=999) cline1
 1000 FORMAT(A132)
C      WRITE(6,FMT='(A132)') cline1
C
      IF(INDEX(cline1,'Run Numb').GT.0) THEN
         READ(cline1(13:132),*) ir
         IRUNR=ir
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
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),WIDTHR)
      ELSE IF(INDEX(cline1,'threshold').EQ.1.AND.ithr.EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         NTHRER=MIN(nw-1,MXTHR)
         IF(NTHRER.GT.0) THEN
            DO i=1,NTHRER
               READ(cwor(i+1)(1:lwor(i+1)),*) ITHRER(i)
            ENDDO
         ENDIF
         ithr=0
      ELSE IF(INDEX(cline1,'delay(ns)').EQ.1.AND.idel.EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         NDELAYR=MIN(nw-1,MXDEL)
         IF(NDELAYR.GT.1) THEN
            DO i=1,NDELAYR
               READ(cwor(i+1)(1:lwor(i+1)),*) IDELAYR(i)
            ENDDO
         ENDIF
         idel=0
C
      ELSE IF(cline1(1:1).EQ.' '.AND.iplu.EQ.2) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.4) THEN
            IF(NPLUR.LT.MXPLU) NPLUR=NPLUR+1
            DO i=1,3
               READ(cwor(i+1)(1:lwor(i+1)),*) IPLUR(i,NPLUR)
            ENDDO
         ELSE
            iplu=0
         ENDIF
      ELSE IF(INDEX(cline1,'_energy').GT.0) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),ENERR)
C
      ELSE IF(INDEX(cline1,'IPM1H0').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) THEN
            j=1
            IF(INDEX(cwor(1),'1H03').GT.0) j=2
            i=0
            IF(INDEX(cwor(1),'XPOS').GT.0) i=1
            IF(INDEX(cwor(1),'YPOS').GT.0) i=2
            IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),BPMR(i,j))
         ENDIF
      ELSE IF(INDEX(cline1,'SLD1H').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) THEN
            IF(nsld.LT.2) nsld=nsld+1
            CALL READFL(cwor(2)(1:lwor(2)),SLDR(nsld))
         ENDIF
      ELSE IF(INDEX(cline1,'hac_bcm').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) THEN
            IF(nbcm.LT.2) nbcm=nbcm+1
            CALL READFL(cwor(2)(1:lwor(2)),BCMR(nbcm))
         ENDIF
      ELSE IF(INDEX(cline1,'MQM1H02M').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNR(1))
      ELSE IF(INDEX(cline1,'MQO1H03M').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNR(2))
      ELSE IF(INDEX(cline1,'MQO1H03AM').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNR(3))
      ELSE IF(INDEX(cline1,'MMA1H01M').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),AMAGNR(4))
      ELSE IF(INDEX(cline1,'HAmol_lin.RBV').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),POSTAR(1))
      ELSE IF(INDEX(cline1,'HAmol_rot.RBV').EQ.1) THEN
         CALL WSPLIT(cline1,20,cwor(1),nw,lwor(1))
         IF(nw.GT.1) CALL READFL(cwor(2)(1:lwor(2)),POSTAR(2))
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
     +                ,IRUNR,IDATR,ENERR
C      WRITE(6,FMT='(A8,24I5)') 'delay',(IDELAYR(i),i=1,NDELAYR)
C      WRITE(6,FMT='(A8,24I5)') 'thres',(ITHRER(i),i=1,NTHRER)
C      DO i=1,NPLUR
C         WRITE(6,*) 'plu',(IPLUR(j,i),j=1,3)
C      ENDDO
C      WRITE(6,FMT='(A18,13F8.2)') 'bpm,bcm,sld,mag'
C     +        ,BPMR,BCMR,SLDR,AMAGNR,WIDTHR
C      WRITE(6,FMT='(A6,I5,4F10.4)') 'itar..'
C     +        ,ITARGR,POSTAR
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
