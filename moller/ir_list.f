      INTEGER FUNCTION IR_LIST(LOUT,KEY1)
C
C--- RZ: returns the number of records in the current directory,
C       fills a vector with the KEY1 
C       and optionally prints record info for one KEY1
C
C        Input:
C---   LOUT  = 0   do not print anything
C            1:50  print to LUN=6 
C            >50   print to LUN=LUN
C                  printed:  
C                    KEY1 = 0 - all records
C                        != 0 - all records with KEY1
C        Output:   
C                  KEY1 = 0 IR_LIST= the of unique 1-st keys  
C                      != 0          the number of records with KEY1 
C                                    (only 1 cycle counts)
C---
C
      IMPLICIT NONE
      INTEGER  LOUT,KEY1
C
      INTEGER       IQUEST
      COMMON/QUEST/ IQUEST(100)
C
      INCLUDE 'inc/v_rzrec.inc'
C
      INTEGER    mxrlen
      PARAMETER (mxrlen=800)
      INTEGER    ibuf(mxrlen)
C
      INTEGER i,j,nrlen,nrvec,key(3),lkey(3),icycle,nrectot
     +       ,nkword,nkey1,irec,ifind,luno,ifind1,idate,itime
C
      INTEGER    mxkeyc
      PARAMETER (mxkeyc=100)
      INTEGER nkeyc,keyc(3,mxkeyc)

C---    Find the number of records
C
      IR_LIST=0
C
C      CALL RZLDIR(' ','A')
C
      key(1)=1
      key(2)=0
      key(3)=0
      icycle=99999
      CALL RZVIN(ibuf,mxrlen,nrlen,key(1),icycle,'CDS')
      IF(IQUEST(1).NE.0) THEN
         WRITE(6,*) 'IR_LIST error: can not read the current directory'
         WRITE(6,FMT='('' IQUEST='',5I8)') (IQUEST(i),i=1,5)
         GO TO 999
      ENDIF
C
      nrectot=IQUEST(7)
      nkword= IQUEST(8)
C      WRITE(6,*) 'tot=',nrectot
C
      luno=LOUT
      IF((luno.GT.0.AND.luno.LE.50).OR.luno.GT.99) luno=6
C
      IR_LIST=nrectot
C
      nrvec=0
      nkey1=0
      nkeyc=0
      DO irec=1,nrectot
         key(1)=irec
         icycle=99999
         CALL RZVIN(ibuf,mxrlen,nrlen,key(1),icycle,'CDS')
         IF(IQUEST(1).NE.0) THEN
            WRITE(6,*) 'IR_LIST error on record=',irec
            WRITE(6,FMT='('' IQUEST='',5I8)') (IQUEST(i),i=1,5)
            GO TO 999
         ENDIF
         DO i=1,3
            lkey(i)=IQUEST(i+20)
         ENDDO
         icycle=IQUEST(6)
         ifind=0
         DO i=1,nrvec
            IF(KEYRZREC(i).EQ.lkey(1)) ifind=i
         ENDDO
         IF(ifind.EQ.0) THEN
            IF(nrvec.LE.MXRZREC) THEN
               nrvec=nrvec+1
               KEYRZREC(nrvec)=lkey(1)
            ELSE
               WRITE(6,*) 'IR_LIST error: vector KEYRZREC'
     +              ,' is too short ',MXRZREC
            ENDIF
         ENDIF
C
         IF(KEY1.NE.0.AND.lkey(1).EQ.KEY1) THEN
            ifind1=0
            DO i=1,nkeyc
               IF(lkey(2).EQ.keyc(2,i).AND.
     +            lkey(3).EQ.keyc(3,i)) THEN
                  ifind1=1
               ENDIF
            ENDDO
            IF(ifind1.EQ.0) THEN
               IF(nkeyc.LT.mxkeyc) THEN
                  nkeyc=nkeyc+1
                  DO j=2,3
                     keyc(j,nkeyc)=lkey(j)
                  ENDDO
               ELSE
                  WRITE(6,*) 'IR_LIST error: too short'
     +                 ,' mxkeyc=',mxkeyc
               ENDIF
            ENDIF
         ENDIF
C
         IF(luno.GT.0) THEN
            IF(KEY1.EQ.0.OR.KEY1.EQ.lkey(1)) THEN
C               CALL RZDATE(IQUEST(14),idate,itime,1)
               WRITE(luno,2000) (lkey(i),i=1,3),icycle
     +                   ,IQUEST(14),nrlen
 2000          FORMAT(3X,I9,2X,A4,2X,A4,2X,I5,2X,Z16,2X,I5)
            ENDIF
         ENDIF
C
      ENDDO
C
      IF(nrvec.GT.0) CALL SORTIN(KEYRZREC(1),nrvec)
C
      IF(KEY1.NE.0) THEN
         IR_LIST=nkeyc
      ELSE
         IR_LIST=nrvec
      ENDIF
C
 999  CONTINUE
      END
C
      INCLUDE 'sortin.f'




