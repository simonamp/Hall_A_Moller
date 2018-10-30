      INTEGER FUNCTION IR_EXIST(KEYI,NKEY,ICYC)
C
C--- RZ Moller:checks if there is a record with an integer key KEYI..
C---     in the current RZ directory
C        Input:
C---   KEYI(1:NKEY) - the keys 
C---   ICYC  - the cycle (=0 or 99999 - any cycle)
C---     Output:
C      = 0 - no record
C      > 0 - the max record length (if NKEY=3)
C          - the number of records (one cycle per record counts) (if NKEY<3)
C
      IMPLICIT NONE
      INTEGER  KEYI(NKEY),NKEY,ICYC
C
      INTEGER       IQUEST
      COMMON/QUEST/ IQUEST(100)
C
C
      INTEGER    mxrlen
      PARAMETER (mxrlen=20)
      INTEGER    ibuf(mxrlen)
C
      INTEGER  key(3),icycle,nrlen,nkey2,icyc1,icyc2,nrlenmx
     +        ,i,nrectot,irec,nrfind
C
C---    Find the number of records
C
      IR_EXIST=0
C
C      CALL RZLDIR(' ','A')
C
      DO i=1,NKEY
         key(i)=KEYI(i)
      ENDDO
C
      IF(NKEY.EQ.3) THEN
C
         CALL RZVIN(ibuf,mxrlen,nrlen,key(1),ICYC,'CD')
         IF(IQUEST(1).NE.0) THEN
            GO TO 999
         ENDIF
         icyc1=IQUEST(6)
         IF(ICYC.GT.0.AND.ICYC.LT.9999.AND.icyc1.NE.ICYC) THEN
            GO TO 999
         ENDIF
         IR_EXIST=nrlen
C
      ELSE
C
         key(1)=1
         CALL RZVIN(ibuf,mxrlen,nrlen,key(1),icycle,'CDS')
         IF(IQUEST(1).NE.0) THEN
            GO TO 999
         ENDIF
C
         nrectot=IQUEST(7)
         nrfind=0
C
C         WRITE(6,*) 1,nrectot
         DO irec=1,nrectot
            key(1)=irec
            key(2)=0
            key(3)=0
            icycle=99999
            CALL RZVIN(ibuf,mxrlen,nrlen,key(1),icycle,'CDS')
            IF(IQUEST(1).EQ.0) THEN
               WRITE(6,FMT='(I8,2A8)') (IQUEST(i),i=21,23)
               IF(KEYI(1).EQ.IQUEST(21)) THEN
                  WRITE(6,*) 2,nrectot,KEYI,'iq=',IQUEST(22)
                  IF(NKEY.GE.2) THEN
                     IF(KEYI(2).EQ.IQUEST(22)) THEN
C                        WRITE(6,*) 3,nrectot
                        IF(NKEY.GE.3) THEN
                           IF(KEYI(3).EQ.IQUEST(23)) THEN
                              nrfind=nrfind+1
                           ENDIF
                        ELSE 
                           nrfind=nrfind+1
                        ENDIF 
                     ENDIF
                  ELSE
                     nrfind=nrfind+1
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
C
         IR_EXIST=nrfind
C
      ENDIF
C
 999  CONTINUE
      END


