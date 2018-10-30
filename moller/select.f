      REAL FUNCTION SELECT(DUMMY)
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
C
      VECTOR IDD0(1)
C
      INTEGER ntch(32),itmin(32),jtch(32)
      INTEGER jch(4)
      DATA ievv/0/
      DATA jch/1,2,4,5/
C
      id0=IDD0(1)
      SELECT=0.
      ievv=ievv+1
C      WRITE(6,*) 'ievv=',ievv
C      IF(ievv.GE.541.AND.ievv.LE.543) THEN
C        WRITE(6,2000) IRUN,IFLA,ITYP
C        WRITE(6,2100) NADC,(IADC(i),i=1,NADC)
C        WRITE(6,2200) NTDC,(i,ITCH(i),ITIM(i),ITED(i),i=1,NTDC)
C      ENDIF
C
C      CALL HF2(301,IADC(1)+0.1,ievv+0.1,1.)
C
      DO ich=1,32
         ntch(ich)=0
         itmin(ich)=999999
      END DO
      DO i=1,NTDC
         ich=ITCH(i)
         IF(ich.GE.1.OR.ich.LE.32) THEN
            ntch(ich)=ntch(ich)+1
            IF(ITIM(i).LT.itmin(ich)) itmin(ich)=ITIM(i)
            IF(ntch(ich).EQ.1) jtch(ich)=i
         ELSE
            WRITE(6,*) '*** WRONG ich=',ich,' event,i=',ievv,i
         ENDIF
      END DO
C
      icoin=0
      IF(ntch(1).GT.0.AND.ntch(2).GT.0.AND.
     +   ntch(4).GT.-1.AND.ntch(5).GT.-1) THEN
        icoin=0
        iclose=0
        DO i=1,4
          ich=jch(i)
          DO j=1,ntch(ich)
            IF(ABS(REAL(itim(jtch(ich)+j-1))-150.).LT.10.) THEN
              icoin=icoin+1
              GO TO 50
            ENDIF
          END DO
 50       CONTINUE
          DO j=1,ntch(ich)
            IF(ABS(REAL(itim(jtch(ich)+j-1))-150.).GT.10..AND.
               ABS(REAL(itim(jtch(ich)+j-1))-150.).LT.80.) THEN
C              iclose=1
            ENDIF
          END DO
          IF(iclose.NE.0) icoin=0
        END DO
        IF(icoin.EQ.4) SELECT=1.
      ENDIF
C
C
 999  CONTINUE
 2000 FORMAT(' IRUN,IFLA,ITYP ',3I8)
 2100 FORMAT(' NADC=',I2,/(12I6))
 2200 FORMAT(' NTDC=',I3,/(2X,I3,2X,I3,I5,2X,I2))
      END

