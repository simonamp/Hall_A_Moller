      REAL FUNCTION RDNTUP(DUMMY)
      IMPLICIT REAL(A-H,O-Z),INTEGER(J-N)
      INCLUDE ?
C
      VECTOR IDD0(1)
      VECTOR PEDES(12)
C
      INTEGER ntch(64),itmin(64),jtch(64),jhit(100)
      DATA ievv/0/
C
      id0=IDD0(1)
      RDNTUP=1.
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
      DO i=1,20
         jhit(i)=0
      END DO
      
      DO i=1,NTDC
         ich=ITCH(i)
         IF(ich.GE.1.OR.ich.LE.32) THEN
C            IF(iadc(1)+iadc(2)+iadc(3)+iadc(4)-270.GT.500) THEN
              CALL HF1(200+ich+id0,ITIM(i)+0.1,1.)
C            ENDIF
            ntch(ich)=ntch(ich)+1
            IF(ITIM(i).LT.itmin(ich)) itmin(ich)=ITIM(i)
            IF(ntch(ich).EQ.1) jtch(ich)=i
            IF(ABS(REAL(ITIM(i))-150.).LT.10.) THEN
               IF(ich.EQ.1) jhit(1)=1
               IF(ich.EQ.2) jhit(2)=1
               IF(ich.EQ.4) jhit(3)=1
               IF(ich.EQ.5) jhit(4)=1
            ENDIF
         ELSE
            WRITE(6,*) '*** WRONG ich=',ich,' event,i=',ievv,i
         ENDIF
      END DO
      DO ich=1,32
         IF(ntch(ich).GT.0) THEN
            CALL HF1(300+ich+id0,itmin(ich)+0.1,1.)
            CALL HF1(400+ich+id0,ntch(ich)+0.1,1.)
            CALL HF1(700+ich+id0,itmin(ich)+0.1,1.)
         ENDIF
         CALL HF1(340+id0,ich+0.1,REAL(ntch(ich)))
      END DO
      IF(ntch(1).GT.0.AND.ntch(2).GT.0) THEN
         CALL HF2(350+id0,itmin(1)+0.1,itmin(2)+0.1,1.)
      ENDIF
C
      IF(jhit(1)+jhit(3).EQ.2) jhit(5)=1
      IF(jhit(2)+jhit(4).EQ.2) jhit(6)=1
      IF(jhit(5)+jhit(6).EQ.2) jhit(7)=1
      IF(jhit(1)+jhit(2).EQ.2) jhit(8)=1
      IF(jhit(2)+jhit(3).EQ.2) jhit(9)=1
      IF(jhit(3)+jhit(4).EQ.2) jhit(10)=1
      IF(jhit(1)+jhit(2).EQ.2.AND.jhit(3)+jhit(4).GT.0) jhit(11)=1
      DO i=1,20
         IF(jhit(i).NE.0) CALL HF1(id0+360,i+0.1,1.)
      END DO
      CALL HF1(id0+370,jhit(1)+jhit(2)+jhit(3)+jhit(4)+.1,1.)
C
C      CALL HF1(id0+375,iev+.1,
C
      k=1
      IF(ntch(k).GE.2) THEN
         CALL HF2(id0+500+k,itim(jtch(k)+ntch(k)-1)+.1
     +       ,itim(jtch(k)+ntch(k)-2)+.1,1.)
      ENDIF
C
      k=13
      IF(ntch(k).GE.2) THEN
         CALL HF2(id0+500+k,itim(jtch(k)+ntch(k)-1)+.1
     +       ,itim(jtch(k)+ntch(k)-2)+.1,1.)
      ENDIF
C
      IF(ntch(13).GE.1.AND.ntch(14).GE.1) THEN
         CALL HF2(id0+613,itim(jtch(14)+ntch(14)-1)+0.1
     +       ,itim(jtch(13)+ntch(13)-1)+.1,1.)
         IF(itim(jtch(13)+ntch(13)-1).GT.59.AND.
     +      itim(jtch(13)+ntch(13)-1).LT.62)
     +   CALL HF1(id0+813,itim(jtch(14)+ntch(14)-1)+0.1,1.)
      ENDIF
C
 999  CONTINUE
 2000 FORMAT(' IRUN,IFLA,ITYP ',3I8)
 2100 FORMAT(' NADC=',I2,/(12I6))
 2200 FORMAT(' NTDC=',I3,/(2X,I3,2X,I3,I5,2X,I2))
      END

