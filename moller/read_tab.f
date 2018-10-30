      SUBROUTINE READ_TAB
C
C===   Reads a runs.tab file
C
      IMPLICIT NONE
C
      VECTOR RTMP(30,1000)
C      REAL RTMP(30,200)
C
      INTEGER ilin(5),i,j,iost,k,i1,ii,ir,nr
      REAL    rlin(12)
C
      CHARACTER cline1*160
C
C     ------------------------------------------------------------------
C
      OPEN(UNIT=1,FILE='runs.tab.cur',STATUS='OLD',IOSTAT=iost)
      REWIND 1
      IF(iost.NE.0) THEN
         WRITE(6,*) ' Error opening runs.tab.cur'
         GO TO 999
      ENDIF
C
      nr=0
 10   READ(1,1000,END=999) cline1
 1000 FORMAT(A160)
      WRITE(6,*) cline1
      WRITE(6,FMT='(A160)') cline1
      GO TO 999
C
C---     Get rid of +/-
C     
      k=0
 20   k=k+1
      i1=INDEX(cline1,'+/-')
      IF(i1.GT.0) THEN
         cline1(i1:i1+2)='   '
         GO TO 20
      ELSE
         IF(k.EQ.1) GO TO 10
      ENDIF
C
      READ(cline1,*) (ilin(i),i=1,4),(rlin(i),i=1,2),(ilin(i),i=5,5)
     +              ,(rlin(i),i=3,12)
C
      nr=nr+1
      WRITE(6,*) ilin,rlin
      ii=0
      ir=0
      DO i=1,4
         ii=ii+1
         RTMP(i,nr)=ilin(ii)
      ENDDO
      DO i=5,6
         ir=ir+1
         RTMP(i,nr)=rlin(ir)
      ENDDO
      DO i=7,7
         ii=ii+1
         RTMP(i,nr)=ilin(ii)
      ENDDO
      DO i=12,15
         ir=ir+1
         RTMP(i,nr)=rlin(ir)
      ENDDO
      DO i=18,20
         ir=ir+1
         RTMP(i,nr)=rlin(ir)
      ENDDO
      DO i=22,24,2
         ir=ir+1
         RTMP(i,nr)=rlin(ir)
      ENDDO
      DO i=27,27
         ir=ir+1
         RTMP(i,nr)=rlin(ir)
      ENDDO

      GO TO 10
C
 999  RETURN
      CLOSE(UNIT=1)
      END

