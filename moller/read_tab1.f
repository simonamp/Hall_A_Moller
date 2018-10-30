      SUBROUTINE READ_TAB1
C
C===     Read  a runs.tab.cur.1,2 table
C
      IMPLICIT NONE
      INCLUDE 'inc/v_run.inc'
      INCLUDE 'inc/v_runset.inc'
C
      INTEGER i,j,irun,ir,lun(3),iost
      INTEGER itmp(50)
      INTEGER mxres
      PARAMETER (mxres=MXRESW/4)
C
      CHARACTER fnam(3)*14
      REAL tim
C
      DATA ir/0/
      DATA fnam/'runs.tab.cur.1','runs.tab.cur.2','cur.tmp'/
      DATA lun/1,2,3/
C
      DO i=1,3
         write(6,*) i,lun(i),' ',fnam(i)
 5       OPEN(UNIT=lun(i),FILE=fnam(i),STATUS='OLD',IOSTAT=iost)
         IF(iost.NE.0) THEN
            WRITE(6,*) ' Error opening ',fnam(i)
            GO TO 999
         ENDIF
         REWIND lun(i)
      ENDDO
C
 10   ir=ir+1
C      if(ir.ge.2) GO TO 999
C      write(6,*) ' 1 ir=',ir
      READ(lun(1),1200,END=999) irun,(RESRUN(i,ir),i=1,3)
     +              ,(RESRUN(i,ir),i=4,5),RESRUN(7,ir)
     +              , RESRUN(3+mxres*2,ir),ERESRUN(3+mxres*2,ir)
C      write(6,*) ' 2 ir=',ir,irun
      READ(lun(2),1300,END=999) 
     +                RESRUN(3+mxres*3,ir),ERESRUN(3+mxres*3,ir) 
     +              ,(CONSRUN(i,ir),i=1,3)
     +              , RESRUN(1+mxres*3,ir),RESRUN(2+mxres*3,ir) 
     +              , RESRUN(5+mxres*2,ir),ERESRUN(5+mxres*2,ir) 
     +              , CONSRUN(6,ir)
C      write(6,*) ' 3 ir=',ir,irun
      DO j=1,28
         RUNSET(j,ir)=-999999.
      ENDDO
      READ(lun(3),*) (itmp(j),j= 1, 9),(RUNSET(j,ir),j=10,13)
     +              ,(itmp(j),j=14,18),(RUNSET(j,ir),j=19,20)
     +              ,(itmp(j),j=21,26),(RUNSET(j,ir),j=27,28)
C      write(6,*) itmp
      IF(itmp(1).EQ.irun) THEN
         DO j=1,28
            IF(RUNSET(j,ir).LT.-999990.) RUNSET(j,ir)=itmp(j) 
         ENDDO
         RUNSET(7,ir)=RUNSET(7,ir)+(RUNSET(5,ir)+RUNSET(6,ir)/60.)/24.
         RUNSET(8,ir)=RUNSET(8,ir)/1000.
         RUNSET(10,ir)=RUNSET(10,ir)/1000.
      ELSE
         WRITE(6,*) ' *** Error: run mismatch:',irun,itmp(1)
      ENDIF
C      WRITE(6,*) ' irun=',irun,(RUNSET(j,ir),j=1,4)
C
 1200 FORMAT(1X,I4,2F8.0,F7.0,F7.0,F8.0,F7.0,2X
     +           ,F7.4,3X,F7.4)
 1300 FORMAT(2X,F7.4,3X,F7.4
     +           ,2X,F6.1,F7.3,1X,F7.4
     +           ,2X,F7.4,2X,F7.4
     +           ,1X,2F8.5,F5.1)
      IF(irun.GT.1.AND.irun.LE.MXKRUN) THEN
         KRUNPNT(irun)=ir
         KRUSTAT(1)=MAX(KRUSTAT(1),ir)
      ENDIF
C
      IF(ir.LT.MXRSET) GO TO 10
C
      WRITE(6,*) ' *** runs.tab.cur: is longer than the RUNSET vector' 
C
 999  CONTINUE
      WRITE(6,*) ir,' runs read'
      END
