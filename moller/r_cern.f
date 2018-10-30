      PROGRAM R_CERN
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(50000)
      DIMENSION LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
C
      DIMENSION KEY(10)
      CHARACTER*8 CHTAG(10),RTIME
C #if defined(CERNLIB_TESTCX)
      CHARACTER*16 CHDIR
C #endif
      CHARACTER*1 BSLSH
C      PARAMETER (BSLSH=CHAR(92))
      DIMENSION IA(10000)
      DATA IA/10000*0/
*
*     -----------------------------------------------------------------
*
C
C      OPEN(UNIT=1,FILE='rzcomp.dat',ACCESS='DIRECT',RECL=6000,
C     +      STATUS='UNKNOWN')


      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(50000))
      CALL TIMED(T0)
C      CALL ETIMEC(RTIME)
C
C            Create RZ file
      LUN1=1
      CALL RZOPEN(1,CHDIR,'RZTEST3.DAT','NX',1024,ISTAT)
C
C #if defined(CERNLIB_TESTCX)
C      CALL RZOPEN(LUN1,CHDIR,'RZTEST3.DAT','CNX',1500,ISTAT)
C       if(ISTAT.NE.0)print*,' Error trying to create file'
C      CALL RZMAKE(LUN1,'RZTEST',1,'I','TOPTAG  ',1024,'CIX')
C #else
      CALL RZMAKE(1,'RZTEST',1,'I','TOPTAG  ',1024,'X')
C #endif
C
C            Create 2 subdirectories
C
      CHTAG(1)='INDEX'
      CALL RZMDIR('DIR1',1,'I',CHTAG)
      CALL RZMDIR('DIR2',1,'I',CHTAG)
C
      CALL RZCDIR('DIR1',' ')
C
      CALL TIMED(T1)
C      CALL ETIMEC(RTIME)
      PRINT 1000,RTIME,T1
 1000 FORMAT(' STEP 1: REAL TIME ',A,' CP TIME',F10.5,' SECONDS')
C
      CALL RZEND('RZTEST')

      END







