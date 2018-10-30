      PROGRAM R_MAKE
C
C---   Creates an exportable RZ file with a name template.rz for Moller results
C
      IMPLICIT NONE
C
      INTEGER IXSTOR,IXDIV,IFENCE,LEV,LEVIN,LQ,IQ
      REAL    BLVECT,Q
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(50000)
      DIMENSION LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
C
      INTEGER IQUEST
      COMMON/QUEST/ IQUEST(100)
C
      INTEGER    mxrec
      PARAMETER (mxrec=200)
      INTEGER lun,lrecl,istat,i,nrec,iev,key(2),icycle,nkeys,lenc
     +       ,l1,l2
      CHARACTER chdir*8,chtag(2)*8,chopt*8,chdnam*32,cnmb*2
C
      CALL MZEBRA(-1)
      CALL MZSTOR(IXSTOR,'/CRZT/',' ',IFENCE,LEV,BLVECT(1),BLVECT(1),
     +            BLVECT(5000),BLVECT(50000))
C
      lun=1
      lrecl=1024
      chdir='LUN1'
      chopt='NX'
      CALL RZOPEN(lun,chdir,'template.rz',chopt,lrecl,istat)
      WRITE(6,*) 'chdir=',chdir
      IF(istat.NE.0) THEN
         WRITE(6,*) ' *** R_MAKE: Error opening file LUN=',lun
     +            ,' istat=',istat
     +            ,' IQUEST=',(IQUEST(i),i=1,6)
         GO TO 999
      ENDIF
C
      CALL RZMAKE(lun,'chdir',1,'H','dummy',1024,'X')
C
      IF(IQUEST(1).NE.0) THEN
         WRITE(6,*) ' *** R_MAKE: Error making file LUN=',lun
     +            ,' IQUEST=',(IQUEST(i),i=1,6)
         GO TO 999
      ENDIF
C
      CALL RZCLOS(chdir,'A')
C
 999  CONTINUE
      END


