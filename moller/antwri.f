      SUBROUTINE ANTWRI(LIN,LOUT)
C
C---     Write a CW NTUPLE for scaler "train" data
C---     Read the data from LIN (should have been opened) and 
C---     Write ntuple to LOUT: open and close here
C
      IMPLICIT NONE
      INTEGER LIN,LOUT
C
      INCLUDE 'inc/cntasym.inc'
C
C===      Counting rates/polarizations for a given run
C
      INTEGER istat,id,lrec,nzug,icycle,i,j,ksca,kelem,k1,k2,nerr
      CHARACTER fnam*16

C=== For Qweek configuration (helicity~1000Hz) form Oct.15 2010
C http://www.jlab.org/~brads/Manuals/pawfaq/
      COMMON/QUEST/IQUEST(100)
C      CALL HLIMIT(150000)

C
C     ------------------------------------------------------------------
C



      IF(LIN.LE.0.OR.LOUT.LE.0) THEN
         WRITE(6,*) ' *** Error in ANTWRI: wrong LIN,LOUT=',LIN,LOUT
         GO TO 999
      ENDIF
C
      fnam='scal.nt'
C     lrec=1024
C=== For Qweek configuration (helicity~1000Hz) form Oct.15 2010
      IQUEST(10)=256000
      lrec=1024

CC      IQUEST(10)=65000
CC      lrec=8192

C====  
      CLOSE(UNIT=LOUT)
C      CALL HROPEN(LOUT,'scal',fnam,'N',lrec,istat)
      CALL HROPEN(LOUT,'scal',fnam,'NQE',lrec,istat)
      IF(istat.NE.0) THEN
        WRITE(6,1100) istat,LOUT
        GO TO 999
      ENDIF
C
      CALL HCDIR('//scal',' ')
C
      id=1
      CALL HBNT(id,'Train scaler data',' ')
C
      CALL ANTPINI(id)
C
      nzug=0
      nerr=0
C
      REWIND LIN
      READ(LIN,ERR=991,END=992) kelem,ksca
      REWIND LIN
C
 10   CONTINUE
      CALL ANTEVINI
      READ(LIN,ERR=991,END=992) k1,k2
     +               ,KRUN
     +               ,ANGL   
     +               ,HELMH  
     +               ,PTARG  
     +               ,ITARG
     +               ,(XYTARG(i),i=1,2)
     +               ,KZUG   
     +               ,IFZUG  
     +               ,NELEM  
     +               ,NSCAL
     +              ,(JTICK(i) 
     +               ,JDTICKAF(i) 
     +               ,JDTICKAL(i) 
     +               ,JFLA(i)     
     +               ,JADC(i)     
     +               ,JHEL(i)     
     +               ,(JCNT(j,i),j=1,ksca),i=1,kelem)
      nzug=nzug+1
      IF(IFZUG.NE.0) nerr=nerr+1
C
C      WRITE(77,*) KRUN,KZUG,NELEM,(JHEL(i),i=1,k1)
      CALL ANTFIL(id)
C
      GO TO 10
 991  WRITE(6,*) ' *** ANTWRI: error reading file LIN ',LIN,nzug
      GO TO 300
 992  WRITE(6,*) ' === ANTWRI: ',nzug,' helicity supercycles read    '
     + ,nerr,'  marked bad' 
C
 300  CONTINUE
C
C      CALL HCDIR('//scal',' ')
      CALL HROUT(id,icycle,' ')
C
      CALL HREND('scal')
      CLOSE (UNIT=LOUT)
C
 999  CONTINUE
 1100 FORMAT(' *** ANTINI: Error openning NTUPLE',2I6)
      END
C
      INCLUDE 'antpini.f'
      INCLUDE 'antevini.f'
      INCLUDE 'antfil.f'
