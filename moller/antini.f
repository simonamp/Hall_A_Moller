      SUBROUTINE ANTINI
C
C---     Initialize an CW NTUPLE for asymmetry Moller data
C
      IMPLICIT NONE
C
C===      Counting rates/polarizations for a given run
C
      INTEGER istat,id,lrec,lun
      CHARACTER fnam*16
C
C     ------------------------------------------------------------------
C
      lun=2
      fnam='asym.nt'
      lrec=1024
C      CALL HROPEN(2,'LUN2',fnam,'N',lrec,istat)
C      IF(istat.NE.0) THEN
C        WRITE(6,1100) istat,lun
C        GO TO 999
C      ENDIF
C
      CALL HCDIR('//LUN2',' ')
      id=1
      CALL HBNT(id,'Asymmetry scaler data',' ')
C
      CALL ANTPINI
C      CALL ANTIINI
C
 999  CONTINUE
 1100 FORMAT(' *** ANTINI: Error openning NTUPLE',2I6)
      END
C
      INCLUDE 'antpini.f'
C      INCLUDE 'antiini.f'
