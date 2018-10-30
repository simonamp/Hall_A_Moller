      SUBROUTINE PNTINI
C
C---     Initialize an CW NTUPLE for run-wise Moller data
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
      fnam='pol_runs.nt'
      lrec=1024
      CALL HROPEN(2,'pol_ntup',fnam,'N',lrec,istat)
      IF(istat.NE.0) THEN
        WRITE(6,1100) istat,lun
        GO TO 999
      ENDIF
C
      id=1
      CALL HBNT(id,'RUN-wise data',' ')
C
      CALL PNTPINI(id)
      CALL PNTIINI(id)
C
 999  CONTINUE
 1100 FORMAT(' *** PNTINI: Error openning NTUPLE',2I6)
      END
C
      INCLUDE 'pntpini.f'
      INCLUDE 'pntiini.f'











