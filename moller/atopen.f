      SUBROUTINE ATOPEN(LUN,IFL)
C
C===    Open temporary file for asymmetry data
C===    IFL=1 - write, IFL=0 - read
C
      IMPLICIT NONE
      INTEGER LUN,IFL
C
      CHARACTER cnam*80
C
      cnam='/tmp/moller_scal.dat'
C
      IF(IFL.EQ.0) THEN
         OPEN(LUN,FILE=cnam,STATUS='OLD',FORM='UNFORMATTED')
      ELSE
         OPEN(LUN,FILE=cnam,STATUS='UNKNOWN',FORM='UNFORMATTED')
      ENDIF
      REWIND LUN
C
      END
