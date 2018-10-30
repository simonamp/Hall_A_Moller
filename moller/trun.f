      SUBROUTINE TRUN
      REWIND 78
      n=0
 10   READ(78,FMT='(I1)',END=999) i
      x=TSTSEQ(i)
      n=n+1
      IF(n.LT.70) GO TO 10
 999  CONTINUE
      END
C
      INCLUDE 'tstseq.f'
