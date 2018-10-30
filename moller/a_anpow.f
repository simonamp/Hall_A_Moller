      REAL FUNCTION A_ANPOW(IRUN,IVAR)
C
C===    Return the analyzing power
C        IRUN - run number
C        IVAR - the variable number (1 - Left, 2 -Right, 3 - Coinc., 4 - Accidentals)
C
      IMPLICIT NONE
      INTEGER IRUN,IVAR
C
      INTEGER mxkey
      PARAMETER (mxkey=120)
      VECTOR RUN_SETT(mxkey)
C
      REAL anpow,e0
      INTEGER jrun,i1,i2
      CHARACTER crun*12,cline*132
C
      anpow=0.76
C
      IF(IRUN.LT.11790) THEN
         anpow=0.76
      ELSE IF(IRUN.LT.14000) THEN
         jrun=INT(RUN_SETT(100)+0.1)
         IF(IRUN.NE.jrun) THEN
C
C---       Read the settings
C
            WRITE(6,*) ' Initialize the settings,for run=',IRUN
            WRITE(crun,FMT='(I12)') IRUN
            CALL STR_TRIM(crun,i1,i2)
            cline='exec get_settings run='//crun(i1:i2)
C              WRITE(6,*) cline
            CALL KUEXEL(cline)
         ENDIF
         e0=RUN_SETT(1)
         anpow=0.75257    ! temporary setting - needs to be updated
      ELSE IF(IRUN.LT.99999) THEN
         anpow=0.75257   ! 1.05 GeV PREX
      ENDIF
C
 999  A_ANPOW=anpow
      RETURN
      END
      INCLUDE 'str_trim.f'

      
