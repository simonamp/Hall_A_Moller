      INTEGER FUNCTION IR_CDIR(LUN,DIR)
C
C--- RZ: changes directory to LUN/DIR and returns 1 if OK, 0 if not
C
      IMPLICIT NONE
      INTEGER  LUN
      CHARACTER DIR*(*)
C
      INTEGER IQUEST
      COMMON/QUEST/ IQUEST(100)
C
      INTEGER l1,lenc
      CHARACTER chdnam*64,cnmb*2
C
C     ------------------------------------------------------------------
C
      IR_CDIR=0
C
      IF(LUN.LT.1.OR.LUN.GT.99) THEN
         WRITE(6,*) 'Error: LUN=',LUN,' is out of range'
         GO TO 999
      ENDIF
C
C      WRITE(6,*) LUN,' ',DIR
      lenc=INDEX(DIR,' ')-1
      IF(lenc.LT.0) lenc=LEN(DIR) 
C      WRITE(6,*) LUN,' ',DIR,' lenc=',lenc
C
      WRITE(cnmb,FMT='(I2)') LUN
      l1=2
      IF(LUN.GT.9) l1=1
      chdnam='//LUN'//cnmb(l1:2)//'/'//DIR(1:lenc)//':'
      l1=INDEX(chdnam,':')
C
      CALL RZCDIR(chdnam(1:l1-1),' ')
      IF(IQUEST(1).NE.0) THEN
         WRITE(6,*) 'Error: can not go to directory ',chdnam(1:l1-1)
         GO TO 999
      ENDIF
C      CALL RZLDIR(' ',' ')
C
      IR_CDIR=1
C
 999  CONTINUE
      END





