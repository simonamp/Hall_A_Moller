      REAL FUNCTION DTICK(IFLA)
C
C===     Tick interval to the previous event 
C
      IMPLICIT NONE
      INTEGER IFLA      !  =0 - all events, =1 - between the last adc and scaler
C
      INCLUDE ?
C
      INTEGER itick0,itick1
      SAVE itick0
      DATA itick0/-1/
C
C     ------------------------------------------------------------------
C
      DTICK=-1
      itick1=ITICK
      IF(itick0.GT.0) THEN
         IF(IFLA.EQ.0.OR.
     +     (IFLA.EQ.1.AND.NSCA.GT.0)
     +       ) THEN 
            DTICK=ITICK-itick0
         ENDIF
      ENDIF
      itick0=ITICK
C
      RETURN
      END
C
