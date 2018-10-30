      REAL FUNCTION SCADIFF(IS)
C
C===     Return the increment of scaler IS
C
      IMPLICIT NONE
      INTEGER IS
C
      INCLUDE ?
C
      VECTOR IPVSCA(50000)
      VECTOR KVSCA(16,16000)
C
      INTEGER iev,ip
C
C     ------------------------------------------------------------------
C
      SCADIFF=-100.
      iev=IDNEVT
      IF(IS.LT.1.OR.IS.GT.16) GO TO 999
      IF(NSCA.LE.0) GO TO 999
C
      IF(iev.GT.0.AND.iev.LE.50000) THEN
         ip=IPVSCA(iev)
         IF(ip.GT.1.AND.ip.LE.16000) THEN
            SCADIFF=ISCA(IS)-KVSCA(IS,ip-1)
         ENDIF
      ENDIF
                  
 999  RETURN
      END
C
