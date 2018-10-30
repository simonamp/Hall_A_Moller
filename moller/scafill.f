      REAL FUNCTION SCAFILL(IFLA)
C
C===     Fill vectors: ivp=IPVSCA(idnevt) - link between event numbers and scaler pointer 
C===                       KVSCA(16,ivp) - scaler values 
C
      IMPLICIT NONE
      INTEGER IFLA      
C
      INCLUDE ?
C
      VECTOR IPVSCA(50000)
      VECTOR KVSCA(16,16000)
C
      INTEGER iev,ip,i
      DATA ip/0/
C
C     ------------------------------------------------------------------
C
      SCAFILL=0.
      iev=IDNEVT
      IF(iev.EQ.1) ip=0
C
      IF(iev.GT.0.AND.iev.LE.50000) THEN
         IPVSCA(iev)=0
C
         IF(NSCA.GT.0) THEN
            ip=ip+1
            IF(ip.LE.16000) THEN
               IPVSCA(iev)=ip
C         
               DO i=1,MIN(NSCA,16)
                  KVSCA(i,ip)=ISCA(i)
               ENDDO
               SCAFILL=ip
            ENDIF
         ENDIF
                  
      ENDIF
C
      RETURN
      END
C
