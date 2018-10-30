      REAL FUNCTION WAVE_TR(X)
C
C===    Fit to the polarization(wien_angle) taking the transverse polarization into account
C
      IMPLICIT NONE
      REAL X  ! wien angle deg
C
      INCLUDE 'inc/v_poltr.inc'
C
      COMMON/PAWPAR/ PARA(10)
      VECTOR PAR(10)
      VECTOR IPFIT(10)
      REAL dif,ddif,tang(2),wang,wang0,parl(10),radeg,ptfac,plfac
      INTEGER ip,jp,i
C
      WAVE_TR=0. 
      radeg=ACOS(0.)/90.
C
      DO i=1,10
         IF(IPFIT(1).EQ.0) THEN
            parl(i)=PAR(i)
         ELSE
            parl(i)=PARA(i)
         ENDIF
      ENDDO
      wang=X*radeg
      wang0=parl(2)*radeg
C
      IF(IPFIT(2).EQ.0) THEN
        parl(3)=PAR(3)
      ENDIF
C
C---  Find the point (from the angle)
C
      ip=0
      dif=9999.
      DO jp=1,MXWIEN
         ddif=ABS(X-ANGLES(jp))
         IF(ddif.LT.dif) THEN
            dif=ddif
            ip=jp
         ENDIF
      ENDDO
      ptfac=0.
      plfac=1.
C      WRITE(6,*) 'ip=',ip,IPFIT(2)
      IF(IPFIT(3).NE.0.AND.ip.GT.0) THEN
         DO i=1,2
            tang(i)=ATANG1S(ip,i)*radeg
         ENDDO
         IF(IPFIT(3).EQ.1) THEN
            ptfac=(TAN(tang(1))+TAN(tang(2)))/2.
         ELSE IF(IPFIT(3).EQ.2) THEN
            ptfac=(TAN(tang(1))-TAN(tang(2)))/2.
            plfac=0.
         ENDIF
      ENDIF
C      WRITE(6,*) 'ptfac',ptfac,plfac
C
C      PRINT *,X,PARA(1),PARA(2),PARA(3),period
      WAVE_TR=parl(1)*(COS((wang-wang0)*parl(3))*plfac
     +                +SIN((wang-wang0)*parl(3))*ptfac/7.)
      END





