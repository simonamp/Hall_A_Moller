      REAL FUNCTION FOIL_EDGE(XX)
C
C ---      The edge of a foil, seen  in X-scanning data, assuming a round beam 
C      PAR(1) - amplit
C         (2) - X - edge
C         (3) - R - radius of the neam spot
C
      IMPLICIT NONE
      REAL XX
      COMMON/PAWPAR/ PARA(10)
      REAL PARA
      VECTOR PAR(10)
      VECTOR IPFIT(1)
C
      REAL a,x,r,xr,res
C
      a=PARA(1)
      r=ABS(PARA(3))
      x=XX-PARA(2)
      IF(IPFIT(1).NE.0) THEN
         a=PAR(1)
         r=ABS(PAR(3))
         x=XX-PAR(2)
C         write(6,*) 'xx=',x,XX
      ENDIF
      xr=x*1.E20
      IF(r.NE.0.) xr=x/r
C
C      write(6,*) a,r,x,xr
      IF(xr.LE.-1.) THEN
         res=1.
      ELSE IF(xr.LT.1.) THEN
         res=1.-(ACOS(-xr)+xr*SQRT(1.-xr**2))/3.1416
C         write(6,*) 'res=',res
      ELSE
         res=0.
      ENDIF
C
      FOIL_EDGE=a*res
C
      RETURN
      END
