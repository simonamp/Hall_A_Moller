      REAL FUNCTION THET_DEC(X)
C
C===      Theta in LAB of a J/Psi decay product, X- COS(Thet_CM)
C
      IMPLICIT NONE
      REAL X
C
      REAL e,p,am,am1,amnucl,gam,bet,ecm,pcm,pt,pl,sl,sl1,thet
C
      e=8.2
      am=3.1
      am1=0.105
      amnucl=0.938
C
      p=SQRT(e**2-am**2)
      gam=e/am
      bet=p/e
C
      ecm=am/2.
      pcm=SQRT(ecm**2-am1**2)
      pl=gam*(pcm*X+bet*ecm)
      pt=pcm*SQRT(1.-X**2)
      sl=pt/pl
      thet=ATAN(sl)
C
      THET_DEC=thet
C
      END
