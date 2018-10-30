      REAL FUNCTION SPIN_AB(EREL)
C
C===    Spin angle difference between halls A and B
C===    E=E0*(1.+EREL)
C===   vprec1(1) - E0 - linac (GeV)
C===         (2) - number of passes (A) 
C===         (3) - number of passes (B)
C===         (4) - number of passes (C)
C===         (5) - = 0 do not print the values
C===               = 1 print only the values
C===               = 2 print also a header
C===         (6) - = lun
C
      IMPLICIT NONE
      REAL EREL
C
      VECTOR VPREC1(6)
      REAL a,elin,turn(3),ang(3),angr(3),pass
      INTEGER ihall,k,lun,npas(3)
C
      REAL alpha         !  E inject/E linac
      REAL arc(3)        !  arc influence
      REAL g2            ! (g-2)/2
      REAL ame           ! electron mass
      DATA alpha/0.1125/
      DATA g2/0.00116/
      DATA ame/0.000511/
C
C     ------------------------------------------------------------------
C
      lun=INT(VPREC1(6)+0.1)
      IF(lun.LE.0) lun=6
C
      arc(1)=-1./2.4
      arc(2)=0.
      arc(3)=1./2.4
C
      elin=vprec1(1)*(1.+EREL)
      a=elin/ame*g2
C
      DO ihall=1,2
         pass=vprec1(ihall+1)
         npas(ihall)=INT(pass+0.1)
         turn(ihall)=a*(2.*pass**2-pass*(1.-2*alpha+arc(ihall))
     +      -alpha*(1.+arc(ihall)/2.))
         ang(ihall)=turn(ihall)*180.
         angr(ihall)=(turn(ihall)-(INT(turn(ihall)+0.5)))*180.
      ENDDO
      SPIN_AB=(angr(2)-angr(1))
C
      k=INT(VPREC1(5)+0.1)
      IF(k.GT.1) THEN
         WRITE(lun,1000)
 1000    FORMAT(' E/linac GeV dE/E  Pass: A B C'
     +         ,'  Turns: A       B       C'
     +         ,'    Angles: A       B       C (deg)   A-B (deg)  '
     +         ,'A-B reduced (deg)')
      ENDIF
      IF(k.GT.0) THEN
         WRITE(lun,1100) elin,EREL,npas,turn,ang,ang(2)-ang(1)
     +                ,angr(2)-angr(1)
 1100 FORMAT(4X,F7.4,1X,F7.4,5X,3I2,5X,3F8.3,3X,3F8.1
     +      ,4X,F8.1,4X,F7.2)
      ENDIF
C
      END

