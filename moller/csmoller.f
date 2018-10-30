      REAL FUNCTION CSMOLLER(THETX)
C
C---     Moller cross-section d(sigma)/d(Omega) for:
C---         COS(theta_lab)=COSTHX - scattering angle in Lab
C---         CSDAT(1)              - beam energy,GeV
C---         CSDAT(2)              - target Z
C                                 
C
      IMPLICIT NONE
      REAL COSTHX,THETX
      VECTOR CSDAT(3)
C
C      REAL p,pcm,e,ecm,s,sqs,gam,bet,costh,sinth,costhcm,p1,e1
C     +    ,q1,q2,a,b,c,cos2,sin2,deriv,angterm,cross,sinthcm
      DOUBLE PRECISION p,pcm,e,ecm,s,sqs,gam,bet,costh,sinth,costhcm
     +    ,p1,e1,q1,q2,a,b,c,cos2,sin2,deriv,angterm,cross,sinthcm,ztar
      REAL ame,alpha
      REAL gevfm               !  1/1GeV in fm 
      DATA ame/0.000511/
      DATA gevfm/0.1973/
C
C     ------------------------------------------------------------------
C
      CSMOLLER=-1.
      COSTHX=COS(THETX)
C      COSTHX=THETX
      alpha=1./137.
      costh=COSTHX
C
      e=CSDAT(1)
      ztar=CSDAT(2)
      p=SQRT(e**2-ame**2)
C
      s=2.*ame**2+2.*ame*e
      sqs=SQRT(s)
      gam=(e+ame)/sqs
      bet=SQRT(1.-1./gam**2)
      ecm=sqs/2.
      pcm=SQRT(ecm**2-ame**2)
C
C      costhcm=costh
C      costh=gam*SQRT((1.+costhcm)/(1.+gam**2+costhcm*(gam**2-1.)))
C
      IF(costh.LT.0..OR.costh.GT.1.) THEN
         WRITE(6,*) ' *** Error: costh is out of range =',costh
         GO TO 999
      ENDIF
C
C---     Find the scatternig angle in CM
C
      sinth=SQRT(1.-costh**2)
      q1=costh**2
      q2=(gam*sinth)**2
      costhcm=(q1-q2)/(q1+q2)
      sinthcm=SQRT(1.-costhcm**2)
      cos2=(1.+costhcm)/2.
      sin2=1.-cos2
      a=pcm*sinthcm
      b=gam*(pcm*costhcm+bet*ecm)
      c=b/SQRT(a**2+b**2)
C
C      WRITE(6,*) '..=',bet,gam,sqs,ecm,pcm,bet*ecm/pcm
C     +          ,costh,sinth,costhcm,a,b,c
C
C---     Cross-section (CM) 
C
      deriv=4.*ABS(costh)*gam**2/(q1+q2)**2   ! solid angle CM ==> Lab
      angterm=((1.+cos2**2)/sin2**2+2./sin2/cos2+((1.+sin2**2)/cos2**2))
      cross=alpha**2/8./ecm**2*deriv*angterm
C      cross=alpha**2/8./ecm**2*angterm
C      angterm=((1.+costhcm)*(3.+costhcm**2)/sinthcm**2)**2
C      cross=(alpha/2./ame)**2*angterm
C      cross=(2.82/2./gam*(4.-sinthcm**2)/sinthcm**2)**2
      cross=cross*gevfm**2/100.*ztar
      CSMOLLER=cross
C      WRITE(6,*) 'Cross-section in barn=',cross
C      CSMOLLER=ACOS(costhcm)*180./3.1415
C
 999  CONTINUE
      END







