      REAL FUNCTION A_ASYMM10(IVAR,IBG,NORM,IPOL,IPAIR,NPAIRS,ID)
C
C ===  Analyze the SCALER ntuple (one quad per entry)
C ==   Returns signal (scaler values) asymmetry or polarization: diff between the opposite helicities (a(1)-a(0))/(a(1)+a(0)):
C ==    IVAR >  0 - scaler number (1-Left, 2-Right, 3-Coin, 4-accid, 5- BCM...)
C ==     IBG >  0 - subtract the background (accid) JCNT(IBG...)
C       NORM =  0 - not normalized
C            =  1 - normalized to BCM
C            =  2 - normalized to scaler 
C       IPOL =  0 - just return the raw asymmetry
C            =  1 - return the raw asymmetry, with inverted sign for helm_coils<0
C            =  2 - return the polarization, take anal. power from a_anpow.f
C            = -1 - return the multiplicative factor (1/anpow/cos(angle)/P_target)
C            = -2 - calculate the asymmetry for 1-st pair - 2-nd pair
C      IPAIR = 1/2 - the pair number, =0 - all pairs (2)
C     NPAIRS >  1 - accumulate NPAIRS of pairs before histogramming into ID
C            = -1 - take the difference with the prev. cycle
C         ID <> 0 - histogram for sums of NPAIRS     
C
      IMPLICIT NONE
      INTEGER IVAR,IBG,NORM,IPOL,IPAIR,NPAIRS,ID
      INCLUDE ?
C
      INCLUDE 'inc/a_tpol.inc' ! target polarization
C
      LOGICAL  HEXIST
      REAL A_TPOL,A_ANPOW,A_BEAM
      EXTERNAL HEXIST,A_TPOL,A_ANPOW,A_BEAM
C
      INTEGER iq,n,nel,i,ipairs,kpairs,mpairs,ifirst,idrop
     +       ,idnevt0,j,ipairstot
      REAL a,an,anom,aa,dnom,pol,bg,as,pfact,tpol,ptar,astmp
C
      DOUBLE PRECISION dsig(2,2),dbg(2,2),dnorm(2,2),dv(2,2)
      CHARACTER cline*132,ctarg*1,chelm*6
C
      DATA ipairs/0/
      DATA ipairstot/0/
      DATA ifirst/1/
      DATA idnevt0/0/
C
      astmp=-2.
C
      IF(ifirst.NE.0.OR.IDNEVT.EQ.idnevt0) THEN
         idnevt0=idnevt
         ifirst=0
         ipairs=0
         ipairstot=0
         IF(ID.NE.0.AND.HEXIST(ID)) CALL HRESET(ID,'    ')
      ENDIF
C
      idrop=0
C
      IF(IPOL.LT.-2.OR.IPOL.GT.2) THEN
         WRITE(6,*) ' Parameter IPOL=',IPOL,' is out of range '
         IPOL=0
      ENDIF
      pfact=1.
      IF(IPOL.EQ.2.OR.IPOL.EQ.-1) THEN
         astmp=-20.
C         ptar=A_TPOL(0)
         ptar=ptarg
C         pfact=A_ANPOW(IZRUN,IVAR)*COS(ANGL*3.1416/180.)*ptar
C
         pfact=A_ANPOW(IZRUN,IVAR)*ptar
         IF(ABS(pfact).GT.1E-6) THEN
            pfact=1./pfact
         ELSE
            pfact=SIGN(1.E6,pfact)
            idrop=1
         ENDIF
      ENDIF
      IF(IPOL.GE.1) THEN
         IF(HELMH.GT.0.) pfact=-pfact ! positive polarization with positive coils - negative asymmetry
      ENDIF
C
      IF(idrop.NE.0) GO TO 999
C
      IF(IFZUG.NE.0) GO TO 999  ! remove faulty trains (quads)
      IF(NELEM.LT.2) GO TO 999  ! at least
      IF(IVAR.LT.1.OR.IVAR.GT.NSCAL) THEN
         WRITE(6,*) ' *** Error: zug=',IZUG,' IVAR=',IVAR
     +              ,' is out of range ',1,NSCAL
         GO TO 999
      ENDIF
C
      nel=NELEM
      anom=0.
      dnom=0.
      n=0
      kpairs=NPAIRS
      IF(NPAIRS.LT.1) kpairs=1
      IF(NPAIRS.EQ.-1) kpairs=2
      IF(ipairs.EQ.0) THEN
         DO i=1,2
            dsig(i,1)=0.D0
            dbg(i,1)=0.D0
            dnorm(i,1)=0.D0
         ENDDO
      ENDIF
      IF(ipairstot.EQ.0) THEN
         DO i=1,2
            dsig(i,2)=0.D0
            dbg(i,2)=0.D0
            dnorm(i,2)=0.D0
         ENDDO
      ENDIF
      mpairs=0
C
      DO iq=1,nel
         IF(IPAIR.EQ.0.OR.INT((iq+1)/2).EQ.IPAIR) THEN
            pol=2.*jhel(iq)-1.
            IF(IPOL.EQ.-2) THEN
               pol=1.
               IF(iq.GT.2) pol=-1.
            ENDIF
            a=JCNT(IVAR,iq)
            bg=0.
            IF(IBG.GT.0.AND.IBG.LE.nscal) bg=JCNT(IBG,iq)
            an=1.
            IF(NORM.EQ.1) THEN
               an=A_BEAM(iq)
C               an=JCNT(5,iq)
            ELSE IF(NORM.EQ.2) THEN
               an=JCNT(12,iq)
            ENDIF
            IF(an.GT.0.01) THEN
               aa=(a-bg)/an
               n=n+1
               dnom=dnom+aa
               anom=anom+aa*pol
C               write(6,*) 'a,an',iq,n,a,pol,an,anom,dnom
            ENDIF
C
            i=1
            IF(pol.LT.0.) i=2
C            write(6,*) mpairs,NPAIRS
            IF(ipairs.EQ.1.AND.NPAIRS.EQ.-1) THEN
               i=3-i
            ENDIF
            DO j=1,2
               dsig(i,j)=dsig(i,j)+DBLE(a)
               dbg(i,j)=dbg(i,j)+DBLE(bg)
               dnorm(i,j)=dnorm(i,j)+DBLE(an)
            ENDDO
C
            mpairs=mpairs+1
C
         ENDIF
      ENDDO
C
      ipairs=ipairs+mpairs/2
C     write(6,*) ipairs,kpairs
      IF(ipairs.GE.kpairs) THEN
         DO i=1,2
            dv(i,1)=0.D0
            IF(dnorm(i,1).GT.0.D0) 
     +               dv(i,1)=(dsig(i,1)-dbg(i,1))/dnorm(i,1)
         ENDDO
         as=-2.
         IF(IABS(IPOL).EQ.2) as=-20.
         IF(dv(1,1)+dv(2,1).GT.0.D0) 
     +         as=(dv(1,1)-dv(2,1))/(dv(1,1)+dv(2,1))
         IF(ID.NE.0) CALL HF1(ID,as*pfact,1.)
         ipairs=0
      ENDIF
C
      IF(n.GT.1.AND.dnom.GT.0.) THEN
         astmp=anom/dnom*pfact
         IF(IPOL.EQ.-1) astmp=pfact
C         write(6,*) 'anom,dnom ', anom,dnom,A_ASYMM10
      ENDIF
C
 999  A_ASYMM10=astmp
      END
C
      INCLUDE 'a_tpol.f'
      INCLUDE 'a_anpow.f'
      INCLUDE 'a_beam.f'

