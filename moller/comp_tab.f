      SUBROUTINE COMP_TAB(ICOL1,ICOL2,ICOL3,ICOL4)
C
C ===     Read in the Compton polarimeter html result table comp.in,
C ===      extracts the date and the result of the measurement and prints 
C ===      to a file comp.out
C ===      Write onlyt the columns ICOL1-4 (=0 - ignore)
C ===      The date is parsed
C
      IMPLICIT NONE
      INTEGER ICOL1,ICOL2,ICOL3,ICOL4
      INTEGER LENSTR
C
      INTEGER    mxcol
      PARAMETER (mxcol=100)
      INTEGER nline,nmeas,icol(4),i,lens,ist,ncol,kcol,lcol(mxcol),j
     +       ,idat(5),k
      CHARACTER line*132,wrd*80,col(mxcol)*80,cc*1
      LOGICAL colopen,comhtml,wrim
      REAL pol(2)
C
C     ------------------------------------------------------------------
C
      OPEN(UNIT=1,FILE='comp.in',STATUS='OLD')
      REWIND 1
      OPEN(UNIT=2,FILE='comp.out',STATUS='UNKNOWN')
      REWIND 2
C
      icol(1)=ICOL1
      icol(2)=ICOL2
      icol(3)=ICOL3
      icol(4)=ICOL4
      kcol=0
      DO i=1,4
         IF(icol(i).GT.0) kcol=i
      ENDDO
C
      nline=0
      nmeas=0
      ncol=0
      colopen=.false.
      comhtml=.false.
C
 10   READ(1,FMT='(A132)',END=999) line
      nline=nline+1
      lens=LENSTR(line)
C      PRINT *,nmeas,lens,' ',line(1:40)
C      WRITE(2,FMT='(40Z3)') (ICHAR(line(i:i)),i=1,40)
      IF(lens.LE.0) GO TO 10
C
C---      Parse the line character-wise
C
      ist=1
 20   CONTINUE
      IF(line(ist:ist).EQ.'>') THEN
         comhtml=.false.
      ELSE IF(line(ist:ist).EQ.'<') THEN
C
C---     Find the HTML command
C
         comhtml=.true.
         IF(line(ist:ist+2).EQ.'<TR'.OR.line(ist:ist+3).EQ.'</TR') THEN
C
C---     Write the previous line
C
            IF(ncol.GT.0) THEN
               nmeas=nmeas+1
               wrim=.true.
               DO i=1,kcol
                  IF(lcol(icol(i)).EQ.0) THEN
                     lcol(icol(i))=2
                     col(icol(i))(1:2)='??'
                     wrim=.false.
                  ENDIF
                  IF(lcol(icol(i)).LT.2) wrim=.false.
                  IF(INDEX(col(icol(i)),'?').GT.0) wrim=.false.
                  IF(lcol(icol(i)).LT.5) wrim=.false.
C
C---          Remove +/-
C
                  j=INDEX(col(icol(i)),'+/-')
                  IF(j.GT.0) col(icol(i))(j:j+2)='  '
C
C---          Remove (...
C
C                  j=INDEX(col(icol(i)),'(')
C                  IF(j.GT.0) lcol(icol(i))=j-1
               ENDDO
C
C---          Parse the date
C
C               IF(wrim) THEN
C                  PRINT *, 
C     +              '##',(col(icol(i))(1:lcol(icol(i))),' ',i=1,kcol)
C               ENDIF
               IF(wrim) THEN
                  j=0
                  DO i=1,lcol(icol(1))
                     cc=col(icol(1))(i:i)
                     IF(cc.EQ.'/'.OR.
     +                  cc.EQ.':'.OR.
     +                  cc.EQ.'-'.OR.
     +                  cc.EQ.'['.OR.
     +                  cc.EQ.']') THEN
                        cc=' '
                        j=j+1
                     ENDIF
                     IF(ICHAR(cc).LT.48.OR.ICHAR(cc).GT.57) cc=' '
C                     print *,nmeas,j,' ',cc
                     col(icol(1))(i:i)=cc
                     IF(j.EQ.5) THEN                        
                        lcol(icol(1))=i
                        GO TO 40
                     ENDIF
                  ENDDO
                  IF(j.LT.5) wrim=.false.
 40               CONTINUE
C                  IF(wrim) THEN
C                     wrd=' '
C                     wrd=col(icol(1))(1:lcol(icol(1)))
C                     PRINT *,wrd
C                     PRINT *,(ICHAR(wrd(i:i)),i=1,80)
C                     READ(wrd,*) idat
C                     WRITE(col(icol(1)),FMT='(5I3)') idat
C                     PRINT *,idat
C                     lcol(icol(1))=15
C                  ENDIF
               ENDIF
C
               IF(wrim) THEN
                  wrd=' '
                  j=0
                  DO i=1,kcol
                     DO k=1,lcol(icol(i))
                        cc=col(icol(i))(k:k)
                        j=j+1
                        IF((ICHAR(cc).GE.48.AND.ICHAR(cc).LE.57).OR.
     +                      cc.EQ.'.'.OR.cc.EQ.'-') THEN
                           wrd(j:j)=cc
                        ELSE
                           wrd(j:j)=' '
                        ENDIF
                     ENDDO
                  ENDDO
C                  PRINT *,wrd
                  READ(wrd,*) idat,pol
                  idat(3)=idat(3)+2000
                  WRITE(wrd,FMT='(2I3,I5,2I3,2X,2F5.1)') idat,pol
C                  PRINT *,idat
C                  WRITE(2,*) 
C     +                (col(icol(i))(1:lcol(icol(i))),' ',i=1,kcol)
                  WRITE(2,*) wrd
               ELSE
                  PRINT *, 'reject',nmeas,(lcol(icol(i)),i=1,kcol)
               ENDIF
               ncol=0
            ENDIF
            colopen=.false.
         ELSE IF(line(ist:ist+2).EQ.'<TD') THEN
            colopen=.true.
            ncol=ncol+1
            lcol(ncol)=0
         ENDIF
C
C---     Find the end of the HTML command
C
      ELSE
C
C---     Not a part of HTML command: add to the current column 
C
         IF(colopen.AND..not.comhtml) THEN
            lcol(ncol)=lcol(ncol)+1
            col(ncol)(lcol(ncol):lcol(ncol))=line(ist:ist)
         ENDIF
      ENDIF
      ist=ist+1
      IF(ist.LE.lens) GO TO 20
C
      IF(nmeas.LT.100000) GO TO 10
C
 999  CONTINUE
      CLOSE(UNIT=2)
C
      END
C
      INCLUDE 'lenstr.f'
