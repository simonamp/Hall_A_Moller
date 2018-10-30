*
* $Id: rfstft.F,v 1.1 1996/04/17 12:32:04 mclareni Exp $
*
* $Log: rfstft.F,v $
* Revision 1.1  1996/04/17 12:32:04  mclareni
* Add d/rfstft.F (D705) and to Imakefile. cfstft.F becomes D706.
* In tests, add d705m.F for rfstft and d706m.F for cfstft and the corresponding
* additions to main.F and Imakefile.
*
*
      SUBROUTINE DRFSTFT(MS,A)
 
      DOUBLE COMPLEX A(0:*),T,T1,T2,U,W
      DOUBLE PRECISION F,PHI

      PARAMETER (PI = 3.14159 26535 89793D0)
 
      IF(MS .EQ. 0) THEN
       A(0)=DBLE(A(0))
       RETURN
      ENDIF
      M=ABS(MS)-1
      N=2**M
      U=(0.D0,1.D0)
      IF(MS .LT. 0) THEN
       CALL DCFSTFT(-M,A)
       F=0.25D0/N
       DO 1 I = 0,N-1
    1  A(I)=F*A(I)
       A(N)=A(0)
       U=DCONJG(U)
      ENDIF
    2 PHI=PI/SIGN(N,MS)
      W=DCMPLX(DCOS(PHI),DSIN(PHI))
      DO 3 J = 0,N/2
      T=DCONJG(A(N-J))
      T1=A(J)+T
      T2=(A(J)-T)*U
      A(J)=T1+T2
      A(N-J)=DCONJG(T1-T2)
    3 U=U*W
      IF(MS .GT. 0) CALL DCFSTFT(M,A)
      RETURN
      END
