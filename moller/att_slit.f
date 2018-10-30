      FUNCTION ATT_SLIT(X)
C
C---      Slit attenuation function
C
      REAL par(5)
      DATA par/0.32209,-0.01468,0.29857,0.21897,-0.09835/
C
      x1=X-15.
      f=par(1)
      x2=1.
      DO i=2,5
        x2=x2*x1
        f=f+par(i)*x2
      ENDDO
      ATT_SLIT=f
      END
