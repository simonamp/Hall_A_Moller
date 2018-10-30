      FUNCTION ATT_LASER(X)
C
C---      Laser attenuation function
C
C      a=3.1415/2/550*(X-50)
      a=3.1415/2/550*(X+0)
      ATT_LASER=SIN(a)**2
      END
