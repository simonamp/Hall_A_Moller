      REAL FUNCTION CAL_ENFUN(X)
      COMMON/PAWPAR/ PAR(10)
C
C---      Calorimeter tests - energy function(hodoscope channel)
C
      CAL_ENFUN=26.8/(26.8-(X-PAR(1))*0.5)
      END
