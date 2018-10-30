      real function func_a(x)
c
      real x
      real alpha
      real p
      real theta
      integer n_a
      integer n_b
c
      n_a= 2
      n_b = 3
      alpha = .1125
      p = (418.31/.511)*(1.00116-1)
      theta = p*(2.0*n_a*n_a - n_a*(1.-2*alpha-1./2.4)
     $ - alpha*(1.-1./4.8))*3.1415
      func_a = cos(theta+x*3.1415/180.)
c
      end
