      real function func_b(x)
c
      real x
      real alpha
      real p
      real theta
      integer n_a
      integer n_b
c
      n_a= 4
      n_b = 3
      alpha = .1125
      p = (400./.511)*(1.00116-1)
      theta = p*(2.0*n_b*n_b - n_b*(1.-2*alpha)
     $ - alpha)*3.1415
      func_b = cos(theta+x*3.1415/180.)
c
      end
