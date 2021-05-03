c
c $$$$$ calls no other routines $$$$$
c
c     revised version of pai.  given fault geometry as s,n,b, fgpl
c     returns the azimuth and plunge of the poles of the fault planes
c     in zeta and eta.
c
      subroutine fgpl(zeta, eta, s, n, b)
      real zeta(3), eta(3), s(3), n(3), b(3)
      data cni / 57.295780 /
      data tol / 1e-5 /
      data tol2 / 2e-5 /
# 11 "fgpl.for"
      do 2 k = 1, 3
    2 zeta(k) = 0.
      ce = - sign(1.,s(1))
      if ((abs(s(2)) + abs(s(3))) .gt. tol2) zeta(1) = cni * atan2(ce * 
     &s(3),- (ce * s(2)))
      eta(1) = cni * asin(- (ce * s(1)))
      ce = - sign(1.,n(1))
      if ((abs(n(2)) + abs(n(3))) .gt. tol2) zeta(2) = cni * atan2(ce * 
     &n(3),- (ce * n(2)))
      eta(2) = cni * asin(- (ce * n(1)))
      ce = - sign(1.,b(1))
      if ((abs(b(2)) + abs(b(3))) .gt. tol2) zeta(3) = cni * atan2(ce * 
     &b(3),- (ce * b(2)))
      eta(3) = cni * asin(- (ce * b(1)))
      do 1 k = 1, 3
      if (zeta(k) .lt. 0.) zeta(k) = zeta(k) + 360.
c   put zeta into canonical form if eta is nearly zero.
# 27 "fgpl.for"
    1 continue
# 29 "fgpl.for"
      if ((abs(s(1)) .le. tol) .and. (zeta(1) .gt. 180.)) zeta(1) = zeta
     &(1) - 180.
# 30 "fgpl.for"
      if ((abs(n(1)) .le. tol) .and. (zeta(2) .gt. 180.)) zeta(2) = zeta
     &(2) - 180.
# 31 "fgpl.for"
      if ((abs(b(1)) .le. tol) .and. (zeta(3) .gt. 180.)) zeta(3) = zeta
     &(3) - 180.
# 32 "fgpl.for"
      return 
      end
