c
c $$$$$ calls fgpaf $$$$$
c
      subroutine pai(zeta, eta, s, n, b)
      real zeta(3), eta(3), s(3), n(3), b(3), p(3), t(3)
      data cni / 57.295780 /
      data tol / 1e-5 /
      data tol2 / 2e-5 /
# 7 "pai.for"
      call fgpaf(s, n, p, t)
      do 2 k = 1, 3
    2 zeta(k) = 0.
      ce = - sign(1.,p(1))
      if ((abs(p(2)) + abs(p(3))) .gt. tol2) zeta(1) = cni * atan2(ce * 
     &p(3),- (ce * p(2)))
      eta(1) = cni * asin(- amin1(amax1(ce * p(1),-1.),1.))
      ce = - sign(1.,t(1))
      if ((abs(t(2)) + abs(t(3))) .gt. tol2) zeta(2) = cni * atan2(ce * 
     &t(3),- (ce * t(2)))
      eta(2) = cni * asin(- amin1(amax1(ce * t(1),-1.),1.))
      ce = - sign(1.,b(1))
      if ((abs(b(2)) + abs(b(3))) .gt. tol2) zeta(3) = cni * atan2(ce * 
     &b(3),- (ce * b(2)))
      eta(3) = cni * asin(- amin1(amax1(ce * b(1),-1.),1.))
      do 1 k = 1, 3
      if (zeta(k) .lt. 0.) zeta(k) = zeta(k) + 360.
c   put zeta into canonical form if eta is nearly zero.
# 24 "pai.for"
    1 continue
# 26 "pai.for"
      if ((abs(p(1)) .le. tol) .and. (zeta(1) .gt. 180.)) zeta(1) = zeta
     &(1) - 180.
# 27 "pai.for"
      if ((abs(t(1)) .le. tol) .and. (zeta(2) .gt. 180.)) zeta(2) = zeta
     &(2) - 180.
# 28 "pai.for"
      if ((abs(b(1)) .le. tol) .and. (zeta(3) .gt. 180.)) zeta(3) = zeta
     &(3) - 180.
# 29 "pai.for"
      return 
      end
