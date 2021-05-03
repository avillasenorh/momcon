c
c $$$$$ calls cross and fgpai $$$$$
c
c   there are two pa routines.  paf converts an angular description (in
c   degrees) of a set of principal axes into the fault geometry
c   coordinate system defined by unit vectors s, n, and b (see
c   subroutine fgf).  the portion of each axis below the (theta,phi)
c   plane is described by an azimuth, zeta, measured clockwise from
c   north in the (theta,phi) plane (0<=zeta<360) and a plunge, eta, the
c   acute angle between the (theta,phi) plane and the axis measured in
c   the plane of the axis and radius (0<=eta<=90).  zeta(i) and eta(i)
c   describe the compression or p axis (i=1), the tension or t axis
c   (i=2), and the null or b axis (i=3).  since the geometry is
c   uniquely defined by the p and t axes, the b axis (zeta(3) and
c   eta(3)) is computed by paf.  the unit vectors s, n, and b define
c   one of the two conjugate fault planes consistent with the principal
c   axes (subroutine cnfg will calculate s, n, and b for the other).
c   pai performs the inverse operation.  programmed on 1 nov 78 by
c   r. buland.
c
      subroutine paf(zeta, eta, s, n, b)
      real zeta(3), eta(3), s(3), n(3), b(3), p(3), t(3)
      data con / .017453293 /
      data cni / 57.295780 /
      data tol / 1e-5 /
      data tol2 / 2e-5 /
# 24 "paf.for"
      ce = cos(eta(1) * con)
      p(1) = sin(eta(1) * con)
      p(2) = cos(zeta(1) * con) * ce
      p(3) = - (sin(zeta(1) * con) * ce)
      ce = cos(eta(2) * con)
      t(1) = sin(eta(2) * con)
      t(2) = cos(zeta(2) * con) * ce
      t(3) = - (sin(zeta(2) * con) * ce)
      call cross(p, t, b)
c   calculate zeta and eta for the b axis.
# 33 "paf.for"
      call fgpai(s, n, p, t)
# 35 "paf.for"
      ce = - sign(1.,b(1))
      zeta(3) = 0.
      if ((abs(b(2)) + abs(b(3))) .gt. tol2) zeta(3) = cni * atan2(ce * 
     &b(3),- (ce * b(2)))
      eta(3) = cni * asin(- (ce * b(1)))
      if (zeta(3) .lt. 0.) zeta(3) = zeta(3) + 360.
      if ((abs(b(1)) .le. tol) .and. (zeta(3) .gt. 180.)) zeta(3) = zeta
     &(3) - 180.
# 42 "paf.for"
      return 
      end
