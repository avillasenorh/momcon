c
c $$$$$ calls cross $$$$$
c
c   there are two fg routines.  fgf converts from a description of
c   fault geometry in terms of the angles strike (sig), dip (del), and
c   slip (gam) to fault geometry unit vectors slip (s), normal (n), and
c   null (b).  angles are in degrees with the following conventions:
c   strike is measured clockwise from north in the (theta,phi) plane
c   such that if one faces in the strike direction the fault plane dips
c   to the right at an acute angle (0<=sig<360).  dip is the acute
c   angle between the (theta,phi) plane and the fault plane measured in
c   the plane normal to strike (0<=dip<=90).  slip is the direction the
c   hanging wall moves relative to the foot wall measured counterclock-
c   wise from the strike in the fault plane (0<=gam<360).  s is the
c   unit vector in the fault plane in the direction of slip.  n is the
c   unit vector normal to the fault plane such that n(1)>=0.  b = n x
c   s.  vectors are in spherical coordinates:  radius up, theta south,
c   and phi east.  fgi performs the inverse transformation.  programmed
c   30 oct 78 by r. buland.
c
      subroutine fgf(sig, del, gam, s, n, b)
      real s(3), n(3), b(3)
      data con / .017453293 /
# 24 "fgf.for"
      ss = - sin(sig * con)
      cs = - cos(sig * con)
      sd = sin(del * con)
      cd = cos(del * con)
      sg = - sin(gam * con)
      cg = - cos(gam * con)
      s(1) = - (sd * sg)
      s(2) = (- (cs * cg)) - ((ss * cd) * sg)
      s(3) = (ss * cg) - ((cs * cd) * sg)
      n(1) = cd
      n(2) = - (ss * sd)
      n(3) = - (cs * sd)
      call cross(n, s, b)
      return 
      end
