c
c $$$$$ calls no other routine $$$$$
c
c   there are two fgpa routines.  fgpaf converts the unit vectors
c   defining the fault geometry coordinate system (see subroutine fgf)
c   to the unit vectors defining the principal axis coordinate system:
c   p, t, and b.  these unit vectors are colinear with the p, t, and b
c   axes such that p(1)>=0 and t = b x p (where b = n x s).  b is not
c   needed as it is common to both systems.  fgpai performs the inverse
c   transformation.  programmed on 30 oct 78 by r. buland.
c
      subroutine fgpaf(s, n, p, t)
      real s(3), n(3), p(3), t(3)
      data con / .70710678 /
# 15 "fgpaf.for"
      cn = con
      if ((n(1) - s(1)) .lt. 0.) cn = - cn
      do 1 k = 1, 3
      p(k) = cn * (n(k) - s(k))
    1 t(k) = cn * (n(k) + s(k))
      return 
      end
