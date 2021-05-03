c
c $$$$$ calls no other routine $$$$$
c
c   there are two fgmrt routines.  fgmrtf converts the unit vectors s
c   and n (see subroutine fgf) and the moment m0 to a moment tensor m.
c   fgmrti performs the inverse operation.  programmed on 30 oct 78 by
c   r. buland.
c
      subroutine fgmrtf(s, n, b, m0, m)
      real s(3), n(3), b(3), m0, m(3, 3)
# 11 "fgmrtf.for"
      do 1 k = 1, 3
      do 1 j = 1, 3
    1 m(j,k) = m0 * ((s(k) * n(j)) + (s(j) * n(k)))
      return 
      end
