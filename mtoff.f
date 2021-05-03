c
c $$$$$ calls no other routine $$$$$
c
c   there are two mtof routines.  mtoff converts moment tensor m into
c   six vector representation g .  mtofi converts g into m.  programmed
c   on 31 oct 78 by r. buland.
c
      subroutine mtoff(m, g)
      real m(3, 3), g(6)
# 10 "mtoff.for"
      k = 0
      do 1 j = 1, 3
      do 1 l = 1, j
      k = k + 1
    1 g(k) = m(l,j)
      return 
      end
