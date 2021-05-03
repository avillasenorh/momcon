c
c $$$$$ calls no other routine $$$$$
c
c   cross computes the vector product c = a x b, where a, b, and c are
c   all three vectors.  programmed on 30 oct 78 by r. buland.
c
      subroutine cross(a, b, c)
      dimension a(3), b(3), c(3)
# 9 "cross.for"
      c(1) = (a(2) * b(3)) - (b(2) * a(3))
      c(2) = (a(3) * b(1)) - (b(3) * a(1))
      c(3) = (a(1) * b(2)) - (b(1) * a(2))
      return 
      end
