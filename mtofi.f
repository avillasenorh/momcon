c
c $$$$$ calls no other routine $$$$$
c
      subroutine mtofi(m, g)
      real m(3, 3), g(6)
# 6 "mtofi.for"
      k = 0
      do 2 j = 1, 3
      do 2 l = 1, j
      k = k + 1
      m(l,j) = g(k)
    2 m(j,l) = g(k)
      return 
      end
