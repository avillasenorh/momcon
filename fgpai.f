c
c $$$$$ calls no other routine $$$$$
c
      subroutine fgpai(s, n, p, t)
      real s(3), n(3), p(3), t(3)
      data con / .70710678 /
# 7 "fgpai.for"
      cn = con
      if ((t(1) + p(1)) .lt. 0.) cn = - cn
      do 2 k = 1, 3
      n(k) = cn * (t(k) + p(k))
    2 s(k) = cn * (t(k) - p(k))
      return 
      end
