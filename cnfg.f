c
c $$$$$ calls fgpaf and fgpai $$$$$
c
c   cnfg uses unit vectors s, n, and b (see subroutine fgf) defining a
c   unique fault geometry to compute s, n, and b for the conjugate
c   fault geometry.  programmed on 30 oct 78 by r. buland.
c
      subroutine cnfg(s, n, b)
      real p(3), t(3), b(3), s(3), n(3)
# 10 "cnfg.for"
      call fgpaf(s, n, p, t)
      do 1 k = 1, 3
      t(k) = - t(k)
    1 b(k) = - b(k)
      call fgpai(s, n, p, t)
      return 
      end
