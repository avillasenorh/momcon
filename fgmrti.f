c
c $$$$$ calls cross, fgpaf, fgpai, and jacobi $$$$$
c
      subroutine fgmrti(s, n, b, m0, m)
      real s(3), n(3), b(3), m0, m(3, 3), p(3), t(3), ev(3), vec(3, 3)
c   decompose the moment tensor.
# 6 "fgmrti.for"
      call fgpaf(s, n, p, t)
c   find the maximum and minimum eigenvalues.
# 8 "fgmrti.for"
      call jacobi(3, m, ev, vec, nrot)
# 10 "fgmrti.for"
      emax = ev(1)
      emin = emax
      np = 1
      nt = 1
      do 2 k = 2, 3
      if (ev(k) .ge. emin) goto 3
      emin = ev(k)
      np = k
    3 if (ev(k) .le. emax) goto 2
      emax = ev(k)
      nt = k
c   compute s and n.
# 21 "fgmrti.for"
    2 continue
# 23 "fgmrti.for"
      sp = sign(1.,vec(1,np))
      st = sign(1.,vec(1,nt))
      do 4 k = 1, 3
      p(k) = sp * vec(k,np)
    4 t(k) = st * vec(k,nt)
      call cross(p, t, b)
      m0 = .5 * (ev(nt) - ev(np))
      call fgpai(s, n, p, t)
      return 
      end
