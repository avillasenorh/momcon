c
c $$$$$ calls only system routines $$$$$
c
      subroutine fgi(sig, del, gam, s, n, b)
      real s(3), n(3), b(3)
      data cni / 57.295780 /
# 7 "fgi.for"
      del = acos(n(1))
      sig = atan2(n(2),n(3))
      gam = cni * atan2(s(1) / sin(del),(s(3) * sin(sig)) - (s(2) * cos(
     &sig)))
# 10 "fgi.for"
      del = cni * del
      sig = cni * sig
      if (sig .lt. 0.) sig = sig + 360.
      if (gam .lt. 0.) gam = gam + 360.
      return 
      end
