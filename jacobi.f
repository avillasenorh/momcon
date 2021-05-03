c
c $$$$$ calls only library routines $$$$$
c
c   jacobi finds all eigenvalues and eigenvectors of the n by n real
c   symmetric matrix a by jacobi's method.  n must be less than or
c   equal to 50 (determined by the dimensions of b and z below).
c   the unordered eigenvalues are returned in real n vector d and
c   the corresponding eigenvectors are in the respective columns of
c   n by n real matrix v.  the number of jacobi rotations needed are
c   returned in nrot.  taken from linear algebra, volume ii, by
c   wilkinson and reinsch, 1971, springer-verlag.  the algorithm
c   is published (in algol) by h. rutishauser on pages 202-211.
c   converted to fortran by r. buland, 30 oct. 1978.
c
      subroutine jacobi(n, a, d, v, nrot)
      integer p, q, p1, p2, q1, q2
      dimension a(n, n), d(n), v(n, n), b(50), z(50)
# 18 "jacobi.for"
      n1 = n - 1
c   initialize array storage.
# 19 "jacobi.for"
      nn = n * n
# 21 "jacobi.for"
      do 1 p = 1, n
      do 2 q = 1, n
    2 v(p,q) = 0.
      v(p,p) = 1.
      b(p) = a(p,p)
      d(p) = b(p)
    1 z(p) = 0.
c
c   make up to 50 passes rotating each off diagonal element.
c
# 28 "jacobi.for"
      nrot = 0
# 32 "jacobi.for"
      do 3 i = 1, 50
      sm = 0.
      do 4 p = 1, n1
      p1 = p + 1
      do 4 q = p1, n
c   exit if all off diagonal elements have underflowed.
# 37 "jacobi.for"
    4 sm = sm + abs(a(p,q))
# 39 "jacobi.for"
      if (sm .eq. 0.) goto 13
      tresh = 0.
c
c   loop over each off diagonal element.
c
# 41 "jacobi.for"
      if (i .lt. 4) tresh = (.2 * sm) / nn
# 45 "jacobi.for"
      do 5 p = 1, n1
      p1 = p + 1
      p2 = p - 1
      do 5 q = p1, n
      q1 = q + 1
      q2 = q - 1
c   skip this element if it has already underflowed.
# 51 "jacobi.for"
      g = 100. * abs(a(p,q))
# 53 "jacobi.for"
      if (((i .le. 4) .or. ((abs(d(p)) + g) .ne. abs(d(p)))) .or. ((abs(
     &d(q)) + g) .ne. abs(d(q)))) goto 6
      a(p,q) = 0.
      goto 5
c   compute the rotation.
# 57 "jacobi.for"
    6 if (abs(a(p,q)) .le. tresh) goto 5
# 59 "jacobi.for"
      h = d(q) - d(p)
      if ((abs(h) + g) .eq. abs(h)) goto 7
      theta = (.5 * h) / a(p,q)
      t = 1. / (abs(theta) + sqrt(1. + (theta * theta)))
      if (theta .lt. 0.) t = - t
      goto 14
    7 t = a(p,q) / h
   14 c = 1. / sqrt(1. + (t * t))
      s = t * c
c   rotate the diagonal.
# 68 "jacobi.for"
      tau = s / (1. + c)
# 70 "jacobi.for"
      h = t * a(p,q)
      z(p) = z(p) - h
      z(q) = z(q) + h
      d(p) = d(p) - h
      d(q) = d(q) + h
c   rotate the off diagonal.  note that only the upper diagonal
c   elements are touched.  this allows the recovery of matrix a later.
# 75 "jacobi.for"
      a(p,q) = 0.
# 78 "jacobi.for"
      if (p2 .lt. 1) goto 15
      do 8 j = 1, p2
      g = a(j,p)
      h = a(j,q)
      a(j,p) = g - (s * (h + (g * tau)))
    8 a(j,q) = h + (s * (g - (h * tau)))
   15 if (q2 .lt. p1) goto 16
      do 9 j = p1, q2
      g = a(p,j)
      h = a(j,q)
      a(p,j) = g - (s * (h + (g * tau)))
    9 a(j,q) = h + (s * (g - (h * tau)))
   16 if (n .lt. q1) goto 17
      do 10 j = q1, n
      g = a(p,j)
      h = a(q,j)
      a(p,j) = g - (s * (h + (g * tau)))
c   rotate the eigenvector matrix.
# 95 "jacobi.for"
   10 a(q,j) = h + (s * (g - (h * tau)))
# 97 "jacobi.for"
   17 do 11 j = 1, n
      g = v(j,p)
      h = v(j,q)
      v(j,p) = g - (s * (h + (g * tau)))
   11 v(j,q) = h + (s * (g - (h * tau)))
      nrot = nrot + 1
c   reset the temporary storage for the next rotation pass.
# 103 "jacobi.for"
    5 continue
# 105 "jacobi.for"
      do 3 p = 1, n
      d(p) = b(p) + z(p)
      b(p) = d(p)
c
c   all finished.  prepare for exiting by reconstructing the upper
c   triangle of a from the untouched lower triangle.
c
# 108 "jacobi.for"
    3 z(p) = 0.
# 113 "jacobi.for"
   13 do 12 p = 1, n1
      p1 = p + 1
      do 12 q = p1, n
   12 a(p,q) = a(q,p)
      return 
      end
