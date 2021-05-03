      program meca2perc


      parameter(rdeg=0.017453293)

      character*256 mecafile
      character*256 percfile

      character*256 line
      character*256 comment
      character*1 ifmt

c zeta : array with azimuth (trend) of P, T, and B axes
c eta  : array with plunge of P, T, and B axes

      real*4 sv(3),nv(3),bv(3)
      real*4 zeta(3),eta(3)

      logical lex

      write(*,1003) 'Input meca file:'
      read(*,1000) mecafile
      inquire(file=mecafile,exist=lex)
      if (.not.lex) then
         stop 1
      endif
      write(*,1003) 'Output percentage file:'
      read(*,1000) percfile
      write(*,1003) 'Output meca format (a or c):'
      read(*,1000) ifmt

      if (ifmt.ne.'a' .and. ifmt.ne.'A' .and.
     &    ifmt.ne.'c' .and. ifmt.ne.'C') then
          write(0,'(2a)') 'ERROR: invalid meca format: ',ifmt
          stop 1
      endif

      open(1,file=mecafile,status='old',form='formatted',
     & access='sequential')
      open(2,file=percfile,status='unknown',form='formatted',
     & access='sequential')

      iline=0
   10 read(1,1000,end=100) line
      iline=iline+1

      if (ifmt.eq.'a' .or. ifmt.eq.'A') then
      read(line,*,iostat=ios) rlon1,rlat1,depth,strike1,dip1,rake1,
     & rmag,rlon2,rlat2,comment
      elseif (ifmt.eq.'c' .or. ifmt.eq.'C') then
      read(line,*,iostat=ios) rlon1,rlat1,depth,strike1,dip1,rake1,
     & strike2,dip2,rake2,smo,expo,rlon2,rlat2,comment
      endif

      if (ios.ne.0) then
         write(0,1001) 'Error reading line',iline
         write(0,1000) line(1:lnblnk(line))
         go to 10
      endif

      call fgf(strike1,dip1,rake1,sv,nv,bv)
      call pai(zeta,eta,sv,nv,bv)

c     calculate percentage (fraction) of thrust, strike-slip and
c     normal

      fth = (sin(eta(2)*rdeg))**2
      fss = (sin(eta(3)*rdeg))**2
      fno = (sin(eta(1)*rdeg))**2

      ftest=abs(1.-(fth+fss+fno))

      if (ftest.gt.0.005) then
         write(0,1000) 'WARNING: sum of fractions not equal to 1:'
         write(0,1000) line(1:lnblnk(line))
         write(0,'(4(1x,f6.4))') ftest,fth,tss,fno
      endif

      if (ifmt.eq.'a' .or. ifmt.eq.'A') then
         write(2,1011) rlon1,rlat1,depth,
     &    nint(strike1),nint(dip1),nint(rake1),
     &    rmag,rlon2,rlat2,comment(1:8),fth,fss,fno
      elseif (ifmt.eq.'c' .or. ifmt.eq.'C') then
         write(2,1012) rlon1,rlat1,depth,
     &    nint(strike1),nint(dip1),nint(rake1),
     &    nint(strike2),nint(dip2),nint(rake2),
     &    smo,nint(expo),rlon2,rlat2,comment(1:8),fth,fss,fno
      endif

      go to 10

  100 write(0,1001) 'Number of mechanisms read in:',iline

      close(1)
      close(2)

 1000 format(a)
 1001 format(a,1x,i6)
 1003 format(a,1x,$)
 1010 format(f8.2,f7.2,f6.1,i4,i3,i5,f4.1,f8.2,f7.2,1x,a)
 1011 format(f8.2,f7.2,f6.1,i4,i3,i5,f4.1,f8.2,f7.2,1x,a8,
     & 3(1x,f4.2))
 1012 format(f8.2,f7.2,f6.1,2(i4,i3,i5),f7.2,1x,i2,f8.2,f7.2,1x,a8,
     & 3(1x,f4.2))

      stop
      end
