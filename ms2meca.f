      program ms2meca

c 101   25 Jul 1962   18.90  -81.19   0   261   10    171    0  A  LL-SS
c 102   23 Mar 1966   16.82  -85.90   9   250   10    160    0  C  LL-SS
c 113   31 Aug 1961   18.49  -66.42 131    --   --     --   --  C  N
c 118    4 Oct 1957   10.92  -62.81  32    --   --    174   16  C  Hinge
c 122   18 Feb 1962    8.12  -74.84  12    --   --     --   --  C  N

      parameter(rdeg=0.017453293)

      character*256 line
      character*3 mon
      character*1 q
      character*16 type
      character*24 comment
      logical lmeca

      real slv(3),nov(3),nuv(3)

c     ifmt output meca format
c     1 = map (lat,lon) output
c     2 = fppage (multiple mechanism in table format)
      
      ifmt=1

      xwidth=6.5
      ywidth=9.0
      nx=4
      ny=6
      diam=1.
      dx=xwidth/float(nx) - diam
      dy=ywidth/float(ny) - diam

c     open(1,file='molnar_sykes69.txt',status='old',form='formatted')

      i=0
      ix=1
      iy=ny
   10 read(*,1000,end=20) line
      lmeca=.false.
      if (line(42:43).ne.'--') then

         read(line,2001,err=888,iostat=ios) nev,iday,mon,iyr,rlat,rlon,
     &   depth,slipaz,slippl,faulaz,faulpl,q,type
         lmeca=.true.


c azimuth of the (fault) plane is equal to the azimuth of the pole 
c plus 90 degrees (this way plane always dips to the right when
c looking in the direction of the azimuth)
         strike1=faulaz+90.
         if (strike1.ge.360.) strike1=strike1-360.
c dip of the (fault) plane is easily related to the plunge of the pole
         dip1=90.-faulpl
c slip vector must be in fault plane

c special case: horizontal plane

         if (faulpl.eq.90.) then
            faulpl=slippl
            faulaz=slipaz+180.
            if (faulaz.ge.360.) faulaz=faulaz-360.
            slippl=90.
            slipaz=0.
         endif

c build vectors s (slv), and n (nov). b not required

         slv(1)=sin(slippl*rdeg)
         slv(2)=cos(slipaz*rdeg)*cos(slippl*rdeg)
         slv(3)=-sin(slipaz*rdeg)*cos(slippl*rdeg)
         nov(1)=sin(faulpl*rdeg)
         nov(2)=cos(faulaz*rdeg)*cos(faulpl*rdeg)
         nov(3)=-sin(faulaz*rdeg)*cos(faulpl*rdeg)

         orto=slv(1)*nov(1)+slv(2)*nov(2)+slv(3)*nov(3)
         if (abs(orto).gt.0.1) then
            write(0,5500) nev,orto
         endif

         call fgi(strike,dip,rake,slv,nov,nuv)

c check rake according to type

         if (type(1:1).eq.'N') then
            if (rake.lt.180.) rake=rake-180.
         elseif (type(1:1).eq.'T') then
            continue
         elseif (type(1:5).eq.'LL-SS') then
            if (rake.gt.90.) rake=rake-180.
         elseif (type(1:5).eq.'RL-SS') then
            if (rake.lt.90.) rake=rake-180.
         endif

c errors and roundoff

         if (strike.gt.360. .or. strike.lt.0. .or.
     &       dip.lt.0. .or. dip.gt.90. .or.
     &       rake.gt.180. .or. rake.lt.-180.) then
             write(0,*) 'ERROR: invalid fault plane solution:',
     &       strike,dip,rake
         endif

         if (strike.ge.359.9) strike=0.
         if (rake.le.-179.9) rake=180.

      else

         read(line,2002,err=888,iostat=ios) nev,iday,mon,iyr,rlat,rlon,
     &   depth,q,type

      endif

      rmag=5.
      write(comment,2010) nev

      if (ifmt.eq.1) then
         xlon=rlon
         ylat=rlat
      elseif (ifmt.eq.2) then
         xlon=(float(ix)-0.5)*(dx+diam)
         ylat=(float(iy)-0.5)*(dy+diam)
         ix=ix+1
         if (ix.gt.nx) then
            ix=1
            iy=iy-1
            if (iy.lt.1) iy=ny
         endif
      endif

      if (lmeca) then
         write(*,2003) xlon,ylat,depth,nint(strike),nint(dip),
     &   nint(rake),rmag,rlon,rlat,nev
      endif

      i=i+1
      go to 10

   20 continue
      write(0,1001) 'Number of lines read in:',i
c     close(1)


 1000 format(a)
 1001 format(a,1x,i3)
 2001 format(i3,3x,i2,1x,a3,1x,i4,2x,f6.2,1x,f7.2,1x,f3.0,3x,
     & f3.0,3x,f2.0,4x,f3.0,3x,f2.0,2x,a1,2x,a)
 2002 format(i3,3x,i2,1x,a3,1x,i4,2x,f6.2,1x,f7.2,1x,f3.0,25x,a1,2x,a)
 2003 format(f8.3,1x,f7.3,1x,f5.1,3x,i4,1x,i2,1x,i4,3x,f3.1,1x,
     & f8.3,1x,f7.3,1x,i3)

 2010 format(i3)

 5500    format('WARNING: S and N not ortogonal for event no.',
     &   1x,i3,'. Scalar product:',1x,f8.5)

      go to 8888

  888 write(0,1000) 'ERROR: wrong format in line:'
      write(0,1000) line(1:lnblnk(line))
      go to 8888

 8888 stop
      end
