      program shmaxreg


c     saz(i): Si-axis azimuth (i=1,2,3)
c     spl(i): Si-axis plunge (i=1,2,3)

      parameter(ncrit=6)

      character*256 mecafile
      character*256 shfile

      character*256 line
      character*256 comment

c sv : slip vector in cartesian coordinates
c nv : vector normal to fault plane, in cartesian coordinates
c bv : bv = sv x nv (cross product). Also B = P x T (?)

c zeta : array with azimuth (trend) of P, T, and B axes
c eta  : array with plunge of P, T, and B axes

c saz : array with azimuth (trend) of P/S1, B/S2, and T/S3 axis
c spl : array with plunge of P/S1, B/S2, and T/S3 axis

      real*4 sv(3),nv(3),bv(3)
      real*4 zeta(3),eta(3)
      real*4 saz(3),spl(3)
      logical lcrit(ncrit)
      character*2 regime
      character*1 quality

      logical lex

      write(*,1003) 'Input meca file:'
      read(*,1000) mecafile
      inquire(file=mecafile,exist=lex)
      if (.not.lex) then
         stop 1
      endif
      write(*,1003) 'Onput SHMAX file:'
      read(*,1000) shfile

      open(1,file=mecafile,status='old',form='formatted',
     & access='sequential')
      open(2,file=shfile,status='unknown',form='formatted',
     & access='sequential')
      open(3,file='ptb.out',status='unknown',form='formatted',
     & access='sequential')

      iline=1
      nu=0
   10 read(1,1000,end=100) line
      iline=iline+1

      do i=1,ncrit
         lcrit(i)=.false.
      enddo

      read(line,*,iostat=ios) rlon1,rlat1,depth,strike,dip,rake,rmag,
     & rlon2,rlat2,comment

      if (ios.ne.0) then
         write(0,1001) 'Error reading line',iline
         write(0,1000) line(1:lnblnk(line))
         go to 10
      endif

      call fgf(strike,dip,rake,sv,nv,bv)
      call pai(zeta,eta,sv,nv,bv)

c zeta, eta contain P(index=1), T(index=2) and B(index=3)
c saz,  spl contain P(index=1), B(index=2) and T(index=3)
      saz(1)=zeta(1)
      spl(1)=eta(1)
      saz(2)=zeta(3)
      spl(2)=eta(3)
      saz(3)=zeta(2)
      spl(3)=eta(2)

c write out azimuth and plunge of P-T-B axis (in this order!)
      write(3,4000) rlon1,rlat1,depth,nint(strike),nint(dip),nint(rake),
     & rmag,nint(zeta(1)),nint(eta(1)),nint(zeta(2)),nint(eta(2)),
     & nint(zeta(3)),nint(eta(3))

c     P/S1-axis            B/S2-axis       T/S3-axis      Regime  SH-azimuth
c
c 52 < plunge                               plunge < 35     NF    B-axis
c 40 < plunge < 52                          plunge < 20     NS    T-axis + 90
c      plunge < 40       45 < plunge        plunge < 20     SS    T-axis + 90
c      plunge < 20       45 < plunge        plunge < 40     SS    P-axis
c      plunge < 20                     40 < plunge < 52     TS    P-axis
c      plunge < 35                          plunge < 52     TF    P-axis

      shaz=999.
      regime='U '
      quality='E'
      if (spl(1).gt.52.) then
         if (spl(3).le.35.) then
            shaz=saz(2)
            regime='NF'
            lcrit(1)=.true.
         endif
      elseif (spl(1).gt.40. .and. spl(1).le.52.) then
         if (spl(3).le.20.) then
            shaz=saz(3)+90.
            regime='NS'
            lcrit(2)=.true.
         endif
      elseif (spl(1).le.40.) then

c        if (spl(2).ge.45. .and. spl(3).le.20.) then
c           shaz=saz(3)+90.
c           regime='SS'
c           lcrit(3)=.true.
c        endif
c        if (spl(1).le.20. .and. spl(2).ge.45. .and. spl(3).le.40.) then
c           shaz=saz(1)
c           regime='SS'
c           lcrit(4)=.true.
c        endif

c Some mechanisms with stress regime = 'SS" can meet both criteria.
c Therefore there is an ambiguity in the way SH-az is calculated.
c If plunge(P) < 20 and plunge(T) < 20 then we use the axis with the
c lowest plunge (shallower axis) to calculate SH-az.
         if (spl(2).ge.45. .and. spl(3).le.40.) then
            if (spl(1).le.20. .and. spl(3).le.20.) then
               if (spl(1).lt.spl(3)) then
                  shaz=saz(1)
                  regime='SS'
                  lcrit(4)=.true.
               else
                  shaz=saz(3)+90.
                  regime='SS'
                  lcrit(3)=.true.
               endif
            elseif (spl(1).le.20. .and. spl(3).gt.20.) then
               shaz=saz(1)
               regime='SS'
               lcrit(4)=.true.
            elseif (spl(1).gt.20. .and. spl(3).le.20.) then
               shaz=saz(3)+90.
               regime='SS'
               lcrit(3)=.true.
            endif


         endif

         if (spl(1).lt.20. .and. spl(3).gt.40. .and. spl(3).lt.40.) then
            shaz=saz(1)
            regime='TS'
            lcrit(5)=.true.
         endif
         if (spl(1).le.35. .and. spl(3).ge.52.) then
            shaz=saz(1)
            regime='TF'
            lcrit(6)=.true.
         endif
      endif

      nn=0
      do i=1,ncrit
         if (lcrit(i)) nn=nn+1
      enddo

      if (nn.eq.0) then
         nu=nu+1
         write(0,1000) 'WARNING: unknown stress regime for this event:'
         write(0,1000) line(1:lnblnk(line))
         write(0,1010) saz(1),spl(1),saz(2),spl(2),saz(3),spl(3)
      else
         if (nn.gt.1) then
         write(0,1000) 'WARNING: event meets more than one criterium:'
         write(0,1000) line(1:lnblnk(line))
         write(0,1010) saz(1),spl(1),saz(2),spl(2),saz(3),spl(3)
         write(0,*) (lcrit(i),i=1,ncrit)
         endif

         quality='C'

c shaz must be between 0 and 180

         if (shaz.ge.360.) shaz=shaz-360.

         if (shaz.gt.180.) shaz=shaz-180.

      endif
      ishaz=nint(shaz)
      if (ishaz.eq.180) ishaz=0
      write(2,2000) rlon1,rlat1,ishaz,'FMS',quality,regime,depth,
     & comment(1:lnblnk(comment))

      go to 10

  100 write(0,1001) 'Number of mechanisms read in:',iline
      write(0,1001) 'Mechanisms with unknown stress regime:',nu

      close(1)
      close(3)

 1000 format(a)
 1001 format(a,1x,i6)
 1003 format(a,1x,$)
 1010 format(3(4x,f5.1,1x,f4.1))
 2000 format(f8.3,1x,f7.3,1x,i3,1x,a3,1x,a1,1x,a2,1x,f5.1,1x,a)
 4000 format(f8.2,f7.2,f6.1,i4,i3,i5,f4.1,3(3x,i3,1x,i2))

      stop
      end
