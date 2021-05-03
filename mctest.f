      program mctest
      parameter(rdeg=0.017453293)

      real u(3),v(3),b(3)
      real z(3),e(3)

   10 read (*,*,end=8888) strike,dip,rake
      call fgf(strike,dip,rake,u,v,b)

      write (*,1003) v(1),v(2),v(3)

      call fgpl(z,e,u,v,b)

c     write (*,1000) z(1),z(2),z(3)
c     write (*,1000) e(1),e(2),e(3)

      write (*,1001) z(2),e(2)
      write (*,1002) z(1),e(1)

      cve=sin(rdeg*e(2))
      cns=cos(rdeg*z(2))*cos(rdeg*e(2))
      cew=-sin(rdeg*z(2))*cos(rdeg*e(2))

      write(*,1003) cve,cns,cew
      

      go to 10

 1000 format(3(1x,f5.1))
 1001 format('Fault pole (trend and plunge)....:',2(1x,f5.1))
 1002 format('Slip vector (trend and plunge)...:',2(1x,f5.1))
 1003 format(3(1x,f8.5))

 8888 stop
      end
