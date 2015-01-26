c-------------------------------------------------------------------------------------------------c
      program project_euler_317
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A firecracker explodes at a height of 100 m above level ground. It breaks into a large number  c
c  of very small fragments, which move in every direction; all of them have the same initial      c
c  velocity of 20 m/s.                                                                            c
c                                                                                                 c
c  We assume that the fragments move without air resistance, in a uniform gravitational field     c
c  with g=9.81 m/s2.                                                                              c
c                                                                                                 c
c  Find the volume (in m3) of the region through which the fragments move before reaching the     c
c  ground. Give your answer rounded to four decimal places.                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      double precision h, h_max, g, v,pi, x_max, volume, th_min, th_max, th_mid, x_mid, vol_old, slope 
      logical done
      
c initializing some paramters
      h = 100.d0
      g = 9.81d0
      v = 20.d0
      h_max = h + v**2/(2.d0*g)
      pi = 4.d0*datan(1.d0)
      th_min = 0.d0
      th_max = pi
      volume = 0.d0
      done = .false.

c looping until we find the max x_value with respect to theta
      do while(.not. done)
        vol_old = volume
        
        th_mid = (th_min + th_max)/2.d0
        x_mid = v**2.d0*dsin(2.d0*th_mid)/(2.d0*g) + v*dsin(th_mid)/g*dsqrt(v**2*dcos(th_mid)**2+2.d0*g*h)
        volume = pi/2.d0*x_mid**2*h_max
        
        slope = dcos(2.d0*th_mid)*dsqrt(dcos(th_mid)**2+2.d0*g*h/v**2) + dcos(th_mid)*(dcos(2.d0*th_mid)+2.d0*g*h/v**2)
        if (slope.gt.0.d0) then
          th_min = th_mid
        else if (slope.lt.0.d0) then
          th_max = th_mid
        endif
        
        write(*,*) th_mid*180.d0/pi, x_mid, volume
        
        if (dabs(volume-vol_old)/volume .lt. 1.d-12) done = .true.
      enddo
      
      end