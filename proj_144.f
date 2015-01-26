c-------------------------------------------------------------------------------------------------c
      program project_euler_144
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  In laser physics, a "white cell" is a mirror system that acts as a delay line for the laser    c
c  beam. The beam enters the cell, bounces around on the mirrors, and eventually works its way    c
c  back out.                                                                                      c
c                                                                                                 c
c  The specific white cell we will be considering is an ellipse with the equation 4x2 + y2 = 100  c
c                                                                                                 c
c  The section corresponding to −0.01 ≤ x ≤ +0.01 at the top is missing, allowing the light to    c
c  enter and exit through the hole.                                                               c
c                                                                                                 c
c  The light beam in this problem starts at the point (0.0,10.1) just outside the white cell,     c
c  and the beam first impacts the mirror at (1.4,-9.6).                                           c
c                                                                                                 c
c  Each time the laser beam hits the surface of the ellipse, it follows the usual law of          c
c  reflection "angle of incidence equals angle of reflection." That is, both the incident and     c
c  reflected beams make the same angle with the normal line at the point of incidence.            c
c                                                                                                 c
c  In the figure on the left, the red line shows the first two points of contact between the      c
c  laser beam and the wall of the white cell; the blue line shows the line tangent to the         c
c  ellipse at the point of incidence of the first bounce.                                         c
c                                                                                                 c
c  The slope m of the tangent line at any point (x,y) of the given ellipse is: m = −4x/y          c
c                                                                                                 c
c  The normal line is perpendicular to this tangent line at the point of incidence.               c
c                                                                                                 c
c  The animation on the right shows the first 10 reflections of the beam.                         c
c                                                                                                 c
c  How many times does the beam hit the internal surface of the white cell before exiting?        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
  
c parameters used in this program only
      integer nhits
      double precision old_hit(2), new_hit(2), v1(2), v1_len, v2(2), v2_len, v3p(2), v3p_len
      double precision v3m(2), v3m_len, v1_hat(2), v2_hat(2), v3p_hat(2), v3m_hat(2), v1hat_v2hat
      double precision m, a, b, c, xp, xm, yp, ym
      logical done

c initialize parameters
      done = .false.
      old_hit(1) = 0.d0
      old_hit(2) = 10.1d0
      new_hit(1) = 1.4d0
      new_hit(2) = -9.6d0
      nhits = 0
      write(*,*) 0,old_hit(1), old_hit(2)
      write(*,*) 1,new_hit(1), new_hit(2)

      do while(.not.done)
        nhits = nhits + 1

c vector 1 is the vector from the old hit to the new hit        
        v1(1) = new_hit(1) - old_hit(1)
        v1(2) = new_hit(2) - old_hit(2)
        v1_len = dsqrt(v1(1)**2 + v1(2)**2)
        v1_hat(1) = v1(1)/v1_len
        v1_hat(2) = v1(2)/v1_len

c vector 2 is the normal to the ellipse at the new hit        
        v2(1) = 4.d0*new_hit(1)
        v2(2) = new_hit(2)
        v2_len = dsqrt(v2(1)**2 + v2(2)**2)
        v2_hat(1) = v2(1)/v2_len
        v2_hat(2) = v2(2)/v2_len

c there are two possible solutions for the reflected beam        
        v1hat_v2hat = v1_hat(1)*v2_hat(1) + v1_hat(2)*v2_hat(2)
        v3p(1) = v1hat_v2hat*v2_hat(1) + dabs(v2_hat(2))*dsqrt(1.d0-v1hat_v2hat**2)
        v3p(2) = (v1hat_v2hat-v2_hat(1)*v3p(1))/v2_hat(2)
        v3p_len = dsqrt(v3p(1)**2 + v3p(2)**2)
        v3p_hat(1) = v3p(1)/v3p_len
        v3p_hat(2) = v3p(2)/v3p_len
        
        v3m(1) = v1hat_v2hat*v2_hat(1) - dabs(v2_hat(2))*dsqrt(1.d0-v1hat_v2hat**2)        
        v3m(2) = (v1hat_v2hat-v2_hat(1)*v3m(1))/v2_hat(2)
        v3m_len = dsqrt(v3m(1)**2 + v3m(2)**2)
        v3m_hat(1) = v3m(1)/v3m_len
        v3m_hat(2) = v3m(2)/v3m_len
        
c one of these solutions matches v1_hat so we want the other one
        if (dabs(v1_hat(1)-v3p_hat(1)).lt.1.d-5) then
          m = v3m_hat(2)/v3m_hat(1)
        endif
        if (dabs(v1_hat(1)-v3m_hat(1)).lt.1.d-5) then
          m = v3p_hat(2)/v3p_hat(1)
        endif

c transfer the new_hit to old_hit
        old_hit(1) = new_hit(1)
        old_hit(2) = new_hit(2)
        
c find where the reflected beam intersects with the ellipse
        a = 4 + m**2
        b = 2.d0*m*(old_hit(2)-m*old_hit(1))
        c = (old_hit(2)-m*old_hit(1))**2 - 100.d0
        
        xp = (-b + dsqrt(b**2 - 4.d0*a*c))/(2.d0*a)
        yp = m*(xp-old_hit(1)) + old_hit(2)
        xm = (-b - dsqrt(b**2 - 4.d0*a*c))/(2.d0*a)
        ym = m*(xm-old_hit(1)) + old_hit(2)

c one of these solutions is the old hit which we don't want
        if (dabs(xp-old_hit(1)).lt.1.d-5) then
          new_hit(1) = xm
          new_hit(2) = ym
        endif
        if (dabs(xm-old_hit(1)).lt.1.d-5) then
          new_hit(1) = xp
          new_hit(2) = yp
        endif
        
        write(*,*) nhits+1,new_hit(1), new_hit(2)
c        read(*,*)
        
        if ((dabs(new_hit(1)).lt.0.01).and.(new_hit(2).gt.0.d0)) then
          done = .true.
        endif
      enddo
      
      write(*,*) 'Number of hits before exiting the ellispe ',nhits
      end