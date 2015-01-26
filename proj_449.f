c-------------------------------------------------------------------------------------------------c
      program project_euler_449
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Phil the confectioner is making a new batch of chocolate covered candy. Each candy centre is   c
c  shaped like an ellipsoid of revolution defined by the equation:                                c
c  b^2x^2 + b^2y^2 + a^2z^2 = a^2b^2.                                                             c
c                                                                                                 c
c  Phil wants to know how much chocolate is needed to cover one candy centre with a uniform coat  c
c  of chocolate one millimeter thick.                                                             c
c  If a=1 mm and b=1 mm, the amount of chocolate required is 28/3 π mm^3                          c
c                                                                                                 c
c  If a=2 mm and b=1 mm, the amount of chocolate required is approximately 60.35475635 mm3.       c
c                                                                                                 c
c  Find the amount of chocolate in mm3 required if a=3 mm and b=1 mm. Give your answer as the     c
c  number rounded to 8 decimal places behind the decimal point.                                   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only 
      double precision integral, x, z, x_new, z_new, x_old, z_old
      double precision a, b, center_int, center
      
      external center_int

c initialize the results
      integral = 0.d0
      
      x_old = 0.d0
      z_old = 2.d0

      do x1=1,1000000000
C         x = 2.0d0*dble(x1)/1000000000.d0
C         z = dsqrt((4.d0-x**2)/4.d0)
        x = 3.0d0*dble(x1)/1000000000.d0
        z = dsqrt((9.d0-x**2)/9.d0)
        
C         x_new = x + x/dsqrt(16.d0-3.d0*x**2)
C         z_new = z + 4.d0*z/dsqrt(16.d0-3.d0*x**2)
        x_new = x + x/dsqrt(81.d0-8.d0*x**2)
        z_new = z + 9.d0*z/dsqrt(81.d0-8.d0*x**2)
        
        integral = integral + (x_new**2-x_old**2)*(z_old+z_new)/4.d0
        
        x_old = x_new
        z_old = z_new
      enddo
      
      integral = integral*16.d0*datan(1.d0)
      
c now to get the volume of the center
      a = 0.d0
      b = 4.d0*datan(1.d0)
      call qromb(center_int,a,b,center)
      
      write(*,*) integral, center
      write(*,*) 'The amount of candy coating is ',integral - center

      end
      


c-------------------------------------------------------------------------------------------------c
      function center_int(theta)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function gives the theta integral for romberg to integrate over.                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c input parameters and function definition
      double precision theta, center_int

c parameters used in this function only
      double precision r_theta

C       r_theta = dsqrt(4.d0/(1.d0+3.d0*dcos(theta)**2))
      r_theta = dsqrt(9.d0/(1.d0+8.d0*dcos(theta)**2))

      center_int = dsin(theta)*r_theta**3*8.d0*datan(1.d0)/3.d0

      return
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      subroutine qromb(func,a,b,ss)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Returns as ss the integral of the function func from a to b.  Integration is performed by      c
c  Romberg's method of order 2K, where, e.g. K=2 is Simpson's rule.  Parameters: EPS is the       c
c  fractional accuracy desired as determined by the extrapolation error estimate; JMAX limits     c
c  the total number of steps; K is the number of poitnes used in the extrapolation                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

      integer jmax, jmaxp, k, km
      double precision a, b, func, ss, eps
      external func
      parameter (eps=1.0d-12, jmax=20, jmaxp=jmax+1, k=5, km=k-1)

c uses polint, trapzd
      integer j

c these store the successive trapezoidal approximations and their
c relative stepsizes
      double precision dss, h(jmaxp), s(jmaxp)
      h(1) = 1.d0
      do j=1,jmax
        call trapzd(func,a,b,s(j),j)
        if (j.ge.k) then
          call polint(h(j-km),s(j-km),k,0.d0,ss,dss)
          if (dabs(dss).le.(eps*dabs(ss))) return
        endif
        s(j+1)=s(j)

c This is a key step: The factor is 0.25 even though the stepsize decrease is only 0.5
c This makes the extrapolation a polynomial in h^2.
        h(j+1)=0.25d0*h(j)
      enddo

      pause 'too many steps in qromb'
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      subroutine polint(xa,ya,n,x,y,dy)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Given arrays xa and ya, each of length n, and given a value x, this routine returns a value    c
c  y, and an error estimate dy.  If P(x) is the polynomial of degree N=1 such that                c
c  P(xa_i) = ya_i, i=1...n then the returned value y = P(x).                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      integer n,nmax
      double precision dy,x,y,xa(n),ya(n)
      parameter (nmax=10)
      integer i,m,ns
      double precision den,dif,dift,ho,hp,w,c(nmax),d(nmax)
      ns = 1
      dif = dabs(x-xa(1))

c Here we find the index ns of the closest table entry
      do i=1,n
        dift = dabs(x-xa(i))
        if (dift.lt.dif) then
          ns = i
          dif = dift
        endif

c and initialize the tableau of c's and d's
        c(i) = ya(i)
        d(i) = ya(i)
      enddo

c This is the initial approximation to y
      y = ya(ns)
      ns = ns-1

c For each column of the tableau,
c we loop over the current c's and d's and update them
      do m=1,n-1
        do i=1,n-m
          ho = xa(i)-x
          hp = xa(i+m)-x
          w = c(i+1)-d(i)
          den = ho-hp

c This error can occur only if two input xa's are (to within roundoff) identical.
          if (den .eq. 0.d0) pause 'failure in polint'
          den = w/den

c Here the c's and d's are updated
          d(i) = hp*den
          c(i) = ho*den
        enddo
        if(2*ns .lt. n-m) then
          dy = c(ns+1)
        else
          dy = d(ns)
          ns = ns-1
        endif
        y = y+dy
      enddo

      return
      end
      
      

c-------------------------------------------------------------------------------------------------c
      subroutine trapzd(func,a,b,s,n)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This routine computes the nth stage of refinement of an extended trapezoidal rule.  func is    c
c  input as the name of the function to be integrated between limits a and b, also input.  When   c
c  called with n=1, the routine returns as s the crudent estimate of the integral.  Subsequent    c
c  calls with n=2,3,... will improve the accuracy of s by adding 2^(n-2) additional interior      c
c  points.  s should not be modified between sequential calls.                                    c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

      integer n
      double precision a,b,s,func
      external func
      integer it,j
      double precision del,sum,tnm,x

      if (n.eq.1) then
        s = 0.5d0*(b-a)*(func(a)+func(b))
      else
        it = 2**(n-2)
        tnm = it

c This is the spacing of the points to be added
        del = (b-a)/tnm
        x = a+0.5d0*del
        sum = 0.d0
        do j=1,it
          sum = sum+func(x)
          x = x+del
        enddo

c This replaces s by its refined value
        s = 0.5d0*(s+(b-a)*sum/tnm)
      endif

      return
      end      