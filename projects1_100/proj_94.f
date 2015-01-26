c-------------------------------------------------------------------------------------------------c
      program project_euler_94
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It is easily proved that no equilateral triangle exists with integral length sides and         c
c  integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.  c
c                                                                                                 c
c  We shall define an almost equilateral triangle to be a triangle for which two sides are equal  c
c  and the third differs by no more than one unit.                                                c
c                                                                                                 c
c  Find the sum of the perimeters of all almost equilateral triangles with integral side lengths  c
c  and area and whose perimeters do not exceed one billion (1,000,000,000).                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this subroutine only
      integer*8 total, frac(0:100), index, numer, denom, dummy, a, b, c
      logical done

c initailze some parameters
      total = 0
      done = .false.

c here is the first solution to the Pell's equation
      frac(0) = 1
      do x1=1,100
        if (((x1/2)*2).eq.x1) then
          frac(x1) = 2
        else
          frac(x1) = 1
        endif
      enddo
      index = 3
      
c loop over all the solutions until we have triangles that have a perimeter larger than 1 billion
      do while(.not.done)
        numer = 1
        denom = frac(index)
        do x1=index-1,1,-1
          dummy = denom
          denom = denom*frac(x1) + numer
          numer = dummy
        enddo
        numer = denom*frac(0) + numer
                
        b = denom
        a = (2*numer+1)/3
        if ((2*numer+1).ne.3*((2*numer+1)/3)) then
          a = (2*numer-1)/3
        endif
        c = int(dsqrt(dble(a**2-b**2)))
        if ((a**2).eq.(b**2+c**2)) then
          if (abs(2*c-a).eq.1) then
            if (2*(a+c).gt.1000000000) then
              done = .true.
            else
              total = total + 2*(a+c)
              write(*,*) a,2*c,total
            endif
          endif
        endif
        index = index + 1
      enddo
                    
      write(*,*) 'total perimeter of all almost equilateral triangles is ',total
      
      end