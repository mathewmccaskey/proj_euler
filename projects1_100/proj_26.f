c-------------------------------------------------------------------------------------------------c
      program project_euler_26
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions  c
c  with denominators 2 to 10 are given:                                                           c
c                                                                                                 c
c     1/2	= 	0.5                                                                                 c
c     1/3	= 	0.(3)                                                                               c
c     1/4	= 	0.25                                                                                c
c     1/5	= 	0.2                                                                                 c
c     1/6	= 	0.1(6)                                                                              c
c     1/7	= 	0.(142857)                                                                          c
c     1/8	= 	0.125                                                                               c
c     1/9	= 	0.(1)                                                                               c
c     1/10	= 	0.1                                                                               c
c                                                                                                 c
c  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7     c
c  has a 6-digit recurring cycle.                                                                 c
c                                                                                                 c
c  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal   c
c  fraction part.                                                                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 divisor(1000), remainder(1000), index, max_length, d
      logical cycle_over
      
c initialize the max_lngth of the recurring cycle
      max_length = 0
      
      do x1=2,100
        index = 1
        cycle_over = .false.
        
        if (x1.le.10) then
          divisor(index) = 10/x1
          remainder(index) = 10 - divisor(index)*x1
        else if (x1.le.100) then
          divisor(1) = 0
          remainder(1) = -1
          index = index + 1
          divisor(index) = 100/x1
          remainder(index) = 100 - divisor(index)*x1
        else if (x1.le.1000) then
          divisor(1) = 0
          divisor(2) = 0
          remainder(1) = -1
          remainder(2) = -2
          index = index + 2
          divisor(index) = 1000/x1
          remainder(index) = 1000 - divisor(index)*x1
        endif
        
        do while(.not.(cycle_over))
          index = index + 1
          divisor(index) = (remainder(index-1)*10)/x1
          remainder(index) = (remainder(index-1)*10) - divisor(index)*x1
          
          do x2=1,index-1
            if ((divisor(x2).eq.divisor(index)).and.(remainder(x2).eq.remainder(index)).and.(.not.(cycle_over))) then
              cycle_over = .true.
              write(*,fmt='(I4,1X,A2,300I1)') x1, '0.',(divisor(x3), x3=1,index-1)
              
              if ((index-x2).gt.max_length) then
                max_length = index-x2
                d = x1
              endif
            endif
          enddo
        enddo
      enddo
      
      write(*,*) 'The number in which 1/d has the largest cycle is ',d
      write(*,*) 'The cycle itself has a length of ',max_length
      
      end