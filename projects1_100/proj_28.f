c-------------------------------------------------------------------------------------------------c
      program project_euler_28
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral    c
c  is formed as follows:                                                                          c
c                                                                                                 c
c  21 22 23 24 25                                                                                 c
c  20  7  8  9 10                                                                                 c
c  19  6  1  2 11                                                                                 c
c  18  5  4  3 12                                                                                 c
c  17 16 15 14 13                                                                                 c
c                                                                                                 c
c  It can be verified that the sum of the numbers on the diagonals is 101.                        c
c                                                                                                 c
c What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 spiral(1001,1001), number, x_loc, y_loc, direction, step, result
      
c start off in the center
      x_loc = 501
      y_loc = 501
      number = 1 
      spiral(x_loc,y_loc) = number
      
c direction = 1,2,3,4 (right, down, left, up)
      direction = 1
      step = 1
      
      do while(step.lt.1001)
        do x1=1,step
          number = number + 1
          if (direction.eq.1) y_loc = y_loc + 1
          if (direction.eq.2) x_loc = x_loc + 1
          if (direction.eq.3) y_loc = y_loc - 1
          if (direction.eq.4) x_loc = x_loc - 1
          
          spiral(x_loc,y_loc) = number
        enddo
        
        if ((direction.eq.2).or.(direction.eq.4)) step = step + 1
        
        direction = direction + 1
        if (direction.eq.5) direction = 1
      enddo
      
      do x1=1,step
        number = number + 1
        y_loc = y_loc + 1
        spiral(x_loc,y_loc) = number
      enddo
      
C       write(*,fmt='(5(I3))') (spiral(499,x1), x1=499,503)
C       write(*,fmt='(5(I3))') (spiral(500,x1), x1=499,503)
C       write(*,fmt='(5(I3))') (spiral(501,x1), x1=499,503)
C       write(*,fmt='(5(I3))') (spiral(502,x1), x1=499,503)
C       write(*,fmt='(5(I3))') (spiral(503,x1), x1=499,503)
      result = 0
      do x1=1,1001
        result = result + spiral(x1,x1) + spiral(x1,1002-x1)
      enddo
      
      write(*,*) 'The sum of all the elements on the diagonal is ',result
      
      end
