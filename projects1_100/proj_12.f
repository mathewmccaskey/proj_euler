c-------------------------------------------------------------------------------------------------c
      program project_euler_12
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The sequence of triangle numbers is generated by adding the natural numbers. So the 7th        c
c  triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:         c
c                                                                                                 c
c  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...                                                       c
c                                                                                                 c
c  Let us list the factors of the first seven triangle numbers:                                   c
c                                                                                                 c
c     1: 1                                                                                        c
c     3: 1,3                                                                                      c
c     6: 1,2,3,6                                                                                  c
c    10: 1,2,5,10                                                                                 c
c    15: 1,3,5,15                                                                                 c
c    21: 1,3,7,21                                                                                 c
c    28: 1,2,4,7,14,28                                                                            c
c                                                                                                 c
c  We can see that 28 is the first triangle number to have over five divisors.                    c
c                                                                                                 c
c  What is the value of the first triangle number to have over five hundred divisors?             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num_divisors, triangle_num
      
c initialize the number of divisors
      num_divisors = 0
      x1 = 1
      
      do while(num_divisors .le. 500)
        num_divisors = 1
        triangle_num = triangle(x1)
        
        do x2=2,int(dsqrt(dble(triangle_num)))
          if (is_multiple(triangle_num,x2)) num_divisors = num_divisors + 1
        enddo
        
        num_divisors = num_divisors*2
        x1 = x1 + 1
      enddo
      
      write(*,*) 'The triangle number with larger than 500 divisors is ',triangle_num
      
      end    