c-------------------------------------------------------------------------------------------------c
      program project_euler_1
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Project Euler Problem 1:                                                                       c
c                                                                                                 c
c  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6       c
c  and 9. The sum of these multiples is 23.                                                       c
c                                                                                                 c
c  Find the sum of all the multiples of 3 or 5 below 1000.                                        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 max_int, sum
      logical ismult_3, ismult_5

c initialize the sum and the max integer
      sum = 0
      max_int = 1000

c loop over all the numbers from 1 to max_int-1
      do x1=1,max_int-1
        if ((is_multiple(x1,3)).or.(is_multiple(x1,5))) then
          sum = sum + x1
        endif
      enddo
      
      write(*,*) 'The sum of all the multiples of 3 or 5 below ',max_int,' is ',sum
      
      end