c-------------------------------------------------------------------------------------------------c
      program project_euler_53
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  There are exactly ten ways of selecting three from five, 12345:                                c
c                                                                                                 c
c  123, 124, 125, 134, 135, 145, 234, 235, 245, and 345                                           c
c                                                                                                 c
c  In combinatorics, we use the notation, 5C3 = 10.                                               c
c                                                                                                 c
c  In general, nCr = n!/r!(n−r)!, where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.                c
c                                                                                                 c
c  It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.                     c
c                                                                                                 c
c  How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than          c
c  one-million?                                                                                   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 pascal(0:100,0:100), total
      
c start off pascal's triangle
      total = 0
      pascal(0,0) = 1
      
      do x1=1,100
        do x2=0,min(x1,10)
          if ((x2.eq.0).or.(x2.eq.x1)) then
            pascal(x1,x2) = 1
          else
            pascal(x1,x2) = pascal(x1-1,x2-1) + pascal(x1-1,x2)
          endif
          
          if (pascal(x1,x2).gt.1000000) then
            total = total + x1+1 - 2*x2
            exit
          endif
        enddo
      enddo
      
      write(*,*) 'The number of pascal numbers with n<100 greater than one million is ',total
          
      end