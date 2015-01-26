c-------------------------------------------------------------------------------------------------c
      program project_euler_21
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide       c
c  evenly into n).  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and  c
c  each of a and b are called amicable numbers.                                                   c
c                                                                                                 c
c  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;        c
c  therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.   c
c                                                                                                 c
c  Evaluate the sum of all the amicable numbers under 10000.                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 sum_amicable, test_pair, sum_divisors, sum_divisors2

c loop over all the numbers from 2 to 10000
      do x1=6,10000
        sum_divisors = 1
        do x2=2,int(sqrt(dble(x1)))
          if (mod(x1,x2).eq.0) then
            sum_divisors = sum_divisors + x2 + x1/x2
          endif
        enddo
        
        sum_divisors2 = 1
        do x2=2,int(sqrt(dble(sum_divisors)))
          if (mod(sum_divisors,x2).eq.0) then
            sum_divisors2 = sum_divisors2 + x2 + sum_divisors/x2
          endif
        enddo
        
        if ((sum_divisors2.eq.x1).and.(x1.ne.sum_divisors)) then
          sum_amicable = sum_amicable + x1 + sum_divisors2
        endif
      enddo
      
      sum_amicable = sum_amicable/2
      write(*,*) 'The sum of all amicable pairs under 10000 is ',sum_amicable
      
      end