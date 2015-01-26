c-------------------------------------------------------------------------------------------------c
      program project_euler_23
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A perfect number is a number for which the sum of its proper divisors is exactly equal to the  c
c  number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,    c
c  which means that 28 is a perfect number.                                                       c
c                                                                                                 c
c  A number n is called deficient if the sum of its proper divisors is less than n and it is      c
c  called abundant if this sum exceeds n.                                                         c
c                                                                                                 c
c  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can    c
c  be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be       c
c  shown that all integers greater than 28123 can be written as the sum of two abundant numbers.  c
c  However, this upper limit cannot be reduced any further by analysis even though it is known    c
c  that the greatest number that cannot be expressed as the sum of two abundant numbers is less   c
c  than this limit.                                                                               c
c                                                                                                 c
c  Find the sum of all the positive integers which cannot be written as the sum of two abundant   c
c  numbers.                                                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 abundant_list(10000), abundant_count, divisor_sum, answer
      logical is_abundant_sum
      
c initalize the abundant number count
      abundant_count = 0
      
      do x1=12,28123
        divisor_sum = 1
        do x2=2,int(dsqrt(dble(x1)))
          if (is_multiple(x1,x2)) then
            divisor_sum = divisor_sum + x2
            if (x2.ne.(x1/x2)) then
              divisor_sum = divisor_sum + x1/x2
            endif
          endif
        enddo
        
        if (divisor_sum .gt. x1) then
          abundant_count = abundant_count + 1
          abundant_list(abundant_count) = x1
        endif
      enddo
      
C       do x1=1,abundant_count
C         write(*,*) x1,abundant_list(x1)
C       enddo
      
      answer = 0
      do x1=1,28123
        is_abundant_sum = .false.
        x2 = 1
        do while (.not.(is_abundant_sum))
          if (abundant_list(x2).lt.x1) then
            do x3=1,abundant_count
              if ((abundant_list(x2)+abundant_list(x3)).eq.x1) then
                is_abundant_sum = .true.
c                write(*,*) abundant_list(x2), abundant_list(x3), x1
                exit
              endif
              if ((abundant_list(x2)+abundant_list(x3)).gt.x1) exit
            enddo
          else
            exit
          endif

          x2 = x2 + 1
          if (x2.gt.abundant_count) exit          
        enddo
        
        if (.not.(is_abundant_sum)) answer = answer + x1
      enddo
      
      write(*,*) 'The sum of all numbers that cannot be written as the sum of two abundant numbers is ',answer
      
      end
      
      