c-------------------------------------------------------------------------------------------------c
      program project_euler_34
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.                                 c
c                                                                                                 c
c  Find the sum of all numbers which are equal to the sum of the factorial of their digits.       c
c                                                                                                 c
c  Note: as 1! = 1 and 2! = 2 are not sums they are not included.                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 number, n_digits, digit(16), test_sum, result

c initialize the result
      result = 0
      
c loop over all the numbers
      do number=3,1000000
        call get_digits(number, n_digits, digit)
        
        test_sum = 0
        do x1=1,n_digits
          test_sum = test_sum + factorial(digit(x1))
        enddo
        
        if (test_sum.eq.number) then
          result = result + number
          write(*,*) number
        endif
      enddo
      
      write(*,*) 'The sum of all numbers that are equal to the sum of the factorials of the digits is ',result
      
      end
        