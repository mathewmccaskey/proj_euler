c-------------------------------------------------------------------------------------------------c
      program project_euler_30
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Surprisingly there are only three numbers that can be written as the sum of fourth powers of   c
c  their digits:                                                                                  c
c                                                                                                 c
c    1634 = 1^4 + 6^4 + 3^4 + 4^4                                                                 c
c    8208 = 8^4 + 2^4 + 0^4 + 8^4                                                                 c
c    9474 = 9^4 + 4^4 + 7^4 + 4^4                                                                 c
c                                                                                                 c
c  As 1 = 1^4 is not a sum it is not included.                                                    c
c                                                                                                 c
c  The sum of these numbers is 1634 + 8208 + 9474 = 19316.                                        c
c                                                                                                 c
c Find the sum of all the numbers that can be written as the sum of fifth powers of their digits  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 number(8), int_num, digit_sum, result

c initialize variables
      do x1=1,8
        number(x1) = 0
      enddo
      int_num = 0
      result = 0
      
      do while(number(8).eq.0)
        int_num = int_num + 1
        number(1) = number(1) + 1
        
        do x1=1,8
          if (number(x1).eq.10) then
            number(x1) = number(x1) - 10
            number(x1+1) = number(x1+1) + 1
          endif
        enddo
        
        digit_sum = 0
        do x1=1,8
          digit_sum = digit_sum + number(x1)**4
        enddo
        
        if ((digit_sum.eq.int_num).and.(int_num.eq.1)) then
          result = result + int_num
        endif
      enddo
      
      write(*,*) 'The sum of all numbers that can be written as the sum of fifth powers of their digits is ',result
      
      end
          