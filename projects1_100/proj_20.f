c-------------------------------------------------------------------------------------------------c
      program project_euler_18
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  n! means n × (n − 1) × ... × 3 × 2 × 1                                                         c
c                                                                                                 c
c  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,                                         c
c  and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.                 c
c                                                                                                 c
c  Find the sum of the digits in the number 100!                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 num1(1000), num2(1000), num3(1000), len_num1, len_num2, len_num3, sum_digits
      
c initialize the big number
      len_num3 = 1
      num3(1) = 1
      len_num2 = 1
      
c loop over all the numbers to get n!
      do x1=1,100
        do x2=1,len_num3
          num1(x2) = num3(x2)
        enddo
        len_num1 = len_num3
        num2(1) = x1
        
        call big_number_product(num1,len_num1,num2,len_num2,num3,len_num3)
      enddo
      
      sum_digits = 0
      do x1=1,len_num3
        sum_digits = sum_digits + num3(x1)
      enddo
      
      write(*,*) 'The sum of the digits of 100! is ',sum_digits
      
      end