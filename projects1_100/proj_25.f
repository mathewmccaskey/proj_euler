c-------------------------------------------------------------------------------------------------c
      program project_euler_25
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The Fibonacci sequence is defined by the recurrence relation:                                  c
c                                                                                                 c
c     Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.                                                  c
c                                                                                                 c
c  Hence the first 12 terms will be:                                                              c
c                                                                                                 c
c    F1 = 1                                                                                       c
c    F2 = 1                                                                                       c
c    F3 = 2                                                                                       c
c    F4 = 3                                                                                       c
c    F5 = 5                                                                                       c
c    F6 = 8                                                                                       c
c    F7 = 13                                                                                      c
c    F8 = 21                                                                                      c
c    F9 = 34                                                                                      c
c    F10 = 55                                                                                     c
c    F11 = 89                                                                                     c
c    F12 = 144                                                                                    c
c                                                                                                 c
c  The 12th term, F12, is the first term to contain three digits.                                 c
c                                                                                                 c
c  What is the first term in the Fibonacci sequence to contain 1000 digits?                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 big_num(1000), big_num_digits, fibonacci_num, b(1000), b_digits, c(1000), c_digits
      
c inititialize everything
      big_num(1) = 1
      big_num_digits = 1
      b(1) = 1
      b_digits = 1
      fibonacci_num = 2
      c_digits = 0
      
      do while(c_digits.lt.1000)
        
        call big_number_sum(big_num,big_num_digits,b,b_digits,c,c_digits)
        
        big_num_digits = b_digits
        do x1=1,b_digits
          big_num(x1) = b(x1)
        enddo
        b_digits = c_digits
        do x1=1,c_digits
          b(x1) = c(x1)
        enddo
        
        fibonacci_num = fibonacci_num + 1
      enddo
      
      write(*,*) 'The fibonacci number with larger than 1000 digits is ',fibonacci_num
      
      end