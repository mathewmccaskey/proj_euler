c-------------------------------------------------------------------------------------------------c
      program project_euler_4
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A palindromic number reads the same both ways. The largest palindrome made from the product    c
c  of two 2-digit numbers is 9009 = 91 × 99.                                                      c
c                                                                                                 c
c  Find the largest palindrome made from the product of two 3-digit numbers.                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 product, num1, num2, max_palindrome
      integer*8 digit(16), ndigits
      logical is_palindrome

c initialize the max palindrome
      max_palindrome = 0

      do x1=101,999
        do x2=101,999
          product = x1*x2
          call get_digits(product,ndigits,digit)
          
          is_palindrome = .true.
          do x3=1,ndigits
            if (digit(x3).ne.digit(ndigits-x3+1)) is_palindrome = .false.
          enddo
          
          if ((is_palindrome) .and. (product .gt. max_palindrome)) then
            max_palindrome = product
            num1 = x1
            num2 = x2
          endif
        enddo
      enddo
      
      write(*,*) 'Max palindrome found ',max_palindrome
      write(*,*) 'a = ',num1
      write(*,*) 'b = ',num2
      
      end