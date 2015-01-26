c-------------------------------------------------------------------------------------------------c
      program project_euler_36
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The decimal number, 585 = 1001001001_{2} (binary), is palindromic in both bases.               c
c                                                                                                 c
c  Find the sum of all numbers, less than one million, which are palindromic in base 10 and       c
c  base 2.                                                                                        c
c                                                                                                 c
c  (Please note that the palindromic number, in either base, may not include leading zeros.)      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 result, base, digits_10(1000), n_digits_10, digits_2(1000), n_digits_2
      logical is_palindrome_10, is_palindrome_2
 
      result = 0
      do x1=1,1000000-1
        
        base = 10
        call get_digits(x1,base,digits_10,n_digits_10)
        is_palindrome_10 = .true.
        do x2=1,n_digits_10
          if (digits_10(x2).ne.digits_10(n_digits_10-x2+1)) is_palindrome_10 = .false.
        enddo
        
        if (is_palindrome_10) then
          
          base = 2
          call get_digits(x1,base,digits_2,n_digits_2)
          is_palindrome_2 = .true.
          do x2=1,n_digits_2
            if (digits_2(x2).ne.digits_2(n_digits_2-x2+1)) is_palindrome_2 = .false.
          enddo
          
          if (is_palindrome_2) result = result + x1
        endif
      enddo

      write(*,*) 'The sum of all numbers that are palindromes in base 2 and 10 is ',result

      end