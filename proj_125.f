c-------------------------------------------------------------------------------------------------c
      program project_euler_125
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The palindromic number 595 is interesting because it can be written as the sum of consecutive  c
c  squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.                                           c
c                                                                                                 c
c  There are exactly eleven palindromes below one-thousand that can be written as consecutive     c
c  square sums, and the sum of these palindromes is 4164. Note that 1 = 0^2 + 1^2 has not been    c
c  included as this problem is concerned with the squares of positive integers.                   c
c                                                                                                 c
c  Find the sum of all the numbers less than 10^8 that are both palindromic and can be written as c
c  the sum of consecutive squares.                                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only  
      integer*8 start, test, total, digit(1000), ndigits, base, nums(200), n_nums
      logical in_list
      
c initialize the result
      total = 0
      base = 10
      n_nums = 0
      
      do start=1,10000
        test = start**2 + (start+1)**2
        x1 = 1
        do while(test.lt.10**8)
          call get_digits(test,base,digit,ndigits)
          if (is_palindrome(digit,ndigits)) then
            in_list = .false.
            do x2=1,n_nums
              if (nums(x2).eq.test) in_list = .true.
            enddo
            if (.not.in_list) then
              n_nums = n_nums + 1
              nums(n_nums) = test
              total = total + test
            else
              write(*,*) test
            endif
              
          endif
          x1 = x1 + 1
          test = test + (start+x1)**2
        enddo
      enddo
      
      write(*,*) 'The total sum of palindrones is ',total
      end