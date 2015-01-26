c-------------------------------------------------------------------------------------------------c
      program project_euler_145
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely   c
c  of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such    c
c  numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in  c
c  either n or reverse(n).                                                                        c
c                                                                                                 c
c  There are 120 reversible numbers below one-thousand.                                           c
c                                                                                                 c
c  How many reversible numbers are there below one-billion (10^9)?                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 result, number_digits(1000), num_digits, sum_digits(1000)
      logical all_odd
      
c initialize parameters
      result = 0
      do x1=1,1000
        number_digits(x1) = 0
      enddo
      number_digits(1) = 1
      num_digits = 1
      
c loop over all the numbers
      do while(number_digits(10).eq.0)
c        if (number_digits(8).eq.8) write(*,fmt='(11(I1))') (number_digits(x2),x2=num_digits,1,-1)

c leading zeros are not allowed therefore number can't be dividible by 10
        if (number_digits(1).ne.0) then

c initialize the sum        
          do x1=1,1000
            sum_digits(x1) = 0
          enddo

c add the number to its reverse
          do x1=1,num_digits
            sum_digits(x1) = sum_digits(x1) + number_digits(x1) + number_digits(num_digits+1-x1)

c carry over the 10 if necessary
            if ((sum_digits(x1)).ge.10) then
              sum_digits(x1) = sum_digits(x1) - 10
              sum_digits(x1+1) = sum_digits(x1+1) + 1
            endif
          enddo

c check to see if the sum is full of odd numbers
          all_odd = .true.
          do x1=1,num_digits
            if (((sum_digits(x1)/2)*2) .eq. sum_digits(x1)) all_odd = .false.
          enddo
        
          if (all_odd) then
            result = result + 1
c            write(*,fmt='(I10,2X,I4,2X,11(I1))') result, num_digits, (number_digits(x2),x2=num_digits,1,-1)
c            if (((result/10000)*10000) .eq. result) write(*,fmt='(I10,2X,10(I1))') result, (number_digits(x2),x2=num_digits,1,-1)
          endif  
        endif
        
c add 1 to the number
        number_digits(1) = number_digits(1) + 1
        do x1=1,num_digits
          if (number_digits(x1).eq.10) then
            number_digits(x1) = number_digits(x1) - 10
            number_digits(x1+1) = number_digits(x1+1) + 1
          endif
        enddo
        
        if (number_digits(num_digits+1).eq.1) num_digits = num_digits + 1
      enddo
      
      write(*,*) 'The number of reversible numbers is ',result
      end