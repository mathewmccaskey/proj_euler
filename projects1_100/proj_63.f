c-------------------------------------------------------------------------------------------------c
      program project_euler_63
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number,           c
c  134217728=8^9, is a ninth power.                                                               c
c                                                                                                 c
c  How many n-digit positive integers exist which are also an nth power?                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 power, num_ints, digits1(1000), ndigits1, digits2(1000), ndigits2, digits3(1000), ndigits3 

c initialize the number of ints
      num_ints = 9
      
c the base can only go from 1-9.  10^1 is a 2-digit number and 10^2 is a 3-digit number
      do x1=1,9
        power = 2
        digits1(1) = x1
        ndigits1 = 1
        digits2(1) = x1
        ndigits2 = 1
        call big_number_product(digits1,ndigits1,digits2,ndigits2,digits3,ndigits3)
        do while(ndigits3.eq.power)
          write(*,*) x1, power
          num_ints = num_ints + 1
          ndigits1 = ndigits3
          do x2=1,ndigits1
            digits1(x2) = digits3(x2)
          enddo
          call big_number_product(digits1,ndigits1,digits2,ndigits2,digits3,ndigits3)
          power = power + 1
        enddo
      enddo
      
      write(*,*) 'The number of n-digit positive integers which is also an nth power is ',num_ints
        
      end