c-------------------------------------------------------------------------------------------------c
      program project_euler_97
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The first known prime found to exceed one million digits was discovered in 1999, and is a      c
c  Mersenne prime of the form 2^6972593−1; it contains exactly 2,098,960 digits. Subsequently     c
c  other Mersenne primes, of the form 2^p−1, have been found which contain more digits.           c
c                                                                                                 c
c  However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: c
c  28433×2^7830457+1.                                                                             c
c                                                                                                 c
c  Find the last ten digits of this prime number.                                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 digit(10), counter
      
c initialize with the coefficient
      digit(1) = 3
      digit(2) = 3
      digit(3) = 4
      digit(4) = 8
      digit(5) = 2
      
      do x1=1,7830457
        do x2=1,10
          digit(x2) = digit(x2)*2
        enddo
        
        do x2=1,10
          if (digit(x2).ge.10) then
            digit(x2) = digit(x2) - 10
            if (x2.ne.10) digit(x2+1) = digit(x2+1) + 1
          endif
        enddo
      enddo

c add 1
      digit(1) = digit(1) + 1

      write(*,fmt='(10(I1))') (digit(x1), x1=10,1,-1)
      
      end