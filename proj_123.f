c-------------------------------------------------------------------------------------------------c
      program project_euler_123
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Let p_n be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the remainder when                 c
c  (p_n−1)n + (p_n+1)^n is divided by (p_n)^2.                                                    c
c                                                                                                 c
c  For example, when n = 3, p_3 = 5, and 4^3 + 6^3 = 280 ≡ 5 mod 25.                              c
c                                                                                                 c
c  The least value of n for which the remainder first exceeds 10^9 is 7037.                       c
c                                                                                                 c
c  Find the least value of n for which the remainder first exceeds 10^10.                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 n, prime
      double precision remainder
      
c 71059 is the 7037th prime so that is where we will start
      n = 7037-1
      prime = 71059-2
      
c the remainder for this problem was solved in problem 120.
c r = 2*n*prime because 2*n*prime < prime^2 so we don't need to mod anything else
c n must also be odd because if n is even then the remainder will always and forever be 2
      remainder = 2.d0*n*prime
      
      do while(remainder.lt.(10.d0**10))
        prime = prime + 2
        if (is_prime(prime)) then
          n = n + 1
          if (((n/2)*2).ne.n) then
            remainder = 2*n*prime
            write(*,*) n,prime,remainder
          endif
        endif
      enddo

      write(*,*) 'The value of n for which the remainder first exceeds 10^10 is ',n
      
      end