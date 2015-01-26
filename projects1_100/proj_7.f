c-------------------------------------------------------------------------------------------------c
      program project_euler_7
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime  c
c  is 13.                                                                                         c
c                                                                                                 c
c  What is the 10 001st prime number?                                                             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num_primes, nth_prime
      
c initialize the number of primes
      num_primes = 0
      nth_prime = 10001
      x1 = 1
      
      do while(num_primes .lt. nth_prime)
        x1 = x1 + 1
        if (is_prime(x1)) then
          num_primes = num_primes + 1
        endif
      enddo
      
      write(*,*) 'the ',nth_prime,' prime is ',x1
      
      end