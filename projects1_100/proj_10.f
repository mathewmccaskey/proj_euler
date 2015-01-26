c-------------------------------------------------------------------------------------------------c
      program project_euler_10
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.                                          c
c                                                                                                 c
c  Find the sum of all the primes below two million.                                              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 prime_sum
      
c initialize the prime_sum
      prime_sum = 0
      
c loop over all the numbers below 2 million, check if it's prime and add
      do x1=2,2000000
        if (is_prime(x1)) prime_sum = prime_sum + x1
      enddo
      
      write(*,*) 'The sum of all primes below 2 million is ',prime_sum
      
      end