c-------------------------------------------------------------------------------------------------c
      program project_euler_3
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The prime factors of 13195 are 5, 7, 13 and 29.                                                c
c                                                                                                 c
c  What is the largest prime factor of the number 600851475143 ?                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num2factor
      integer*8 prime, number_factors
      
c initialize the number to factor and the prime number
      num2factor = 600851475143
      prime = 1
      
c while the number hasn't been fully factorized keep finding primes to factorize it
      do while ((num2factor .gt. 1).and.(prime .lt. num2factor))
        prime = prime + 1
c        write(*,*) prime, is_prime(prime)

c if "prime" is prime then find out how many times it goes into num2factor
        if (is_prime(prime)) then
          number_factors = num_factors(num2factor,prime)
          num2factor = num2factor/(prime**number_factors)

          if (number_factors .gt. 0) then
            write(*,*) 'The number is divisible by ',prime,' (',number_factors,' times)'
          endif
        endif
      enddo
      
      end