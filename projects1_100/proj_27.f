c-------------------------------------------------------------------------------------------------c
      program project_euler_27
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Euler published the remarkable quadratic formula:                                              c
c                                                                                                 c
c  n² + n + 41                                                                                    c
c                                                                                                 c
c  It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.   c
c  However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when   c
c  n = 41, 41² + 41 + 41 is clearly divisible by 41.                                              c
c                                                                                                 c
c  Using computers, the incredible formula  n² − 79n + 1601 was discovered, which produces 80     c
c  primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601,  c
c  is −126479.                                                                                    c
c                                                                                                 c
c  Considering quadratics of the form:                                                            c
c                                                                                                 c
c    n² + an + b, where |a| < 1000 and |b| < 1000                                                 c
c                                                                                                 c
c    where |n| is the modulus/absolute value of n                                                 c
c    e.g. |11| = 11 and |−4| = 4                                                                  c
c                                                                                                 c
c  Find the product of the coefficients, a and b, for the quadratic expression that produces the  c
c  maximum number of primes for consecutive values of n, starting with n = 0.                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 a, b, n, number, max_primes, a_save, b_save

c initialize the answer
      max_primes = 0
      a_save = 0
      b_save = 0
      
c b must be both positive and a prime
      do b=0,999
        if (is_prime(b)) then
          do a=-999,999
            n = 0
            number = n**2 + a*n + b
            do while(is_prime(number))
              n = n + 1
              number = n**2 + a*n + b
            enddo
            
            if (n.gt.max_primes) then
              max_primes = n
              a_save = a
              b_save = b
            endif
          enddo
        endif
      enddo
      
      write(*,*) 'The formula has coefficients ',a_save,' and ',b_save
      write(*,*) 'Which produces ',max_primes,' consecutive primes'
      write(*,*) 'The product of coefficients is ',a_save*b_save
      
      end