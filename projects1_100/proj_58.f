c-------------------------------------------------------------------------------------------------c
      program project_euler_58
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side   c
c  length 7 is formed.                                                                            c
c                                                                                                 c
c  37 36 35 34 33 32 31                                                                           c
c  38 17 16 15 14 13 30                                                                           c
c  39 18  5  4  3 12 29                                                                           c
c  40 19  6  1  2 11 28                                                                           c
c  41 20  7  8  9 10 27                                                                           c
c  42 21 22 23 24 25 26                                                                           c
c  43 44 45 46 47 48 49                                                                           c
c                                                                                                 c
c  It is interesting to note that the odd squares lie along the bottom right diagonal, but what   c
c  is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that c
c  is, a ratio of 8/13 ≈ 62%.                                                                     c
c                                                                                                 c
c  If one complete new layer is wrapped around the spiral above, a square spiral with side        c
c  length 9 will be formed. If this process is continued, what is the side length of the square   c
c  spiral for which the ratio of primes along both diagonals first falls below 10%?               c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 primes(9592), p_index, side, n_primes, n_not_primes
      logical found_result, prime_test
      
c set up the primes
      p_index = 1
      primes(p_index) = 2
      x1 = 3
      do while(p_index.lt.9592)
        if(is_prime(x1)) then
          p_index = p_index + 1
          primes(p_index) = x1
        endif
        x1 = x1 + 2
      enddo 
      
c initialize the results (starting with 1 which is not prime)
      found_result = .false.
      n_primes = 0
      n_not_primes = 1
      x1 = 1
      side = 2
      
      do while(.not.found_result)
        do x2=1,4
          x1 = x1 + side
          prime_test = .true.
          do x3=1,9592
            if (((primes(x3)*(x1/primes(x3))).eq.x1).and.(primes(x3).ne.x1)) prime_test = .false.
          enddo
          
          if (prime_test) then
            n_primes = n_primes + 1
          else
            n_not_primes = n_not_primes + 1
          endif
        enddo
        
        if ((n_primes*10).lt.(n_primes+n_not_primes)) then
          found_result = .true.
        else
          side = side + 2
        endif
      enddo
      
      write(*,*) 'The side that works is ',side+1
      
      end
      