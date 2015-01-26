c-------------------------------------------------------------------------------------------------c
      program project_euler_187
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A composite is a number containing at least two prime factors. For example, 15 = 3 × 5;        c
c  9 = 3 × 3; 12 = 2 × 2 × 3.                                                                     c
c                                                                                                 c
c  There are ten composites below thirty containing precisely two, not necessarily distinct,      c
c  prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.                                            c
c                                                                                                 c
c  How many composite integers, n < 10^8, have precisely two, not necessarily distinct, prime     c
c  factors?                                                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 primes(1229), num_primes, num_composite, index, factors
      
      num_primes = 1
      primes(1) = 2
      do x1=3,10000,2
        if (is_prime(x1)) then
          num_primes = num_primes + 1
          primes(num_primes) = x1
        endif
      enddo
              
      num_composite = 0
      do x1=1,10**8
        if (((x1/100000)*100000).eq.x1) then
          write(*,*) x1,num_composite
        endif
        
c initialize the index and the number of factors
        index = 1
        factors = 0

c loop that has multiple ways to break in case a number fails
        do while((primes(index).le.(int(dsqrt(dble(x1))))).and.(factors.le.2).and.(index.le.1229))
        
c check to see if this prime is a factor
          if (((x1/primes(index))*primes(index)).eq.x1) then
c if it is then check to see if the result of the division is also a factor
            if (is_prime(x1/primes(index))) then
              factors = factors + 2
c failing that we increase the number of factors by 4 to make sure this number fails
            else
              factors = factors + 4
            endif
          endif
          index = index + 1
        enddo
        
        if (factors.eq.2) then
          num_composite = num_composite + 1
c          write(*,*) num_composite, x1
        endif
      enddo
      
      write(*,*) 'The number of composites primes below 10**8 is ',num_composite
      
      end