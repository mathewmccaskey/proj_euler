c-------------------------------------------------------------------------------------------------c
      program project_euler_50
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The prime 41, can be written as the sum of six consecutive primes:                             c
c  41 = 2 + 3 + 5 + 7 + 11 + 13                                                                   c
c                                                                                                 c
c  This is the longest sum of consecutive primes that adds to a prime below one-hundred.          c
c                                                                                                 c
c  The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21     c
c  terms, and is equal to 953.                                                                    c
c                                                                                                 c
c  Which prime, below one-million, can be written as the sum of the most consecutive primes?      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this
      integer*8 primes(78498), p_index, max_len, start, sum, result
      
c generate the list of primes
      p_index = 0
      do x1=1,1000000
        if (is_prime(x1)) then
          p_index = p_index + 1
          primes(p_index) = x1
        endif
      enddo
      write(*,*) 'Primes done!'
      
c initialize the search
      max_len = 0
      do x1=2,78498
        do x2=x1-1,1,-1
          start = x2
          sum = primes(start)
          do while((sum.lt.primes(x1)).and.(start.ge.1))
            start = start - 1
            sum = sum + primes(start)
          enddo
          if (sum.eq.primes(x1)) then
            if (max_len.lt.(x2-start+1)) then
              max_len = x2-start+1
              result = primes(x1)
            endif
          endif
        enddo
      enddo
      
      write(*,*) 'The prime below 1 million that can be written as the longest sum of consecutive primes is ',result
      
      end