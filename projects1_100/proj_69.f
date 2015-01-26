c-------------------------------------------------------------------------------------------------c
      program project_euler_69
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the   c
c  number of numbers less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7,  c
c  and 8, are all less than nine and relatively prime to nine, φ(9)=6.                            c
c                                                                                                 c
c   n 	Relatively Prime 	φ(n) 	n/φ(n)                                                            c
c   2 	1 	              1 	  2                                                                 c
c   3 	1,2 	            2 	  1.5                                                               c
c   4 	1,3 	            2 	  2                                                                 c
c   5 	1,2,3,4 	        4 	  1.25                                                              c
c   6 	1,5 	            2 	  3                                                                 c
c   7 	1,2,3,4,5,6 	    6 	  1.1666...                                                         c
c   8 	1,3,5,7 	        4 	  2                                                                 c
c   9 	1,2,4,5,7,8 	    6 	  1.5                                                               c
c  10 	1,3,7,9 	        4 	  2.5                                                               c
c                                                                                                 c
c  It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.                                  c
c                                                                                                 c
c  Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 primes(168), dummy, p_index, max_tot
      double precision max_totient, totient
      
c set up the primes
      p_index = 1
      primes(p_index) = 2
      x1 = 3
      do while(p_index.lt.168)
        if(is_prime(x1)) then
          p_index = p_index + 1
          primes(p_index) = x1
        endif
        x1 = x1 + 2
      enddo 

c initialize the result
      max_totient = 0.d0
      
c the totient function phi(n) = n \prod (1-1/p) for all primes p that factor into n
c since we are looking for the max of n/phi(n) then we find the max of 1/prod(1-1/p)
      do x1=2,1000000
        if (is_prime(x1)) then
          totient = dble(x1)/dble(x1-1)
        else
          totient = 1.d0
          dummy = x1
          p_index = 1
          do while ((dummy.gt.1).and.(p_index.le.168))
            if ((primes(p_index)*(dummy/primes(p_index))).eq.dummy) then
              totient = totient*dble(primes(p_index))/dble(primes(p_index)-1)
              do while ((primes(p_index)*(dummy/primes(p_index))).eq.dummy)
                dummy = dummy/primes(p_index)
              enddo
            endif
            p_index = p_index + 1
          enddo
        endif

c        write(*,*) x1,totient
c check to see if the totient is the max
        if (totient.gt.max_totient) then
          max_totient = totient
          max_tot = x1
        endif
      enddo

      write(*,*) 'The maximum totient for n<1000000 is ',max_tot
      end