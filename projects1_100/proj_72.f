c-------------------------------------------------------------------------------------------------c
      program project_euler_72
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is  c
c  called a reduced proper fraction.                                                              c
c                                                                                                 c
c  If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:   c
c                                                                                                 c
c  1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, c
c  6/7, 7/8                                                                                       c
c                                                                                                 c
c  It can be seen that there are 21 elements in this set.                                         c
c                                                                                                 c
c  How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000? c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 primes(168), dummy, p_index, total, totient
      
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
      total = 0
      
c the totient function phi(n) = n \prod (1-1/p) for all primes p that factor into n
      do x1=2,1000000
        if (is_prime(x1)) then
          total = total + (x1-1)
        else
          totient = x1
          dummy = x1
          p_index = 1
          do while ((dummy.gt.1).and.(p_index.le.168))
            if ((primes(p_index)*(dummy/primes(p_index))).eq.dummy) then
              totient = totient/primes(p_index)*(primes(p_index)-1)
              do while ((primes(p_index)*(dummy/primes(p_index))).eq.dummy)
                dummy = dummy/primes(p_index)
              enddo
              
              if (is_prime(dummy)) then
                totient = totient/dummy*(dummy-1)
                dummy = 1
              endif
            endif
            p_index = p_index + 1
          enddo
          total = total + totient
        endif
      enddo

      write(*,*) 'The total number of fractions is ',total
      end