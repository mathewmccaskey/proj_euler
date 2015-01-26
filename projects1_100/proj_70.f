c-------------------------------------------------------------------------------------------------c
      program project_euler_70
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the   c
c  number of positive numbers less than or equal to n which are relatively prime to n. For        c
c  example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6. c
c  The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.         c
c                                                                                                 c
c  Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.        c
c                                                                                                 c
c  Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n)   c
c  produces a minimum.                                                                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 primes(447), dummy, p_index, totient, x1_digits(1000), x1_ndigits, x1_which(0:9)
      integer*8 tot_digits(1000), tot_ndigits, tot_which(0:9), base, answer
      double precision min_ratio
      logical perm
      
c set up the primes
      p_index = 1
      primes(p_index) = 2
      x1 = 3
      do while(p_index.lt.447)
        if(is_prime(x1)) then
          p_index = p_index + 1
          primes(p_index) = x1
        endif
        x1 = x1 + 2
      enddo 

c initialize the result and base
      min_ratio = 1000000.d0
      answer = 0
      base = 10
      
c the totient function phi(n) = n \prod (1-1/p) for all primes p that factor into n
      do x1=2,10000000
        if (is_prime(x1)) then
          totient = x1-1
        else
          totient = x1
          dummy = x1
          p_index = 1
          do while ((dummy.gt.1).and.(p_index.le.447))
            if ((primes(p_index)*(dummy/primes(p_index))).eq.dummy) then
              totient = totient/primes(p_index)*(primes(p_index)-1)
              do while ((primes(p_index)*(dummy/primes(p_index))).eq.dummy)
                dummy = dummy/primes(p_index)
              enddo
            endif
            p_index = p_index + 1
          enddo
          if (dummy.ne.1) then
            totient = totient/dummy*(dummy-1)
            dummy = 1
          endif
        endif

c Now that we have the totient time to find out if the totient is a permutation of the number itself
        call get_digits(x1,base,x1_digits,x1_ndigits)
        call get_which_digits(x1_digits,x1_ndigits,x1_which)
        call get_digits(totient,base,tot_digits,tot_ndigits)
        call get_which_digits(tot_digits,tot_ndigits,tot_which)
        
        perm = .true.
        do x2=0,9
          if (x1_which(x2).ne.tot_which(x2)) perm = .false.
        enddo
        
        if (perm) then
          if (dble(x1)/dble(totient).lt.min_ratio) then
            write(*,*) 'NEW ANSWER ',x1
            answer = x1
            min_ratio = dble(x1)/dble(totient)
          endif
        endif
      enddo

      write(*,*) 'The number whose totient is a permutation and produces the minimum ratio is ',answer
      end