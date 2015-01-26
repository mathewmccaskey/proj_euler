c-------------------------------------------------------------------------------------------------c
      program project_euler_243
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A positive fraction whose numerator is less than its denominator is called a proper fraction.  c
c  For any denominator, d, there will be d−1 proper fractions; for example, with d = 12:          c
c  1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .                 c
c                                                                                                 c
c  We shall call a fraction that cannot be cancelled down a resilient fraction.                   c
c  Furthermore we shall define the resilience of a denominator, R(d), to be the ratio of its      c
c  proper fractions that are resilient; for example, R(12) = 4/11 .                               c
c  In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10 .                  c
c                                                                                                 c
c  Find the smallest denominator d, having a resilience R(d) < 15499/94744 .                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 primes(1229), dummy, p_index, totient
      logical found_result
      
c set up the primes
      p_index = 1
      primes(p_index) = 2
      x1 = 3
      do while(p_index.lt.1229)
        if(is_prime(x1)) then
          p_index = p_index + 1
          primes(p_index) = x1
        endif
        x1 = x1 + 2
      enddo 
      
c the totient function phi(n) = n \prod (1-1/p) for all primes p that factor into n
      found_result = .false.
c since we want a minimum resilience we want a number that is divided by many primes
c se we start with x1 = 2*3*5*7*11*13*17*19 and use that as the incrementing value
      x1 = 9699690
      do while(.not.found_result)
C        if (is_prime(x1)) then
C          totient = x1-1
C        else
          totient = 1
          dummy = x1
          do x2 = 1,1229
            if ((primes(x2)*(dummy/primes(x2))).eq.dummy) then
              totient = totient*(primes(x2)-1)
              dummy = dummy/primes(x2)
            endif
          enddo
          totient = dummy*totient
C        endif

        write(*,*) x1,totient,dble(totient)/dble(x1-1)

c the ratio of resilient functions is the totient/(x1-1)
c check to see if the ratio is small enough
C         if ((totient*10).lt.((x1-1)*4)) then
        if ((dble(totient)*dble(94744)).lt.(dble(x1-1)*dble(15499))) then
          found_result = .true.
          write(*,*) 'The smallest number with the right resilience is ',x1
        endif
        x1 = x1 + 9699690
      enddo
      
      end
      