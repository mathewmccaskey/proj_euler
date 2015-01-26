c-------------------------------------------------------------------------------------------------c
      program project_euler_71
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is  c
c  called a reduced proper fraction.                                                              c
c                                                                                                 c
c  If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:   c
c                                                                                                 c
c  1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5,      c
c  5/6, 6/7, 7/8                                                                                  c
c                                                                                                 c
c  It can be seen that 2/5 is the fraction immediately to the left of 3/7.                        c
c                                                                                                 c
c  By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size,   c
c  find the numerator of the fraction immediately to the left of 3/7.                             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 nprimes, primes(78498), test_num, max_num, max_denom
      logical valid

c initialize parameters
      max_num = 0
      max_denom = 1

c generaete all the primes from 1 to 1000
      nprimes = 0
      do x1=1,1000000
        if (is_prime(x1)) then
          nprimes = nprimes + 1
          primes(nprimes) = x1
c          write(*,*) nprimes, primes(nprimes)
        endif
      enddo
      write(*,*) 'primes done'
      
c loop over all the potential denominators from 3 to 1000000 (1 and 2 go to 0)
      do x1=1000000, 999997, -1
        
c get the first test_numerator
        test_num = x1*3/7
        valid = .false.
        do while(.not.valid)

c check to see if the test_numerator and denominator don't have any common prime factors
          valid = .true.
          do x2=1,78498
            if (primes(x2).gt.test_num) exit
            
            if (is_multiple(x1,primes(x2)).and.(is_multiple(test_num,primes(x2)))) then
              valid = .false.
              exit
            endif
          enddo

c if the numerator fails then decrement it until it works          
          if (.not.valid) test_num = test_num - 1
        enddo
        
c check to see if this fraction is the largest one
        if (((max_num*x1).lt.(test_num*max_denom)).and.(x1.ne.7)) then
          max_num = test_num
          max_denom = x1
          write(*,*) max_num, max_denom
        endif
      enddo
      
      write(*,*) 'The fraction just to the left of 3/7 is ',max_num,'/',max_denom
      
      end