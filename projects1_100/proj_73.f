c-------------------------------------------------------------------------------------------------c
      program project_euler_71
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
c  It can be seen that there are 3 fractions between 1/3 and 1/2.                                 c
c                                                                                                 c
c  How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for   c
c  d ≤ 12,000?                                                                                    c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 nprimes, primes(1438), n_fractions, test_num
      logical valid

c initializing parameters
      n_fractions = 0

c generaete all the primes from 1 to 1000
      nprimes = 0
      do x1=1,12000
        if (is_prime(x1)) then
          nprimes = nprimes + 1
          primes(nprimes) = x1
c          write(*,*) nprimes, primes(nprimes)
        endif
      enddo
c      write(*,*) 'primes done'

c starting with a denominator of 5 we loop up to 12000
      do x1=5,12000
c      do x1=5,8

c get the first test_numerator without including the numerator for 1/2
        test_num = x1/2
        if (is_multiple(x1,2)) test_num = test_num - 1

c loop until the test_numerator reaches x1/3
        do while(dble(test_num)/dble(x1).gt.(1.d0/3.d0))
          valid = .false.
          do while(.not.valid)

c check to see if the test_numerator and denominator don't have any common prime factors
            valid = .true.
            do x2=1,1438
              if (primes(x2).gt.test_num) exit
            
              if (is_multiple(x1,primes(x2)).and.(is_multiple(test_num,primes(x2)))) then
                valid = .false.
                exit
              endif
            enddo

c if the numerator fails then decrement it until it works          
            if (.not.valid) test_num = test_num - 1
          enddo
          
          if (dble(test_num)/dble(x1).gt.(1.d0/3.d0)) then
            n_fractions = n_fractions + 1
c            write(*,*) test_num, x1
          endif
          test_num = test_num -1
        enddo        
        write(*,*) x1,n_fractions
      enddo
      
c      write(*,*) 'Number of fractions between 1/3 and 1/2 ',n_fractions
      end