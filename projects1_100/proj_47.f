c-------------------------------------------------------------------------------------------------c
      program project_euler_47
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The first two consecutive numbers to have two distinct prime factors are:                      c
c                                                                                                 c
c  14 = 2 × 7                                                                                     c
c  15 = 3 × 5                                                                                     c
c                                                                                                 c
c  The first three consecutive numbers to have three distinct prime factors are:                  c
c                                                                                                 c
c  644 = 2^2 × 7 × 23                                                                             c
c  645 = 3 × 5 × 43                                                                               c
c  646 = 2 × 17 × 19.                                                                             c
c                                                                                                 c
c  Find the first four consecutive integers to have four distinct prime factors. What is the      c
c  first of these numbers?                                                                        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
      integer*8 num(4), nprimes(4), primefactor(4,4), power(4,4)
      integer*8 dum_primes(4), dum_powers(4)
      integer*8 primes(5000)
      common/primes/ primes
      
      logical found_result, failure(4)

c generate all the primes up to 4700
      x1=1
      primes(1) = 2
      x2=3
      do while(x1.lt.5000)
        if (is_prime(x2)) then
          x1 = x1 + 1
          primes(x1) = x2
        endif
        x2 = x2 + 2
      enddo

c initialize the logical and the starting number 210 = 2*3*5*7 + 4
      found_result = .false.
      num(1) = 213

      do while(.not.found_result)

c check the factors of the first number, if it passes move to number 2
        call quick_find_prime_factors(num(1),nprimes(1),dum_primes,dum_powers,failure(1))
        if (.not.failure(1)) then
          do x1=1,4
            primefactor(1,x1) = dum_primes(x1)
            power(1,x1) = dum_powers(x1)
          enddo

c check the factors of the second number, if it passes move to number 3          
          num(2) = num(1) - 1
          call quick_find_prime_factors(num(2),nprimes(2),dum_primes,dum_powers,failure(2))
          if (.not.failure(2)) then
            do x1=1,4
              primefactor(2,x1) = dum_primes(x1)
              power(2,x1) = dum_powers(x1)
            enddo

c check the factors of the third number, if it passes move to number 4
            num(3) = num(2) - 1
            call quick_find_prime_factors(num(3),nprimes(3),dum_primes,dum_powers,failure(3))
            if (.not.failure(3)) then
              do x1=1,4
                primefactor(3,x1) = dum_primes(x1)
                power(3,x1) = dum_powers(x1)
              enddo

c check the factors of the third number, if it passes do the final check of the factors
c of all the other numbers            
              num(4) = num(3) - 1            
              call quick_find_prime_factors(num(4),nprimes(4),dum_primes,dum_powers,failure(4))
              if (.not.failure(4)) then
                do x1=1,4
                  primefactor(4,x1) = dum_primes(x1)
                  power(4,x1) = dum_powers(x1)
                enddo
            
                found_result = .true.
                do x1=1,4
                  do x2=x1+1,4
                    do x3=1,4
                      do x4=1,4
                        if ((primefactor(x1,x3).eq.primefactor(x2,x4)).and.(power(x1,x3).eq.power(x2,x4))) then
                          found_result = .false.
                        endif
                      enddo
                    enddo
                  enddo
                enddo

c if the 4th number fails then increase the first number by 1
              else
                num(1) = num(1) + 1
              endif
c if the 3rd number foils then increase the first number by 2
            else
              num(1) = num(1) + 2
            endif
c if the 2nd number fails then increase the first number by 3
          else
            num(1) = num(1) + 3
          endif
c if the 1st number fails then increase it by 4
        else
          num(1) = num(1) + 4
        endif
      enddo
      
      write(*,*) 'The first number that satisfies the criteria is ',num(4)
      
      end



c-------------------------------------------------------------------------------------------------c
      subroutine quick_find_prime_factors(num,nprimes,factors,powers,fail)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine is designed to quickly calculate the prime factors of an input number.         c
c  If there are more than four prime factors then it automatically fails since the problem only   c
c  wants 4 unique prime factors.                                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters
      integer*8 num, nprimes, factors(4), powers(4)
      logical fail

c parameters used in this program only
      integer*8 dummy
      
c primes calculated in the main code
      integer*8 primes(5000)
      common/primes/ primes

c initiaiize the fail boolean
      fail = .false.
      
c this subroutine will fail if the number is a prime
      if (.not.is_prime(num)) then
        nprimes = 0
        dummy = num
        x1 = 1
        do while((dummy.gt.1).and.(x1.le.5000))
          x2 = 0
          do while(primes(x1)*(dummy/primes(x1)).eq.dummy)
            x2 = x2 + 1
            dummy = dummy/primes(x1)
          enddo
          if (x2.ne.0) then
            nprimes = nprimes + 1
            factors(nprimes) = primes(x1)
            powers(nprimes) = x2
          endif
          x1 = x1 + 1
          if (nprimes.gt.4) then
            fail = .true.
            return
          endif
        enddo
        if ((nprimes.ne.4).or.(dummy.ne.1)) fail = .true.
      else
        fail = .true.
      endif
        
      return
      end