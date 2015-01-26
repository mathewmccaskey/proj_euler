c-------------------------------------------------------------------------------------------------c
      program project_euler_37
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The number 3797 has an interesting property. Being prime itself, it is possible to             c
c  continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97,  c
c  and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.                         c
c                                                                                                 c
c  Find the sum of the only eleven primes that are both truncatable from left to right and right  c
c  to left.                                                                                       c
c                                                                                                 c
c  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.                              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 ntrunc, number(1000), ndigits, prime_base, prime, base, dummy_num(1000), sum
      logical is_truncatable
      
c initialize the search
      sum = 0
      ntrunc = 0
      base = 10
      ndigits = 2
      do x1=1,ndigits
        number(x1) = 1
      enddo
      
      do while(ntrunc.lt.11)
        is_truncatable = .true.
        
c check the number itself
        call get_number_from_digits(prime_base, base, number, ndigits)
        if (.not.(is_prime(prime_base))) is_truncatable = .false.
        
c truncate from the left
        do x1=ndigits-1,1,-1
          call get_number_from_digits(prime, base, number, x1)
          if (.not.(is_prime(prime))) is_truncatable = .false.
c          write(*,*) prime, is_prime(prime)
        enddo
        
c truncate from the right
        do x1=1,ndigits
          dummy_num(x1) = number(x1)
        enddo
        do x1=ndigits-1,1,-1
          do x2=1,x1
            dummy_num(x2) = dummy_num(x2+1)
          enddo
          call get_number_from_digits(prime, base, dummy_num, x1)
          if (.not.(is_prime(prime))) is_truncatable = .false.
c          write(*,*) prime, is_prime(prime)
        enddo
        
c if the number survives then add it to the totol
        if (is_truncatable) then
          ntrunc = ntrunc + 1
          sum = sum + prime_base
          write(*,*) prime_base
        endif
c        write(*,*) prime_base, is_truncatable
c        read(*,*)
        
        number(1) = number(1) + 2
        do x1=1,ndigits
          if (number(x1).gt.10) then
            number(x1) = number(x1) - 10
            if (number(x1+1).eq.0) then
              number(x1+1) = 1
              ndigits = ndigits + 1
            else
              number(x1+1) = number(x1+1) + 1
            endif
          endif
        enddo
      enddo
      
      write(*,*) 'Sum of all truncatable primes is ',sum
      
      end