c-------------------------------------------------------------------------------------------------c
      program project_euler_35
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The number, 197, is called a circular prime because all rotations of the digits: 197, 971,     c
c  and 719, are themselves prime.                                                                 c
c                                                                                                 c
c  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.  c
c                                                                                                 c
c  How many circular primes are there below one million?                                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 nprimes, number(1000), ndigits, prime, base, dummy
      logical is_circular
      
c initialize the number of circular primes and the number
      nprimes = 4
      base = 10
      ndigits = 2
      do x1=1,2
        number(x1) = 1
      enddo
      call get_number_from_digits(prime, base, number, ndigits)
      
      do while(prime.lt.1000000)
        is_circular = .true.

c loop over all the rotations        
        do x1=1,ndigits

c rotate the digits
          dummy = number(1)
          do x2=1,ndigits-1
            number(x2) = number(x2+1)
          enddo
          number(ndigits) = dummy
          
          call get_number_from_digits(prime, base, number, ndigits)
          if (.not.(is_prime(prime))) is_circular = .false.
        enddo
        
        if (is_circular) nprimes = nprimes + 1
c        write(*,*) prime, is_circular
c        read(*,*)
        
c since all rotations must be prime none of the digits can be even
        number(1) = number(1) + 2
        do x1=1,ndigits
          if (number(x1).gt.10) then
            number(x1) = number(x1) - 10
            if (number(x1+1).eq.0) then
              number(x1+1) = 1
              ndigits = ndigits + 1
            else
              number(x1+1) = number(x1+1) + 2
            endif
          endif
        enddo
        
        call get_number_from_digits(prime, base, number, ndigits)
      enddo
      
      write(*,*) 'The number of circular primes is ',nprimes
      
      end