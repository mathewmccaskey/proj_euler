c-------------------------------------------------------------------------------------------------c
      program project_euler_49
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is    c
c  unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit      c
c  numbers are permutations of one another.                                                       c
c                                                                                                 c
c  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this  c
c  property, but there is one other 4-digit increasing sequence.                                  c
c                                                                                                 c
c  What 12-digit number do you form by concatenating the three terms in this sequence?            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 primes(1061), base, big_digit1(1000), big_digit2(1000), big_digit3(1000)
      integer*8 ndigits1, ndigits2, ndigits3, which1(0:9), which2(0:9), which3(0:9)
      logical check_12, check_13

c set the base
      base = 10
      
c generate all the 4 digit primes
      x1 = 0
      x2 = 1001
      do while(x1.lt.1061)
        if (is_prime(x2)) then
          x1 = x1 + 1
          primes(x1) = x2
        endif
        x2 = x2 + 2
      enddo
      
      do x1=1,1061
        do x2=x1+1,1061
          do x3=x2+1,1061
            if ((primes(x3)-primes(x2)).eq.(primes(x2)-primes(x1))) then
              call get_digits(primes(x1),base,big_digit1,ndigits1)
              call get_which_digits(big_digit1,ndigits1,which1)
              call get_digits(primes(x2),base,big_digit2,ndigits2)
              call get_which_digits(big_digit2,ndigits2,which2)
              check_12 = .true.
              do x4=0,9
                if (which1(x4).ne.which2(x4)) check_12 = .false.
              enddo
              
              if (check_12) then
                call get_digits(primes(x3),base,big_digit3,ndigits3)
                call get_which_digits(big_digit3,ndigits3,which3)                
                check_13 = .true.
                do x4=0,9
                  if (which1(x4).ne.which3(x4)) check_13 = .false.
                enddo
                if (check_13) then
                  write(*,*) primes(x1), primes(x2), primes(x3)
                endif
              endif
            endif
          enddo
        enddo
      enddo
      
      end