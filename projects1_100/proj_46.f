c-------------------------------------------------------------------------------------------------c
      program project_euler_46
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It was proposed by Christian Goldbach that every odd composite number can be written as the    c
c  sum of a prime and twice a square.                                                             c
c                                                                                                 c
c  9 = 7 + 2×1^2                                                                                  c
c  15 = 7 + 2×2^2                                                                                 c
c  21 = 3 + 2×3^2                                                                                 c
c  25 = 7 + 2×3^2                                                                                 c
c  27 = 19 + 2×2^2                                                                                c
c  33 = 31 + 2×1^2                                                                                c
c                                                                                                 c
c  It turns out that the conjecture was false.                                                    c
c                                                                                                 c
c  What is the smallest odd composite that cannot be written as the sum of a prime and twice a    c
c  square?                                                                                        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
      integer*8 primes(1000), squares(1000)
      logical found_result, found_combo
      
c initialize the logical variable
      found_result = .false.

c generate a list of 2*number^2
      do x1=1,1000
        squares(x1) = 2*x1**2
      enddo
      
c generate a list of primes
      x1 = 0
      x2 = 1
      do while(x1.lt.1000)
        x2 = x2 + 1
        if(is_prime(x2)) then
          x1 = x1 + 1
          primes(x1) = x2
        endif
      enddo

c time to find where the conjecture is false
      x1 = 9
      do while(.not.found_result)
        
c check to see if the number is composite (i.e. not prime)
        if(.not.(is_prime(x1))) then
          found_combo = .false.
          do x2=1,1000
            if (squares(x2).lt.x1) then
              do x3=1,1000
                if (squares(x2)+primes(x3).eq.x1) then
                  write(*,*) x1,primes(x3),squares(x2)
                  found_combo = .true.
                endif
              enddo
            endif
          enddo
          
          if (found_combo) then
            x1 = x1 + 2
          else
            found_result = .true.
          endif
        else
          x1 = x1 + 2
        endif
      enddo
      
      write(*,*) x1,' breaks goldbachs conjecture'
      
      end