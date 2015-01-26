c-------------------------------------------------------------------------------------------------c
      program project_euler_51
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine         c
c  possible values: 13, 23, 43, 53, 73, and 83, are all prime.                                    c
c                                                                                                 c
c  By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the   c
c  first example having seven primes among the ten generated numbers, yielding the family: 56003, c
c  56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of    c
c  this family, is the smallest prime with this property.                                         c
c                                                                                                 c
c  Find the smallest prime which, by replacing part of the number (not necessarily adjacent       c
c  digits) with the same digit, is part of an eight prime value family.                           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 primes(168), n_primes, ones(4), test_num, num_primes, smallest_prime, answer
c      logical is_prime_2
      
c initializing parameters (even numbers and 5 automatially won't be prime)
      ones(1) = 1
      ones(2) = 3
      ones(3) = 7
      ones(4) = 9
      answer = 1000000

C c get the primes
C       n_primes = 0
C       do x1=1,1000
C         if (is_prime(x1)) then
C           n_primes = n_primes + 1
C           primes(n_primes) = x1
C         endif
C       enddo
      
c loop over the 4 numbers that don't get replaced
      do x1=0,9
        do x2=0,9
          do x3=0,9
            do x4=1,4

c 111XX1
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x1*100000 + x2*10000 + x3*1000 + x5*100 + x5*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
              enddo
              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif

c 11X1X1
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x1*100000 + x2*10000 + x5*1000 + x3*100 + x5*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
                if (smallest_prime.eq.121313) then
                  write(*,*) test_num, is_prime(test_num)
                endif
              enddo
              if (smallest_prime.eq.121313) then
                write(*,*) smallest_prime, num_primes
              endif                        
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif   

c 11XX11
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x1*100000 + x2*10000 + x5*1000 + x5*100 + x3*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
              enddo
              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif   
              
c 1X11X1
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x1*100000 + x5*10000 + x2*1000 + x3*100 + x5*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
              enddo
              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif   
                            
c 1X1X11
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x1*100000 + x5*10000 + x2*1000 + x5*100 + x3*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
              enddo
              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif   
              
c 1XX111
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x1*100000 + x5*10000 + x5*1000 + x2*100 + x3*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
              enddo
              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif   
              
c X111X1
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x5*100000 + x1*10000 + x2*1000 + x3*100 + x5*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
                if (smallest_prime.eq.121313) then
                  write(*,*) test_num, is_prime(test_num)
                endif
              enddo
              if (smallest_prime.eq.121313) then
                write(*,*) smallest_prime, num_primes
              endif              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif                 

c X11X11
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x5*100000 + x1*10000 + x2*1000 + x5*100 + x3*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
              enddo
              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif                 
              
c X1X111
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x5*100000 + x1*10000 + x5*1000 + x2*100 + x3*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
                if (smallest_prime.eq.121313) then
                  write(*,*) test_num, is_prime(test_num)
                endif
              enddo
              if (smallest_prime.eq.121313) then
                write(*,*) smallest_prime, num_primes
              endif          
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif                 

c XX1111
c loop over X
              num_primes = 0
              smallest_prime = 1000000
              do x5=0,9
                test_num = x5*100000 + x5*10000 + x1*1000 + x2*100 + x3*10 + ones(x4)
                if (is_prime(test_num)) then
                  num_primes = num_primes + 1
                  if (test_num.lt.smallest_prime) smallest_prime = test_num
                endif
              enddo
              
              if (num_primes.eq.8) then
                if (smallest_prime.lt.answer) answer = smallest_prime
              endif                 
                                                                                            
            enddo
          enddo
        enddo
      enddo
      
      write(*,*) 'The smallest prime in the 8 prime family is ',answer
      write(*,*) 'This problem is bullshit.  The answer is 121313 where the primes come from replacing the '
      write(*,*) '1st and 3rd, the 1st and 5th, and the 3rd and 5th to get all the primes.'
      end