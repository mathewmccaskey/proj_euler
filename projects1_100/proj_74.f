c-------------------------------------------------------------------------------------------------c
      program project_euler_74
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The number 145 is well known for the property that the sum of the factorial of its digits is   c
c  equal to 145:                                                                                  c
c                                                                                                 c
c  1! + 4! + 5! = 1 + 24 + 120 = 145                                                              c
c                                                                                                 c
c  Perhaps less well known is 169, in that it produces the longest chain of numbers that link     c
c  back to 169; it turns out that there are only three such loops that exist:                     c
c                                                                                                 c
c  169 → 363601 → 1454 → 169                                                                      c
c  871 → 45361 → 871                                                                              c
c  872 → 45362 → 872                                                                              c
c                                                                                                 c
c  It is not difficult to prove that EVERY starting number will eventually get stuck in a loop.   c
c  For example,                                                                                   c
c                                                                                                 c
c  69 → 363600 → 1454 → 169 → 363601 (→ 1454)                                                     c
c  78 → 45360 → 871 → 45361 (→ 871)                                                               c
c  540 → 145 (→ 145)                                                                              c
c                                                                                                 c
c  Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating   c
c  chain with a starting number below one million is sixty terms.                                 c
c                                                                                                 c
c  How many chains, with a starting number below one million, contain exactly sixty non-repeating c
c  terms?                                                                                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 nums(10), dummy(10), n_digits, dummy2(10), fact(0:9,10), chain(61), n_chain, counter, result
      logical unique_chain, found_ndigits
      
c set up the factorials      
      do x1=1,10
        nums(x1) = 0
        do x2=0,9
          fact(x2,x1) = 0
        enddo
      enddo
      
c 0! = 1! = 1, 2! = 2, 3! = 6
      fact(0,1) = 1
      fact(1,1) = 1
      fact(2,1) = 2
      fact(3,1) = 6
c 4! = 24
      fact(4,1) = 4
      fact(4,2) = 2
c 5! = 120
      fact(5,2) = 2
      fact(5,3) = 1
c 6! = 720
      fact(6,2) = 2
      fact(6,3) = 7
c 7! = 5040
      fact(7,2) = 4
      fact(7,4) = 5
c 8! = 40320
      fact(8,2) = 2
      fact(8,3) = 3
      fact(8,5) = 4
c 9! = 362880
      fact(9,2) = 8
      fact(9,3) = 8
      fact(9,4) = 2
      fact(9,5) = 6
      fact(9,6) = 3

      counter = 0
      result = 0
c loop over all the numbers below one million
      do while(counter.ne.1000000)
        
c increment the number
        counter = counter + 1
        nums(1) = nums(1) + 1
        do x1=1,10
          if (nums(x1).eq.10) then
            nums(x1) = nums(x1) - 10
            nums(x1+1) = nums(x1+1) + 1
          endif
          dummy(x1) = nums(x1)
        enddo
        
c initialize the chain
        n_chain = 1
        chain(n_chain) = counter
C         write(*,*) n_chain, counter
        unique_chain = .true.

c loop until we find a non-unique number in the chain
        do while(unique_chain)
          
c initialize the nums
          do x1=1,10
            dummy2(x1) = 0
          enddo

c find the number of digits in dummy variable
          found_ndigits = .false.
          do x1=10,1,-1
            if ((dummy(x1).ne.0).and.(.not.found_ndigits)) then
              found_ndigits = .true.
              n_digits = x1
            endif
          enddo
          
c loop through the digits and add the factorials
          do x1=1,n_digits
            do x2=1,10
              dummy2(x2) = dummy2(x2) + fact(dummy(x1),x2)
            enddo
          enddo
        
c loop over the digits and carry over everything
          do x1=1,10
            do while(dummy2(x1).ge.10)
              dummy2(x1) = dummy2(x1) - 10
              dummy2(x1+1) = dummy2(x1+1) + 1
            enddo
            dummy(x1) = dummy2(x1)
          enddo
        
c add the new number to the chain and check if it's unique
          n_chain = n_chain + 1
          chain(n_chain) = 0
          do x1=1,10
            chain(n_chain) = chain(n_chain) + dummy2(x1)*10**(x1-1)
          enddo
          
          do x1=1,n_chain-1
            if (chain(x1).eq.chain(n_chain)) unique_chain = .false.
          enddo
C           if (counter.eq.871) then
C             write(*,*) n_chain, chain(n_chain)
C           endif
        enddo
        
        if (n_chain.eq.61) then
C           write(*,*) 'number with 60 unique elements is ',counter,n_chain-1
          result = result + 1
        endif
C         write(*,*) counter, n_chain-1
      enddo

      write(*,*) 'The number of chains with 60 elements is ', result
      end