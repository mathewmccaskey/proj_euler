c-------------------------------------------------------------------------------------------------c
      program project_euler_95
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The proper divisors of a number are all the divisors excluding the number itself. For example, c
c  the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, c
c  we call it a perfect number.                                                                   c
c                                                                                                 c
c  Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors  c
c  of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an      c
c  amicable pair.                                                                                 c
c                                                                                                 c
c  Perhaps less well known are longer chains. For example, starting with 12496, we form a chain   c
c  of five numbers:                                                                               c
c                                                                                                 c
c  12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)                                          c
c                                                                                                 c
c  Since this chain returns to its starting point, it is called an amicable chain.                c
c                                                                                                 c
c  Find the smallest member of the longest amicable chain with no element exceeding one million.  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 max_chain, chain, elements(1000), min_member, divisors, num
      logical chain_done, amicable_chain
      
c initialize the chain variables
      max_chain = 0
      chain = 0
      do x1=1,1000
        elements(x1) = 0
      enddo
      
      do x1=1,1000000
        chain = 1
        elements(chain) = x1
        num = x1
C         write(*,*) chain,x1
        
        chain_done = .false.
        amicable_chain = .true.
        do while(.not.chain_done)
          divisors = 1
          do x2=2,int(num**0.5)
            if (((num/x2)*x2).eq.num) then
              divisors = divisors + x2
              if (x2.ne.(num/x2)) then
                divisors = divisors + num/x2
              endif
            endif
          enddo
          chain = chain + 1
          elements(chain) = divisors
          num = divisors
C           write(*,*) chain, divisors
          
c if the divisors loops back to the first element then we have the chain
          if (divisors.eq.elements(1)) then
            chain_done = .true.
          endif
c If an element exceeds one million then we break the chain
          if (divisors.gt.1000000) then
            chain_done = .true.
            amicable_chain = .false.
C             write(*,*) 'Too large an element'
          endif
c If the divisors equals 1 then the number is prime and does not make a amicable chain
          if (divisors.eq.1) then
            chain_done = .true.
            amicable_chain = .false.
C             write(*,*) 'not an amicable chain'
          endif
c If the chain hits either a perfect number or another amicable chain then kill it
          do x2=2,chain-1
            if (divisors.eq.elements(x2)) then
              chain_done = .true.
              amicable_chain = .false.
C               write(*,*) 'chain hit a perfect number or another amicable chain'
            endif
          enddo
        enddo
        
        if (amicable_chain) then
          write(*,*) 'NEW CHAIN :',chain
          write(*,*) (elements(x2), x2=1,chain)
C           read(*,*)
          if (chain.gt.max_chain) then
            max_chain = chain
            min_member = 1000000
            do x2=1,chain
              if (elements(x2).lt.min_member) min_member = elements(x2)
            enddo
          endif
        endif
      enddo
      
      write(*,*) 'The smallest member of the longest amicable chain is ',min_member
      
      end