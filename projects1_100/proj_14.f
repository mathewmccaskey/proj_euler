c-------------------------------------------------------------------------------------------------c
      program project_euler_14
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The following iterative sequence is defined for the set of positive integers:                  c
c                                                                                                 c
c  n → n/2 (n is even)                                                                            c
c  n → 3n + 1 (n is odd)                                                                          c
c                                                                                                 c
c  Using the rule above and starting with 13, we generate the following sequence:                 c
c  13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1                                                     c
c                                                                                                 c
c  It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.       c
c  Although it has not been proved yet (Collatz Problem), it is thought that all starting         c
c  numbers finish at 1.                                                                           c
c                                                                                                 c
c  Which starting number, under one million, produces the longest chain?                          c
c                                                                                                 c
c  NOTE: Once the chain starts the terms are allowed to go above one million.                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 max_chain, max_chain_start, chain, nchain

c initialize the max chain
      max_chain = 0
    
c loop over all the numbers from 1 to a million
      do x1=1,1000000
        nchain = 1
        chain = x1
        do while(chain.gt.1)
          nchain = nchain + 1
          if (is_multiple(chain,2)) then
            chain = chain/2
          else
            chain = 3*chain + 1
          endif
        enddo
        
        if (nchain .gt. max_chain) then
          max_chain = nchain
          max_chain_start = x1
        endif
      enddo
      
      write(*,*) 'Starting with ',max_chain_start
      write(*,*) 'The longest chain is ',max_chain
      
      end