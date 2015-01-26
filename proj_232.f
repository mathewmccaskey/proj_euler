c-------------------------------------------------------------------------------------------------c
      program project_euler_232
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Two players share an unbiased coin and take it in turns to play "The Race". On Player 1's      c
c  turn, he tosses the coin once: if it comes up Heads, he scores one point; if it comes up       c
c  Tails, he scores nothing. On Player 2's turn, she chooses a positive integer T and tosses the  c
c  coin T times: if it comes up all Heads, she scores 2^{T-1} points; otherwise, she scores       c
c  nothing. Player 1 goes first. The winner is the first to 100 or more points.                   c
c                                                                                                 c
c  On each turn Player 2 selects the number, T, of coin tosses that maximises the probability of  c
c  her winning.                                                                                   c
c                                                                                                 c
c  What is the probability that Player 2 wins?                                                    c
c                                                                                                 c
c  Give your answer rounded to eight decimal places in the form 0.abcdefgh.                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      double precision states(0:99,0:99,2), final_states(2), probs(0:99,0:99,2)
      integer*8 T_max_prob(0:99,0:99), T, T_2
      logical done

c initialize the results
      do x1=0,99
        do x2=0,99
          probs(x1,x2,1) = 0.d0
          probs(x1,x2,2) = 0.d0
          T_max_prob(x1,x2) = 0
        enddo
      enddo

c loop over all the combinations of initial states
c this is not a typo, we want to loop over player 2's states first
      do x2=99,0,-1
        do x1=99,0,-1
          
c loop over the possible values of T.  I think it's pretty obvious that we only need to go up to 
c a T that will result in a win for player 2
c          do T=1,int(dlog(dble(100-x2))/dlog(2.d0))+1
          do T=1,10
          
c initialize the state
            do x3=0,99
              do x4=0,99
                states(x3,x4,1) = 0.d0
                states(x3,x4,2) = 0.d0
              enddo
            enddo
            final_states(1) = 0.d0
            final_states(2) = 0.d0
            states(x1,x2,1) = 1.d0
            done = .false.
            
c start computing the markov chain
            do while(.not.done)

c the first move is by player 1, who has a 50/50 chance of just passing the turn to player 2 or advancing 1 point
              states(x1,x2,2) = states(x1,x2,1)/2.d0
              if (x1.eq.99) then
                final_states(1) = final_states(1) + states(x1,x2,1)/2.d0
              else
                states(x1+1,x2,2) = states(x1+1,x2,2) + states(x1,x2,1)/2.d0
              endif

c the second move is by player 2, who has a 1/2^T chance of getting 2^{T-1} points and (2^T-1)/2^T chance of passing the turn
              states(x1,x2,1) = states(x1,x2,2)*(2.d0**T - 1.d0)/2.d0**T
              if ((x2+2**(T-1)).ge.100) then
                final_states(2) = final_states(2) + states(x1,x2,2)/2.d0**T
              else
                states(x1,x2+2**(T-1),1) = states(x1,x2+2**(T-1),1) + states(x1,x2,2)/2.d0**T
              endif
              
              if (states(x1,x2,1).le.1.0d-15) done = .true.
            enddo
            
c once the initial state is depleted take care of the state where x1 advanced one space
c in this state we already calculated the choice of T that gives the best chance of winning
            if ((x1+1).ne.100) then
              T_2 = T_max_prob(x1+1,x2)
              states(x1+1,x2,1) = states(x1+1,x2,1) + states(x1+1,x2,2)*(2.d0**T_2-1)/2.d0**(T_2)
              if ((x2+2**(T_2-1)).ge.100) then
                final_states(2) = final_states(2) + states(x1+1,x2,2)/2.d0**(T_2)
              else
                states(x1+1,x2+2**(T_2-1),1) = states(x1+1,x2,2)/(2.d0**T_2)
              endif
            endif

c once the next move is complete then we use the previous results to see what the probability to win is
            if ((x1+1).lt.100) then
              final_states(1) = final_states(1) + states(x1+1,x2,1)*probs(x1+1,x2,1)
            endif
            if ((x2+2**(T-1)).lt.100) then
              final_states(1) = final_states(1) + states(x1,x2+2**(T-1),1)*probs(x1,x2+2**(T-1),1)
            endif
            if ((x2+2**(T_2-1)).lt.100) then
              final_states(1) = final_states(1)  + states(x1+1,x2+2**(T_2-1),1)*probs(x1+1,x2+2**(T_2-1),1)
            endif

            if ((x1+1).lt.100) then
              final_states(2) = final_states(2) + states(x1+1,x2,1)*probs(x1+1,x2,2)
            endif
            if ((x2+2**(T-1)).lt.100) then
              final_states(2) = final_states(2) + states(x1,x2+2**(T-1),1)*probs(x1,x2+2**(T-1),2)
            endif
            if ((x2+2**(T_2-1)).lt.100) then
              final_states(2) = final_states(2)  + states(x1+1,x2+2**(T_2-1),1)*probs(x1+1,x2+2**(T_2-1),2)
            endif

            if (final_states(2).gt.probs(x1,x2,2)) then
              probs(x1,x2,1) = final_states(1)
              probs(x1,x2,2) = final_states(2)
              T_max_prob(x1,x2) = T
            endif
          enddo
          
          write(*,fmt='(2(I4,1X),3(SE16.10,1X),I4)') x1,x2,probs(x1,x2,1),probs(x1,x2,2),probs(x1,x2,1)+probs(x1,x2,2),
     .        T_max_prob(x1,x2)
c          read(*,*)
        enddo
      enddo
               
      write(*,*) 'The probability for player 2 to win is ',probs(0,0,2)
          
      end