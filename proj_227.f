c-------------------------------------------------------------------------------------------------c
      program project_euler_227
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  "The Chase" is a game played with two dice and an even number of players.                      c
c                                                                                                 c
c  The players sit around a table; the game begins with two opposite players having one die       c
c  each. On each turn, the two players with a die roll it.                                        c
c  If a player rolls a 1, he passes the die to his neighbour on the left; if he rolls a 6, he     c
c  passes the die to his neighbour on the right; otherwise, he keeps the die for the next turn.   c
c  The game ends when one player has both dice after they have been rolled and passed; that       c
c  player has then lost.                                                                          c
c                                                                                                 c
c  In a game with 100 players, what is the expected number of turns the game lasts?               c
c                                                                                                 c
c  Give your answer rounded to ten significant digits.                                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      double precision transition(0:50,0:50), turns, prob_notend, turns_old, prob(0:50), prob_new(0:50)
      integer*8 n_turns
      logical done
      
c initialize the transition matrix
      do x1=0,50
        prob(x1) = 0.d0
        do x2=0,50
          transition(x1,x2) = 0.d0
        enddo
      enddo
      
      prob(50) = 1.d0
      n_turns = 0
      done = .false.
      turns = 0.d0
      prob_notend = 1.d0

c if we are in state 0 that is the end of the game
      transition(0,0) = 1.d0

c state 1 can go to 0,1,2,and 3
      transition(1,0) = 2.d0/9.d0
      transition(1,1) = 19.d0/36.d0
      transition(1,2) = 2.d0/9.d0
      transition(1,3) = 1.d0/36.d0

c states 2-48      
      do x1=2,48
        transition(x1,x1-2) = 1.d0/36.d0
        transition(x1,x1-1) = 2.0/9.d0
        transition(x1,x1) = 1.d0/2.d0
        transition(x1,x1+1) = 2.d0/9.d0
        transition(x1,x1+2) = 1.d0/36.d0
      enddo

c state 49
      transition(49,47) = 1.d0/36.d0
      transition(49,48) = 2.d0/9.d0
      transition(49,49) = 19.d0/36.d0
      transition(49,50) = 2.d0/9.d0

c state 50
      transition(50,48) = 1.d0/18.d0
      transition(50,49) = 4.d0/9.d0
      transition(50,50) = 1.d0/2.d0

c loop until done
      do while(.not.done)
        n_turns = n_turns + 1
        turns_old = turns
        do x1=0,50
          prob_new(x1) = 0
          do x2=0,50  
            prob_new(x1) = prob_new(x1) + prob(x2)*transition(x2,x1)
          enddo
        enddo
          
c To find the expectation value we use the probability that the game hasn't ended yet and that the game
c can end on this turn
        prob_notend = prob_notend*(1.d0-prob(0))
        turns = turns + n_turns*(prob_new(0)-prob(0))

        do x1=0,50
          prob(x1) = prob_new(x1)
        enddo
        
c        write(*,fmt='(I10,2X,3(ES20.10))') n_turns, prob(0), prob_notend, turns
        
        if (dabs(turns-turns_old)/turns.lt.1.d-15) done = .true.
      enddo
      
      write(*,*) 'The number of expected turns is ',turns
      
      end