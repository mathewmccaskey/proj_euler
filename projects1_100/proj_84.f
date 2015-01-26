c-------------------------------------------------------------------------------------------------c
      program project_euler_84
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  In the game, Monopoly, the standard board is set up in the following way:                      c
c                                                                                                 c
c  GO 	A1 	CC1 	A2 	T1 	R1 	B1 	CH1 	B2 	B3 	JAIL                                              c
c  H2                                      	  	C1                                                c
c  T2                                      	  	U1                                                c
c  H1                                      	  	C2                                                c
c  CH3                                     	  	C3                                                c
c  R4                                      	  	R2                                                c
c  G3                                      	  	D1                                                c
c  CC3                                     	  	CC2                                               c
c  G2                                      	  	D2                                                c
c  G1                                      	  	D3                                                c
c  G2J 	F3 	U2 	  F2 	F1 	R3 	E3 	E2 	CH2 	E1 	FP                                                c
c                                                                                                 c
c  A player starts on the GO square and adds the scores on two 6-sided dice to determine the      c
c  number of squares they advance in a clockwise direction. Without any further rules we would    c
c  expect to visit each square with equal probability: 2.5%. However, landing on G2J (Go To       c
c  Jail), CC (community chest), and CH (chance) changes this distribution.                        c
c                                                                                                 c
c  In addition to G2J, and one card from each of CC and CH, that orders the player to go directly c
c  to jail, if a player rolls three consecutive doubles, they do not advance the result of their  c
c  3rd roll. Instead they proceed directly to jail.                                               c
c                                                                                                 c
c  At the beginning of the game, the CC and CH cards are shuffled. When a player lands on CC or   c
c  CH they take a card from the top of the respective pile and, after following the instructions, c
c  it is returned to the bottom of the pile. There are sixteen cards in each pile, but for the    c
c  purpose of this problem we are only concerned with cards that order a movement; any            c
c  instruction not concerned with movement will be ignored and the player will remain on the      c
c  CC/CH square.                                                                                  c
c                                                                                                 c
c     Community Chest (2/16 cards):                                                               c
c         Advance to GO                                                                           c
c         Go to JAIL                                                                              c
c     Chance (10/16 cards):                                                                       c
c         Advance to GO                                                                           c
c         Go to JAIL                                                                              c
c         Go to C1                                                                                c
c         Go to E3                                                                                c
c         Go to H2                                                                                c
c         Go to R1                                                                                c
c         Go to next R (railway company)                                                          c
c         Go to next R                                                                            c
c         Go to next U (utility company)                                                          c
c         Go back 3 squares.                                                                      c
c                                                                                                 c
c  The heart of this problem concerns the likelihood of visiting a particular square. That is,    c
c  the probability of finishing at that square after a roll. For this reason it should be clear   c
c  that, with the exception of G2J for which the probability of finishing on it is zero, the CH   c
c  squares will have the lowest probabilities, as 5/8 request a movement to another square, and   c
c  it is the final square that the player finishes at on each roll that we are interested in. We  c
c  shall make no distinction between "Just Visiting" and being sent to JAIL, and we shall also    c
c  ignore the rule about requiring a double to "get out of jail", assuming that they pay to get   c
c  out on their next turn.                                                                        c
c                                                                                                 c
c  By starting at GO and numbering the squares sequentially from 00 to 39 we can concatenate      c
c  these two-digit numbers to produce strings that correspond with sets of squares.               c
c                                                                                                 c
c  Statistically it can be shown that the three most popular squares, in order, are JAIL (6.24%)  c
c  = Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square 00. So these three most popular   c
c  squares can be listed with the six-digit modal string: 102400.                                 c
c                                                                                                 c
c  If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal     c
c  string.                                                                                        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      double precision state(0:39), transition(0:39,0:39), new_state(0:39), total
      
c initialize the state and transition matrix
      do x1=0,39
        state(x1) = 2.5d0
        do x2=0,39
          transition(x1,x2) = 0.d0
        enddo
      enddo

c two-six sided dice
C c from state 0 to state 27 we can just straight up automate the transition
C       do x1=0,27
C         transition(x1,x1+2) = 1.d0/36.d0*(1.d0-1.d0/36.d0)
C         transition(x1,x1+3) = 2.d0/36.d0
C         transition(x1,x1+4) = 2.d0/36.d0 + 1.d0/36.d0*(1.d0-1.d0/36.d0)
C         transition(x1,x1+5) = 4.d0/36.d0
C         transition(x1,x1+6) = 4.d0/36.d0 + 1.d0/36.d0*(1.d0-1.d0/36.d0)
C         transition(x1,x1+7) = 6.d0/36.d0
C         transition(x1,x1+8) = 4.d0/36.d0 + 1.d0/36.d0*(1.d0-1.d0/36.d0)
C         transition(x1,x1+9) = 4.d0/36.d0
C         transition(x1,x1+10) = 2.d0/36.d0 + 1.d0/36.d0*(1.d0-1.d0/36.d0)
C         transition(x1,x1+11) = 2.d0/36.d0
C         transition(x1,x1+12) = 1.d0/36.d0*(1.d0-1.d0/36.d0)
C         
C c chance to land in jail via 3 doubles
C         transition(x1,10) = transition(x1,10) + 1.d0/216.d0
C       enddo
C 
C c here are the spaces where you wrap around the board       
C       do x1=28,39
C         do x2=2,39-x1
C           transition(x1,x1+x2) = transition(0,x2)
C         enddo
C         do x2=0,12-(40-x1)
C           transition(x1,x2) = transition(0,(40-x1)+x2)
C         enddo
C 
C         transition(x1,10) = transition(x1,10)
C       enddo

c---------------------------------------------------------------------------
c two-4 sided dice
c from state 0 to state 31 we can just straight up automate the transition
      do x1=0,31
        transition(x1,x1+2) = 1.d0/16.d0*(1.d0-1.d0/16.d0)
        transition(x1,x1+3) = 2.d0/16.d0
        transition(x1,x1+4) = 2.d0/16.d0 + 1.d0/16.d0*(1.d0-1.d0/16.d0)
        transition(x1,x1+5) = 4.d0/16.d0
        transition(x1,x1+6) = 2.d0/16.d0 + 1.d0/16.d0*(1.d0-1.d0/16.d0)
        transition(x1,x1+7) = 2.d0/16.d0
        transition(x1,x1+8) = 1.d0/16.d0*(1.d0-1.d0/16.d0)

c chance to land in jail via 3 doubles
        transition(x1,10) = transition(x1,10) + 1.d0/64.d0
      enddo

c here are the spaces where you wrap around the board       
      do x1=32,39
        do x2=2,39-x1
          transition(x1,x1+x2) = transition(0,x2)
        enddo
        do x2=0,8-(40-x1)
          transition(x1,x2) = transition(0,(40-x1)+x2)
        enddo
        
        transition(x1,10) = transition(x1,10) + 1.d0/64.d0
      enddo

C c check to see if I got it right
C       write(*,fmt='(40(E10.5,1X))') (transition(0,x1), x1=0,39)
C       write(*,fmt='(40(E10.5,1X))') (transition(35,x1), x1=0,39)

c we have some special transitions depending on the final state
c anything that lands on G2J space (30) automatically go to jail (duh)
      do x1=0,39
        transition(x1,10) = transition(x1,10) + transition(x1,30)
        transition(x1,30) = 0.d0
        
c landing on chance fucks up all sorts of shit
c chance space 7
        transition(x1,0) = transition(x1,0) + transition(x1,7)*1.d0/16.d0
        transition(x1,10) = transition(x1,10) + transition(x1,7)*1.d0/16.d0
        transition(x1,11) = transition(x1,11) + transition(x1,7)*1.d0/16.d0
        transition(x1,24) = transition(x1,24) + transition(x1,7)*1.d0/16.d0
        transition(x1,39) = transition(x1,39) + transition(x1,7)*1.d0/16.d0
        transition(x1,5) = transition(x1,5) + transition(x1,7)*1.d0/16.d0
        transition(x1,15) = transition(x1,15) + transition(x1,7)*2.d0/16.d0
        transition(x1,12) = transition(x1,12) + transition(x1,7)*1.d0/16.d0
        transition(x1,4) = transition(x1,4) + transition(x1,7)*1.d0/16.d0
        transition(x1,7) = transition(x1,7)*6.d0/16.d0

c chance space 22
        transition(x1,0) = transition(x1,0) + transition(x1,22)*1.d0/16.d0
        transition(x1,10) = transition(x1,10) + transition(x1,22)*1.d0/16.d0
        transition(x1,11) = transition(x1,11) + transition(x1,22)*1.d0/16.d0
        transition(x1,24) = transition(x1,24) + transition(x1,22)*1.d0/16.d0
        transition(x1,39) = transition(x1,39) + transition(x1,22)*1.d0/16.d0
        transition(x1,5) = transition(x1,5) + transition(x1,22)*1.d0/16.d0
        transition(x1,25) = transition(x1,25) + transition(x1,22)*2.d0/16.d0
        transition(x1,28) = transition(x1,28) + transition(x1,22)*1.d0/16.d0
        transition(x1,19) = transition(x1,19) + transition(x1,22)*1.d0/16.d0
        transition(x1,22) = transition(x1,22)*6.d0/16.d0

c chance space 36
        transition(x1,0) = transition(x1,0) + transition(x1,36)*1.d0/16.d0
        transition(x1,10) = transition(x1,10) + transition(x1,36)*1.d0/16.d0
        transition(x1,11) = transition(x1,11) + transition(x1,36)*1.d0/16.d0
        transition(x1,24) = transition(x1,24) + transition(x1,36)*1.d0/16.d0
        transition(x1,39) = transition(x1,39) + transition(x1,36)*1.d0/16.d0
        transition(x1,5) = transition(x1,5) + transition(x1,36)*3.d0/16.d0
        transition(x1,12) = transition(x1,12) + transition(x1,36)*1.d0/16.d0
        transition(x1,33) = transition(x1,33) + transition(x1,36)*1.d0/16.d0
        transition(x1,36) = transition(x1,36)*6.d0/16.d0
        
c landing on community chest does not fuck up as much shit as chance
c community chest space 2
        transition(x1,0) = transition(x1,0) + transition(x1,2)*1.d0/16.d0
        transition(x1,10) = transition(x1,10) + transition(x1,2)*1.d0/16.d0
        transition(x1,2) = transition(x1,2)*14.d0/16.d0
        
c community chest space 17
        transition(x1,0) = transition(x1,0) + transition(x1,17)*1.d0/16.d0
        transition(x1,10) = transition(x1,10) + transition(x1,17)*1.d0/16.d0
        transition(x1,17) = transition(x1,17)*14.d0/16.d0
        
c community chest space 33
        transition(x1,0) = transition(x1,0) + transition(x1,33)*1.d0/16.d0
        transition(x1,10) = transition(x1,10) + transition(x1,33)*1.d0/16.d0
        transition(x1,33) = transition(x1,33)*14.d0/16.d0
      enddo

C c check to see if the transition matrix is valid
C       do x1=0,39
C         total = 0.d0
C         do x2=0,39
C           total = total + transition(x1,x2)
C         enddo
C         write(*,*) x1,total
C       enddo
C       read(*,*)
      
c now that the transition matrix is set up we can start doing a bunch of transitions and see which
c space is landed on the most
      do x3=1,10000
        do x1=0,39
          new_state(x1) = 0.d0
          do x2=0,39
            new_state(x1) = new_state(x1) + state(x2)*transition(x2,x1)
          enddo
        enddo
        
        do x1=0,39
          state(x1) = new_state(x1)
        enddo
      enddo
      
      do x1=0,39
        write(*,*) x1, state(x1)
      enddo

      end