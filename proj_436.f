c-------------------------------------------------------------------------------------------------c
      program project_euler_436
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Julie proposes the following wager to her sister Louise.                                       c
c  She suggests they play a game of chance to determine who will wash the dishes.                 c
c  For this game, they shall use a generator of independent random numbers uniformly distributed  c
c  between 0 and 1.                                                                               c
c  The game starts with S = 0.                                                                    c
c  The first player, Louise, adds to S different random numbers from the generator until S > 1    c
c  and records her last random number 'x'.                                                        c
c  The second player, Julie, continues adding to S different random numbers from the generator    c
c  until S > 2 and records her last random number 'y'.                                            c
c  The player with the highest number wins and the loser washes the dishes, i.e. if y > x the     c
c  second player wins.                                                                            c
c                                                                                                 c
c  For example, if the first player draws 0.62 and 0.44, the first player turn ends since         c
c  0.62+0.44 > 1 and x = 0.44.                                                                    c
c  If the second players draws 0.1, 0.27 and 0.91, the second player turn ends since              c
c  0.62+0.44+0.1+0.27+0.91 > 2 and y = 0.91. Since y > x, the second player wins.                 c
c                                                                                                 c
c  Louise thinks about it for a second, and objects: "That's not fair".                           c
c  What is the probability that the second player wins?                                           c
c  Give your answer rounded to 10 places behind the decimal point in the form 0.abcdefghij        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c           71450000000  0.527666496501050       0.527666479759239    
c parameters used in this program only
      integer*8 winx, winy, total, idum
      double precision S, x, y, rnum, random, prob, prob_old
      
c initialize
      idum = -43
      winx = 0
      winy = 4749018071
      total = 9000000000
      prob = 1.d0
      prob_old = 0.d0
      
      do while(dabs(prob-prob_old).gt.1.0d-12)
        prob_old = prob
        
c loop over a certain number of games
        do x1=1,10000000
          S=0
          do while(S.lt.1.d0)
            rnum = random(idum)
            S = S + rnum
c            write(*,*) rnum,S
          enddo
          x = rnum
c          write(*,*) 'x = ',x
          do while(S.lt.2.d0)
            rnum = random(idum)
            S = S + rnum
c            write(*,*) rnum,S
          enddo
          y = rnum
c          write(*,*) 'y = ',y
          if (y.gt.x) then
            winy = winy + 1
          endif
          total = total + 1
        enddo
        
        prob = dble(winy)/dble(total)
        write(*,*) total, prob, prob_old
      enddo
      
      write(*,*) 'num wins: ',winy
      write(*,*) 'The probability that the second player wins is ',prob
      
      end