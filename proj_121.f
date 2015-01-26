c-------------------------------------------------------------------------------------------------c
      program project_euler_121
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A bag contains one red disc and one blue disc. In a game of chance a player takes a disc at    c
c  random and its colour is noted. After each turn the disc is returned to the bag, an extra red  c
c  disc is added, and another disc is taken at random.                                            c
c                                                                                                 c
c  The player pays £1 to play and wins if they have taken more blue discs than red discs at the   c
c  end of the game.                                                                               c
c                                                                                                 c
c  If the game is played for four turns, the probability of a player winning is exactly 11/120,   c
c  and so the maximum prize fund the banker should allocate for winning in this game would be £10 c
c  before they would expect to incur a loss. Note that any payout will be a whole number of       c
c  pounds and also includes the original £1 paid to play the game, so in the example given the    c
c  player actually wins £9.                                                                       c
c                                                                                                 c
c  Find the maximum prize fund that should be allocated to a single game in which fifteen turns   c
c  are played.                                                                                    c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      double precision prob, sub_prob, p_blue(15), p_red(15)
      
c set up the probabilities
      do x1=1,15
        p_blue(x1) = 1.d0/dble(x1+1)
        p_red(x1) = dble(x1)/dble(x1+1)
        write(*,*) x1, p_blue(x1), p_red(x1)
      enddo
      
      prob = 0.d0
c in order to win you need to have more blues than reds, which means you can only have 0-7 reds
c loop over all the combinations of reds

c 7 reds
      do x1=1,15
        do x2=x1+1,15
          if(x1.ne.x2) then
          do x3=x2+1,15
            if((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=x3+1,15
              if((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=x4+1,15
                if((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                do x6=x5+1,15
                  if((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                  do x7=x6+1,15
                    if((x1.ne.x7).and.(x2.ne.x7).and.(x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                      sub_prob = p_red(x1)*p_red(x2)*p_red(x3)*p_red(x4)*p_red(x5)*p_red(x6)*p_red(x7)
      do x8=1,15
        if((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8).and.(x7.ne.x8)) then
          sub_prob = sub_prob*p_blue(x8)
        endif
      enddo
      prob = prob + sub_prob
                    endif
                  enddo
                  endif
                enddo
                endif
              enddo
              endif
            enddo
            endif
          enddo
          endif
        enddo
      enddo
      write(*,*) '7 reds ',prob
      
c 6 reds
      do x1=1,15
        do x2=x1+1,15
          if(x1.ne.x2) then
          do x3=x2+1,15
            if((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=x3+1,15
              if((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=x4+1,15
                if((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                do x6=x5+1,15
                  if((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                    sub_prob = p_red(x1)*p_red(x2)*p_red(x3)*p_red(x4)*p_red(x5)*p_red(x6)
      do x8=1,15
        if((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8)) then
          sub_prob = sub_prob*p_blue(x8)
        endif
      enddo
      prob = prob + sub_prob
                  endif
                enddo
                endif
              enddo
              endif
            enddo
            endif
          enddo
          endif
        enddo
      enddo
      write(*,*) '6-7 reds ',prob
                        
c 5 reds
      do x1=1,15
        do x2=x1+1,15
          if(x1.ne.x2) then
          do x3=x2+1,15
            if((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=x3+1,15
              if((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=x4+1,15
                if((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                  sub_prob = p_red(x1)*p_red(x2)*p_red(x3)*p_red(x4)*p_red(x5)
      do x8=1,15
        if((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8)) then
          sub_prob = sub_prob*p_blue(x8)
        endif
      enddo
      prob = prob + sub_prob
                endif
              enddo
              endif
            enddo
            endif
          enddo
          endif
        enddo
      enddo
      write(*,*) '5-7 reds ',prob

c 4 reds
      do x1=1,15
        do x2=x1+1,15
          if(x1.ne.x2) then
          do x3=x2+1,15
            if((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=x3+1,15
              if((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
                sub_prob = p_red(x1)*p_red(x2)*p_red(x3)*p_red(x4)
      do x8=1,15
        if((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8)) then
          sub_prob = sub_prob*p_blue(x8)
        endif
      enddo
      prob = prob + sub_prob
              endif
            enddo
            endif
          enddo
          endif
        enddo
      enddo
      write(*,*) '4-7 reds ',prob

c 3 reds
      do x1=1,15
        do x2=x1+1,15
          if(x1.ne.x2) then
          do x3=x2+1,15
            if((x1.ne.x3).and.(x2.ne.x3)) then
              sub_prob = p_red(x1)*p_red(x2)*p_red(x3)
      do x8=1,15
        if((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8)) then
          sub_prob = sub_prob*p_blue(x8)
        endif
      enddo
      prob = prob + sub_prob
            endif
          enddo
          endif
        enddo
      enddo
      write(*,*) '3-7 reds ',prob

c 2 reds
      do x1=1,15
        do x2=x1+1,15
          if(x1.ne.x2) then
            sub_prob = p_red(x1)*p_red(x2)
      do x8=1,15
        if((x1.ne.x8).and.(x2.ne.x8)) then
          sub_prob = sub_prob*p_blue(x8)
        endif
      enddo
      prob = prob + sub_prob
          endif
        enddo
      enddo
      write(*,*) '2-7 reds ',prob

c 1 red
      do x1=1,15
        sub_prob = p_red(x1)
        do x8=1,15
          if(x1.ne.x8) then
            sub_prob = sub_prob*p_blue(x8)
          endif
        enddo
        prob = prob + sub_prob
      enddo
      write(*,*) '1-7 reds ',prob

c 0 reds
      sub_prob = 1.d0
      do x1=1,15
        sub_prob = sub_prob*p_blue(x1)
      enddo
      prob = prob + sub_prob
      
      write(*,*) 'The probability to win is ',prob
      write(*,*) 'The amount of money reserved as a prize is ',1.d0/prob
      
      end