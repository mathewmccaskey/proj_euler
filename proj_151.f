c-------------------------------------------------------------------------------------------------c
      program project_euler_151
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A printing shop runs 16 batches (jobs) every week and each batch requires a sheet of special   c
c  colour-proofing paper of size A5.                                                              c
c                                                                                                 c
c  Every Monday morning, the foreman opens a new envelope, containing a large sheet of the        c
c  special paper with size A1.                                                                    c
c                                                                                                 c
c  He proceeds to cut it in half, thus getting two sheets of size A2. Then he cuts one of them in c
c  half to get two sheets of size A3 and so on until he obtains the A5-size sheet needed for the  c
c  first batch of the week.                                                                       c
c                                                                                                 c
c  All the unused sheets are placed back in the envelope.                                         c
c                                                                                                 c
c  At the beginning of each subsequent batch, he takes from the envelope one sheet of paper at    c
c  random. If it is of size A5, he uses it. If it is larger, he repeats the 'cut-in-half'         c
c  procedure until he has what he needs and any remaining sheets are always placed back in the    c
c  envelope.                                                                                      c
c                                                                                                 c
c  Excluding the first and last batch of the week, find the expected number of times (during each c
c  week) that the foreman finds a single sheet of paper in the envelope.                          c
c                                                                                                 c
c  Give your answer rounded to six decimal places using the format x.xxxxxx .                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 states(0:85,4), next_moves(0:85,4), num_next_moves(0:85), new_state(4), num_states
      double precision chain(0:85), prob(0:85,4), sums(14), total_prob
      logical unique, match

c initialize variables
      do x1=0,85
        do x2=1,4
          states(x1,x2) = 0
          next_moves(x1,x2) = 0
          prob(x1,x2) = 0.d0
        enddo
        num_next_moves(x1) = 0
        chain(x1) = 0.d0
      enddo

      do x1=1,14
        sums(x1) = 0.d0
      enddo

c generate the states
c the first state has one sheet of A2, A3, A4, and A5
      num_states = 1
      do x1=1,4
        states(1,x1) = 1
      enddo

      do x1=1,85
        do x2=1,4
c if there is a paper there then we will get a new state
          if (states(x1,x2).ne.0) then
            num_next_moves(x1) = num_next_moves(x1) + 1
            prob(x1,num_next_moves(x1)) = dble(states(x1,x2))
            do x3=1,4
              new_state(x3) = states(x1,x3)
            enddo

c if the paper is not A5, cut until you get A5, otherwise use it
            new_state(x2) = new_state(x2) - 1
            do x3=x2+1,4
              new_state(x3) = new_state(x3) + 1
            enddo
            
c check to see if this is a unique state
            unique = .true.
            do x3=1,num_states
              match = .true.
              do x4=1,4
                if (new_state(x4).ne.states(x3,x4)) match = .false.
              enddo
              
              if (match) then
                unique = .false.
                next_moves(x1,num_next_moves(x1)) = x3
              endif
            enddo
            
            if (unique) then
              num_states = num_states + 1
              do x3=1,4
                states(num_states,x3) = new_state(x3)
              enddo
              next_moves(x1,num_next_moves(x1)) = num_states
            endif
          endif
        enddo
        
        total_prob = 0.d0
        do x2=1,num_next_moves(x1)
          total_prob = total_prob + prob(x1,x2)
        enddo
        do x2=1,num_next_moves(x1)
          prob(x1,x2) = prob(x1,x2)/total_prob
        enddo
        
C         write(*,*) 'State: ',x1
C         write(*,fmt='(4(I3,1X))') (states(x1,x2), x2=1,4)
C         write(*,*) 'Next States:'
C         do x2=1,num_next_moves(x1)
C           write(*,fmt='(I3,1X,F12.10,4(I3,1X))') next_moves(x1,x2), prob(x1,x2), (states(next_moves(x1,x2),x3), x3=1,4)
C         enddo
C         read(*,*)
      enddo

c The calculation after generating the states is super quick
      chain(1) = 1.d0
      
      do x1=1,84
        do x2=1,4
          chain(next_moves(x1,x2)) = chain(next_moves(x1,x2)) + chain(x1)*prob(x1,x2)
        enddo
      enddo
      
      do x1=2,5
        sums(1) = sums(1) + chain(x1)
      enddo
      
      do x1=6,11
        sums(2) = sums(2) + chain(x1)
      enddo
      
      do x1=12,21
        sums(3) = sums(3) + chain(x1)
      enddo
      
      do x1=22,30
        sums(4) = sums(4) + chain(x1)
      enddo
      
      do x1=31,41
        sums(5) = sums(5) + chain(x1)
      enddo
      
      do x1=42,50
        sums(6) = sums(6) + chain(x1)
      enddo
      
      do x1=51,60
        sums(7) = sums(7) + chain(x1)
      enddo
      
      do x1=61,66
        sums(8) = sums(8) + chain(x1)
      enddo
      
      do x1=67,72
        sums(9) = sums(9) + chain(x1)
      enddo
      
      do x1=73,76
        sums(10) = sums(10) + chain(x1)
      enddo
      
      do x1=77,80
        sums(11) = sums(11) + chain(x1)
      enddo
      
      sums(12) = chain(81) + chain(82)
      sums(13) = chain(83) + chain(84)
      sums(14) = chain(85)
      
      do x1=1,14
        write(*,*) x1, sums(x1)
      enddo
      
      write(*,*) chain(60) + chain(80) + chain(84)
      write(*,*) chain(0)
      
      end