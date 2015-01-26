c-------------------------------------------------------------------------------------------------c
      program project_euler_280
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A laborious ant walks randomly on a 5x5 grid. The walk starts from the central square. At      c
c  each step, the ant moves to an adjacent square at random, without leaving the grid; thus       c
c  there are 2, 3 or 4 possible moves at each step depending on the ant's position.               c
c                                                                                                 c
c  At the start of the walk, a seed is placed on each square of the lower row. When the ant       c
c  isn't carrying a seed and reaches a square of the lower row containing a seed, it will start   c
c  to carry the seed. The ant will drop the seed on the first empty square of the upper row it    c
c  eventually reaches.                                                                            c
c                                                                                                 c
c  What's the expected number of steps until all seeds have been dropped in the top row?          c
c  Give your answer rounded to 6 decimal places.                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only

c main states of the game board
      integer*8 num_boards, index, index_old, index_new, next_board_moves(1000,5), num_next_board_moves(1000)
      integer*8 num_boards_per_move(0:10)
      logical game_boards(1000,0:10), test_board(0:10), unique, board_match

c moves for the individutal states of a game board
      integer*8 possible_move, next_single_moves(25,4), num_next_single_moves(25)
      double precision prob(25,4)

c there are 462 game_boards and 25 states within a single board so here are the variables for a complete state
      double precision states(462,25), next_state(462,25), old_result, new_result
      integer*8 move_number
      
c initialize the game boards
      do x1=1,1000
        do x2=0,10
          game_boards(x1,x2) = .false.
        enddo
        do x2=1,5
          next_board_moves(x1,x2) = -1
        enddo
        num_next_board_moves(x1) = 0
      enddo
c initialize the number of boards per move
      do x1=0,10
        num_boards_per_move(x1) = 0
      enddo

c the 0th element of a game board tells if the ant is carrying a seed or not
c elements 1-5 tell if there is a seed in the 5 squares on the bottom row
c elements 6-10 tell if there is a seed in the 5 squares on the top row
c the game starts with the ant not carrying a seed and a seed on every square in the bottom row
      num_boards = 1
      game_boards(1,0) = .false.
      do x1=1,5
        game_boards(1,x1) = .true.
        game_boards(2,x1+5) = .false.
      enddo
      num_boards_per_move(0) = 1
      
      index_old = 1
      index_new = 1
c loop over the number of transitions
      do x1=1,10
c loop over the number of boards in the previous transition
        do index=index_old,index_new
c loop over the number of potential spots to pick up/drop off a seed
          do x2=1,5

c initialize the test board
            do x3=0,10
              test_board(x3) = game_boards(index,x3)
            enddo

c if the ant already has a seed then check where the seed can be dropped off
            if (game_boards(index,0)) then
              if (.not.game_boards(index,x2+5)) then
                test_board(0) = .false.
                test_board(x2+5) = .true.
              endif

c if the ant doesn't have a seed then check where the seed can be picked up
            else
              if (game_boards(index,x2)) then
                test_board(0) = .true.
                test_board(x2) = .false.
              endif
            endif

c check to see if the board was unique                
            unique = .true.
                
            do x3=1,num_boards
              board_match = .true.
              do x4=0,10
                if (test_board(x4).ne.game_boards(x3,x4)) board_match = .false.
              enddo

c if there is a match then this board isn't unique and then add it to the list of next moves
              if (board_match) then
                unique = .false.
                num_next_board_moves(index) = num_next_board_moves(index) + 1
                next_board_moves(index,num_next_board_moves(index)) = x3
              endif
            enddo
                
c if the board is unique then add it to the number of boards
            if (unique) then
              num_boards = num_boards + 1
              do x3=0,10
                game_boards(num_boards,x3) = test_board(x3)
              enddo

c add the board to the list of next moves                  
              num_next_board_moves(index) = num_next_board_moves(index) + 1
              next_board_moves(index,num_next_board_moves(index)) = num_boards
            endif
          enddo
        enddo

c set the new boundaries for the next group of boards
        index_old = index_new + 1
        index_new = num_boards
        num_boards_per_move(x1) = index_new - index_old + 1
      enddo

      write(*,*) 'Done setting up the main boards'
C       write(*,*) num_boards
C       do x1=0,10
C         write(*,*) x1,num_boards_per_move(x1)
C       enddo
C       
C       open(unit=42,file='output_280.txt')
C       do x1=1,num_boards
C         write(42,fmt='(I4,11(L2,1X))') x1, (game_boards(x1,x2), x2=0,10)
C       enddo
C       close(42)
C       
C       open(unit=42,file='move_list_280.txt')
C       do x1=1,num_boards
C         write(42,fmt='(I4,5(I4,1X))') x1, (next_board_moves(x1,x2), x2=1,num_next_board_moves(x1))
C       enddo
C       close(42)
C       read(*,*)

c setting up the next states and probabilities of an individual board

c initialize the single board variables
      do x1=1,25
        do x2=1,4
          next_single_moves(x1,x2) = -1
        enddo
        num_next_single_moves(x1) = 0
      enddo
      
c top left corner is state 1, bottom right corner is state 25
      do x1=1,25
c up
        possible_move = x1-5
        if ((possible_move.ge.1).and.(possible_move.le.25)) then
          num_next_single_moves(x1) = num_next_single_moves(x1) + 1
          next_single_moves(x1,num_next_single_moves(x1)) = possible_move
        endif

c left
        possible_move = x1-1
        if ((possible_move.ge.1).and.(possible_move.le.25)) then
c can't wrap around to the previous row
          if ((possible_move.ne.5).and.(possible_move.ne.10).and.(possible_move.ne.15).and.(possible_move.ne.20)) then
            num_next_single_moves(x1) = num_next_single_moves(x1) + 1
            next_single_moves(x1,num_next_single_moves(x1)) = possible_move
          endif
        endif

c right
        possible_move = x1+1
        if ((possible_move.ge.1).and.(possible_move.le.25)) then
c can't wrap around to the next row
          if ((possible_move.ne.6).and.(possible_move.ne.11).and.(possible_move.ne.16).and.(possible_move.ne.21)) then
            num_next_single_moves(x1) = num_next_single_moves(x1) + 1
            next_single_moves(x1,num_next_single_moves(x1)) = possible_move
          endif
        endif

c down
        possible_move = x1+5
        if ((possible_move.ge.1).and.(possible_move.le.25)) then
          num_next_single_moves(x1) = num_next_single_moves(x1) + 1
          next_single_moves(x1,num_next_single_moves(x1)) = possible_move
        endif
      enddo

c set up the probabilities
      do x1=1,25
        do x2=1,num_next_single_moves(x1)
          prob(x1,x2) = 1.d0/dble(num_next_single_moves(x1))
        enddo
C         write(*,fmt='(I3,1X,F10.6,1X,4(I3,1X))') x1, prob(x1,1), (next_single_moves(x1,x2), x2=1,num_next_single_moves(x1))
      enddo
      write(*,*) 'Done setting up the individual probabilities'
        
c-------------------------------------------------------------------------------------c
c time for the markov chain
      do x1=1,462
        do x2=1,25
          states(x1,x2) = 0.d0
        enddo
      enddo
c the game starts on the very first game board in square 13
      states(1,13) = 1.d0
      move_number = 0
      new_result = 0.d0
      
c      do while((new_result.gt.0.d0).and.(new_result-old_result).gt.1.0d-6)
      do while(move_number.lt.3000)
        move_number = move_number + 1

c initialize the next state
        do x1=1,462
          do x2=1,25
            next_state(x1,x2) = 0.d0
          enddo
        enddo

c loop over the current state and use the probabilities to fill in the next states
        do x1=1,461
          do x2=1,25
            do x3=1,num_next_single_moves(x2)
              next_state(x1,next_single_moves(x2,x3)) = next_state(x1,next_single_moves(x2,x3)) + states(x1,x2)*prob(x2,x3)
            enddo
          enddo
        enddo
        
c Adjust the next state to take into account switching to a new game board
        do x1=1,461

c if the ant isn't carrying a seed then it could potentially switch game boards on the bottom row (21-25)
          if (.not.game_boards(x1,0)) then
            do x2=21,25
              if (game_boards(x1,x2-20)) then
                next_state(next_board_moves(x1,x2-20),x2) = next_state(next_board_moves(x1,x2-20),x2) + next_state(x1,x2)
                next_state(x1,x2) = 0
              endif
            enddo

c if the ant is carrying a seed then it could potentially switch game boards on the top row (1-5)
          else
            do x2=1,5
              if (.not.game_boards(x1,x2+5)) then
                next_state(next_board_moves(x1,x2),x2) = next_state(next_board_moves(x1,x2),x2) + next_state(x1,x2)
                next_state(x1,x2) = 0
              endif
            enddo
          endif
        enddo

c transfer the new_state to states
        do x1=1,462
          do x2=1,25
            states(x1,x2) = next_state(x1,x2)
          enddo
        enddo

c if the ant reaches the final game board then add the result to the expected number of moves
        old_result = new_result
        do x1=1,5
          new_result = new_result + dble(move_number)*next_state(462,x1)
          next_state(462,x1) = 0
        enddo
        
C         write(*,*) '----------------------------------------------------------'
C         write(*,*) 'move # ',move_number
C         write(*,*) 'board #1'
C         do x1=1,5
C           write(*,fmt='(5(F10.6,1X))') (states(1,x2), x2=5*(x1-1)+1,5*x1)
C         enddo
C         write(*,*)
C         write(*,*) 'board #4'
C         do x1=1,5
C           write(*,fmt='(5(F10.6,1X))') (states(4,x2), x2=5*(x1-1)+1,5*x1)
C         enddo
C         write(*,*)
C         write(*,*) 'board #19'
C         do x1=1,5
C           write(*,fmt='(5(F10.6,1X))') (states(19,x2), x2=5*(x1-1)+1,5*x1)
C         enddo
C         write(*,*)
C         read(*,*)
        write(*,*) move_number, new_result
        
      enddo
      
      write(*,*) 'The expected number of moves is ',new_result
      
      end