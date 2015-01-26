c-------------------------------------------------------------------------------------------------c
      program project_euler_493
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  70 colored balls are placed in an urn, 10 for each of the seven rainbow colors.                c
c                                                                                                 c
c  What is the expected number of distinct colors in 20 randomly picked balls?                    c
c                                                                                                 c
c  Give your answer with nine digits after the decimal point (a.bcdefghij).                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c Parameters used in this program only
      integer states(0:20,1000,7), next_moves(0:20,1000,7), n_states(0:20), test_state(7)
      integer move, state, color, swap
      double precision probs(0:20,1000), total, result
      logical unique, test_unique, bubble_sort, ending_state(0:20,1000)
      
c initialize the number of states
      do x1=0,20
        n_states(x1) = 0
c initialize the ending states and probabilities
        do x2=1,1000
          ending_state(x1,x2) = .false.
          probs(x1,x2) = 0.d0
c initialize the states and the next moves for each state
          do x3=1,7
            states(x1,x2,x3) = 0
            next_moves(x1,x2,x3) = -1
          enddo
        enddo
      enddo
      
c initialize the first state
      n_states(0) = 1
      do x1=1,7
        states(0,1,x1) = 0
      enddo

c Write out the number of states per move for testing purposes
      write(*,*) 'Number of states per move'
      write(*,*)

c loop over the total numebr of moves (i.e. balls taken out of the urn)
      do move=0,20
        
        write(*,*) move, n_states(move)
c        read(*,*)
        
c loop over the number of states given in this move
        do state=1,n_states(move)
        
c loop over the seven colors that can be pulled from the urn
          do color=1,7

c check to make sure that we can actually make the move
            if (states(move,state,color) .lt. 10) then

c check to see if we have an ending board, that way we already know what the "next state" will be (the same ending state)
              if (ending_state(move,state)) then
                
c the ending state is the state with all columns = 9
                do x1=1,7
                  test_state(x1) = 9
                enddo
                
c check to see if the state is unique
                unique = .true.
                do x1=1, n_states(move+1)
                  test_unique = .false.
                  do x2=1,7
                    if (test_state(x2) .ne. states(move+1,x1,x2)) then
                      test_unique = .true.
                    endif
                  enddo
                
                  if (.not.test_unique) then
                    unique = .false.
                    next_moves(move,state,color) = x1
                  endif
                enddo

c If the state survives 
                if (unique) then
                  n_states(move+1) = n_states(move+1) + 1
                  do x1=1,7
                    states(move+1,n_states(move+1),x1) = test_state(x1)
                  enddo
                  next_moves(move,state,color) = n_states(move+1)
                  
                  ending_state(move+1,n_states(move+1)) = .true.
                endif                

c Otherwise just generate the new boards as we usually would
              else
              
c copy over the original state to the test state
                do x1=1,7
                  test_state(x1) = states(move,state,x1)
                enddo
            
c add the new color
                test_state(color) = test_state(color) + 1
              
c bubble sort so that the balls are ordered from highest to lowest
                bubble_sort = .true.
                do while(bubble_sort)
                  bubble_sort = .false.
                  do x1=1,6
                    if (test_state(x1) .lt. test_state(x1+1)) then
                      swap = test_state(x1)
                      test_state(x1) = test_state(x1+1)
                      test_state(x1+1) = swap
                      bubble_sort = .true.
                    endif
                  enddo
                enddo

c If we happen to get a modified state with the seventh column filled then we have an ending state
c We will denote this at a state filled with all columns = 9 (for reasons that will become clear later)
                if (test_state(7).ne.0) then
                  do x1=1,7
                    test_state(x1) = 9
                  enddo
                endif
              
c check to see if the state is unique
                unique = .true.
                do x1=1, n_states(move+1)
                  test_unique = .false.
                  do x2=1,7
                    if (test_state(x2) .ne. states(move+1,x1,x2)) then
                      test_unique = .true.
                    endif
                  enddo
                
                  if (.not.test_unique) then
                    unique = .false.
                    next_moves(move,state,color) = x1
                  endif
                enddo

c If the state survives 
                if (unique) then
                  n_states(move+1) = n_states(move+1) + 1
                  do x1=1,7
                    states(move+1,n_states(move+1),x1) = test_state(x1)
                  enddo
                  next_moves(move,state,color) = n_states(move+1)
                
c                  write(*,fmt='(I3,2X,7(I7,2X))') n_states(move+1), (test_state(x1), x1=1,7)
                
c If we are left with an ending state (all columns = 10) then set the appropriate flag
                  if (test_state(7).ne.0) ending_state(move+1,n_states(move+1)) = .true.
                endif
              endif
            endif

          enddo  
c          write(*,fmt='(7(I7,2X))') (next_moves(move,state,x1), x1=1,7)
c          read(*,*)          
        enddo
      enddo
      
c Now that we've generated all the boards now we can do some probabilities
      probs(0,1) = 1.d0
c      read(*,*)
c      write(*,*) 'Time to calculate some probabilities'
    
c loop over the numebr of moves and the number of states per move
      do move=0,20
        do state=1,n_states(move)
          
c          write(*,*) move,state,probs(move,state)
          if (.not.ending_state(move,state)) then
            total = 70.d0-dble(move)
          
c loop ove the next moves
            do x1=1,7
          
c Here is the calcualtion of the probabilites for the next states
c the probability for the next state is the prob of the current state * (10-color column)/total, that way the more
c balls of a certain color are removed
              probs(move+1,next_moves(move,state,x1)) = probs(move+1,next_moves(move,state,x1)) 
     .            + probs(move,state)*(10.d0 - states(move,state,x1))/total
            enddo
          else
          
c for an ending state we have the total = 7 since there is only 1 ball left of each other in our defined ending state
c this way that when the loop finishes the probability of going from on ending state to the next ending state is 1.
            total = 7.d0
            do x1=1,7
              probs(move+1,next_moves(move,state,x1)) = probs(move+1,next_moves(move,state,x1)) 
     .            + probs(move,state)*(10.d0 - states(move,state,x1))/total
            enddo              
          endif
        enddo
c        read(*,*)
      enddo

c Now that we have the probabilities totted up we can calculate the result
      result = 0.d0
      do x1=1, n_states(20)
        if (ending_state(20,x1)) then
          result = result + probs(20,x1)*7
        else if (states(20,x1,6).ne.0) then
          result = result + probs(20,x1)*6         
        else if (states(20,x1,5).ne.0) then
          result = result + probs(20,x1)*5
        else if (states(20,x1,4).ne.0) then
          result = result + probs(20,x1)*4
        else if (states(20,x1,3).ne.0) then
          result = result + probs(20,x1)*3
        else if (states(20,x1,2).ne.0) then
          result = result + probs(20,x1)*2
        else if (states(20,x1,1).ne.0) then
c We should never get here
          result = result + probs(20,x1)*1
        endif
      enddo

c like a boss      
      write(*,*) "Result : ",result
      
      end