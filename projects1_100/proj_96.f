c-------------------------------------------------------------------------------------------------c
      program project_euler_96
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Su Doku (Japanese meaning number place) is the name given to a popular puzzle concept. Its     c
c  origin is unclear, but credit must be attributed to Leonhard Euler who invented a similar,     c
c  and much more difficult, puzzle idea called Latin Squares. The objective of Su Doku puzzles,   c
c  however, is to replace the blanks (or zeros) in a 9 by 9 grid in such that each row, column,   c
c  and 3 by 3 box contains each of the digits 1 to 9. Below is an example of a typical starting   c
c  puzzle grid and its solution grid.                                                             c
c                                                                                                 c
c  003 020 600            483 921 657                                                             c
c  900 305 001            967 345 821                                                             c
c  001 806 400            251 876 493                                                             c
c                                                                                                 c
c  008 102 900            548 132 976                                                             c
c  700 000 008            729 564 138                                                             c
c  006 708 200            136 798 245                                                             c
c                                                                                                 c
c  002 609 500            372 689 514                                                             c
c  800 203 009            814 253 769                                                             c
c  005 010 300            695 417 382                                                             c
c                                                                                                 c
c  A well constructed Su Doku puzzle has a unique solution and can be solved by logic, although   c
c  it may be necessary to employ "guess and test" methods in order to eliminate options (there    c
c  is much contested opinion over this). The complexity of the search determines the difficulty   c
c  of the puzzle; the example above is considered easy because it can be solved by straight       c
c  forward direct deduction.                                                                      c
c                                                                                                 c
c  The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'), contains fifty        c
c  different Su Doku puzzles ranging in difficulty, but all with unique solutions (the first      c
c  puzzle in the file is the example above).                                                      c
c                                                                                                 c
c  By solving all fifty puzzles find the sum of the 3-digit numbers found in the top left corner  c
c  of each solution grid; for example, 483 is the 3-digit number found in the top left corner of  c
c  the solution grid above.                                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 result, grid(9,9), num_grids, squares(9,9), counters(9), square_row(9,9), square_col(9,9)
      integer num_options, pos_option, options_row(9), options_col(9), num_zeros, last_num_zeros, num_trues, last_num_trues
      integer num_choices(9), location(9,9)
      logical lgrid(9,9,9), done, is_option_1, is_not_option_2
      
c initializing parameters
      do x1=1,9
        counters(x1) = 0
      enddo
      
      result = 0
      do x1=1,9
        do x2=1,9
          if ((x1.le.3).and.(x2.le.3)) squares(x1,x2) = 1
          if ((x1.le.3).and.((x2.gt.3).and.(x3.le.6))) squares(x1,x2) = 2
          if ((x1.le.3).and.((x2.gt.6).and.(x3.le.9))) squares(x1,x2) = 3
          if (((x1.gt.3).and.(x1.le.6)).and.(x2.le.3)) squares(x1,x2) = 4
          if (((x1.gt.3).and.(x1.le.6)).and.((x2.gt.3).and.(x3.le.6))) squares(x1,x2) = 5
          if (((x1.gt.3).and.(x1.le.6)).and.((x2.gt.6).and.(x3.le.9))) squares(x1,x2) = 6
          if (((x1.gt.6).and.(x1.le.9)).and.(x2.le.3)) squares(x1,x2) = 7
          if (((x1.gt.6).and.(x1.le.9)).and.((x2.gt.3).and.(x3.le.6))) squares(x1,x2) = 8
          if (((x1.gt.6).and.(x1.le.9)).and.((x2.gt.6).and.(x3.le.9))) squares(x1,x2) = 9
          
          counters(squares(x1,x2)) = counters(squares(x1,x2)) + 1
          square_row(squares(x1,x2),counters(squares(x1,x2))) = x1
          square_col(squares(x1,x2),counters(squares(x1,x2))) = x2
        enddo
      enddo
        
      
Cc open the input file
C      open(unit=42, file='input_96.txt')

Cc loop over all the sudoku grids      
C      do num_grids=1,50
        num_zeros = 81
        num_trues = 729
        
Cc read in the grid
C        read(42,*)
C        do x1=1,9
C          read(42,fmt='(9(I1))') (grid(x1,x2), x2=1,9)
Cc          write(*,fmt='(9(I2))') (grid(x1,x2), x2=1,9)
C        enddo

c enter in the grid manually
        write(*,*) 'Enter grid'
        do x1=1,9
          read(*,fmt='(9(I1))') (grid(x1,x2), x2=1,9)
        enddo

c initialize the logical grid
        do x1=1,9
          do x2=1,9
            do x3=1,9
              lgrid(x1,x2,x3) = .true.
            enddo
          enddo
        enddo

c loop over the grid to set the logical grid
        do x1=1,9
          do x2=1,9
            if (grid(x1,x2).ne.0) then
              do x3=1,9
c set the lgrid for this non-zero point
                lgrid(x1,x2,x3) = .false.
                
c adjust the lgrid in the columnm, row, and 3x3 square
                lgrid(x3,x2,grid(x1,x2)) = .false.
                lgrid(x1,x3,grid(x1,x2)) = .false.
                lgrid(square_row(squares(x1,x2),x3), square_col(squares(x1,x2),x3), grid(x1,x2)) = .false.        
              enddo
            endif
          enddo
        enddo

        done = .false.
        do while(.not.done)
          last_num_zeros = num_zeros
          last_num_trues = num_trues
          done = .true.
          
c loop over each individual cell and if there is only one option set it
          do x1=1,9
            do x2=1,9
              num_options = 0
              do x3=1,9
                if (lgrid(x1,x2,x3)) then
                  num_options = num_options + 1
                  pos_option = x3
                endif
              enddo
              
              if (num_options.eq.1) then
                grid(x1,x2) = pos_option
                lgrid(x1,x2,pos_option) = .false.
                do x3=1,9
                  lgrid(x3,x2,grid(x1,x2)) = .false.
                  lgrid(x1,x3,grid(x1,x2)) = .false.
                  lgrid(square_row(squares(x1,x2),x3), square_col(squares(x1,x2),x3), grid(x1,x2)) = .false.
                enddo                  
              endif
            enddo
          enddo
          
c x1 loops over each number
          do x1=1,9

c loop over each row            
            do x2=1,9
c if there is only one option in the row set it in the grids
              num_options = 0
              do x3=1,9
                if (lgrid(x2,x3,x1)) then
                  num_options = num_options + 1
                  pos_option = x3
                endif
              enddo              
              if (num_options.eq.1) then
                grid(x2,pos_option) = x1
                do x3=1,9
                  lgrid(x2,pos_option,x3) = .false.
                  lgrid(x3,pos_option,x1) = .false.
                  lgrid(square_row(squares(x2,pos_option),x3), square_col(squares(x2,pos_option),x3), x1) = .false.
                enddo
              endif

c for each row check if see if the only options are the intersections of a row and a 3x3 grid
c group 1
              is_option_1 = .false.
              do x3=1,3
                if (lgrid(x2,x3,x1)) is_option_1 = .true.
              enddo
              if (is_option_1) then
                is_not_option_2 = .true.
                do x3=4,9
                  if (lgrid(x2,x3,x1)) is_not_option_2 = .false.
                enddo
                if (is_not_option_2) then
                  do x3=1,9
                    if (square_row(squares(x2,1),x3).ne.x2) then
                      lgrid(square_row(squares(x2,1),x3),square_col(squares(x2,1),x3),x1) = .false.
                    endif
                  enddo
                endif
              endif
c group 2              
              is_option_1 = .false.
              do x3=4,6
                if (lgrid(x2,x3,x1)) is_option_1 = .true.
              enddo
              if (is_option_1) then
                is_not_option_2 = .true.
                do x3=1,3
                  if (lgrid(x2,x3,x1)) is_not_option_2 = .false.
                enddo
                do x3=7,9
                  if (lgrid(x2,x3,x1)) is_not_option_2 = .false.
                enddo
                if (is_not_option_2) then
                  do x3=1,9
                    if (square_row(squares(x2,4),x3).ne.x2) then
                      lgrid(square_row(squares(x2,4),x3),square_col(squares(x2,4),x3),x1) = .false.
                    endif
                  enddo
                endif
              endif
c group 3
              is_option_1 = .false.
              do x3=7,9
                if (lgrid(x2,x3,x1)) is_option_1 = .true.
              enddo
              if (is_option_1) then
                is_not_option_2 = .true.
                do x3=1,6
                  if (lgrid(x2,x3,x1)) is_not_option_2 = .false.
                enddo
                if (is_not_option_2) then
                  do x3=1,9
                    if (square_row(squares(x2,7),x3).ne.x2) then
                      lgrid(square_row(squares(x2,7),x3),square_col(squares(x2,7),x3),x1) = .false.
                    endif
                  enddo
                endif
              endif
            enddo

c loop over each column
            do x2=1,9
c if there is only one option in the column set it in the grids
              num_options = 0
              do x3=1,9
                if (lgrid(x3,x2,x1)) then
                  num_options = num_options + 1
                  pos_option = x3
                endif
              enddo
              if (num_options.eq.1) then
                grid(pos_option,x2) = x1
                do x3=1,9
                  lgrid(pos_option,x2,x3) = .false.
                  lgrid(pos_option,x3,x1) = .false.
                  lgrid(square_row(squares(pos_option,x2),x3), square_col(squares(pos_option,x2),x3), x1) = .false.
                enddo
              endif
              
c for each column check if see if the only options are the intersections of a column and a 3x3 grid
c group 1
              is_option_1 = .false.
              do x3=1,3
                if (lgrid(x3,x2,x1)) is_option_1 = .true.
              enddo
              if (is_option_1) then
                is_not_option_2 = .true.
                do x3=4,9
                  if (lgrid(x3,x2,x1)) is_not_option_2 = .false.
                enddo
                if (is_not_option_2) then
                  do x3=1,9
                    if (square_col(squares(1,x2),x3).ne.x2) then
                      lgrid(square_row(squares(1,x2),x3),square_col(squares(1,x2),x3),x1) = .false.
                    endif
                  enddo
                endif
              endif
c group 2
              is_option_1 = .false.
              do x3=4,6
                if (lgrid(x3,x2,x1)) is_option_1 = .true.
              enddo
              if (is_option_1) then
                is_not_option_2 = .true.
                do x3=1,3
                  if (lgrid(x3,x2,x1)) is_not_option_2 = .false.
                enddo
                do x3=7,9
                  if (lgrid(x3,x2,x1)) is_not_option_2 = .false.
                enddo
                if (is_not_option_2) then
                  do x3=1,9
                    if (square_col(squares(4,x2),x3).ne.x2) then
                      lgrid(square_row(squares(4,x2),x3),square_col(squares(4,x2),x3),x1) = .false.
                    endif
                  enddo
                endif
              endif
c group 3
              is_option_1 = .false.
              do x3=7,9
                if (lgrid(x3,x2,x1)) is_option_1 = .true.
              enddo
              if (is_option_1) then
                is_not_option_2 = .true.
                do x3=1,6
                  if (lgrid(x3,x2,x1)) is_not_option_2 = .false.
                enddo
                if (is_not_option_2) then
                  do x3=1,9
                    if (square_col(squares(7,x2),x3).ne.x2) then
                      lgrid(square_row(squares(7,x2),x3),square_col(squares(7,x2),x3),x1) = .false.
                    endif
                  enddo
                endif
              endif       
            enddo

c loop over the 3x3 squares
            do x2=1,9
c if there is only one option in the 3x3 square then set it in the grids
              num_options = 0
              do x3=1,9
                if (lgrid(square_row(x2,x3), square_col(x2,x3), x1)) then
                  num_options = num_options + 1
                  pos_option = x3
                  options_row(num_options) = square_row(x2,x3)
                  options_col(num_options) = square_col(x2,x3)
                endif
              enddo
              if (num_options.eq.1) then
                grid(square_row(x2,pos_option), square_col(x2,pos_option)) = x1
                do x3=1,9
                  lgrid(square_row(x2,pos_option), square_col(x2,pos_option), x3) = .false.
                  lgrid(square_row(x2,pos_option),x3,x1) = .false.
                  lgrid(x3,square_col(x2,pos_option),x1) = .false.
                enddo
                
c if there are more than one options then check if the options are in the same row or column
              else if (num_options.eq.2) then
c two options in the same row
                if (options_row(1).eq.options_row(2)) then
                  do x3=1,9
                    if ((x3.ne.options_col(1)).and.(x3.ne.options_col(2))) lgrid(options_row(1),x3,x1) = .false.
                  enddo
                endif
c two options in the same column
                if (options_col(1).eq.options_col(2)) then
                  do x3=1,9
                    if ((x3.ne.options_row(1)).and.(x3.ne.options_row(2))) lgrid(x3,options_col(1),x1) = .false.
                  enddo
                endif
              
              else if (num_options.eq.3) then
c three options in the same row
                if ((options_row(1).eq.options_row(2)).and.(options_row(1).eq.options_row(3))) then
                  do x3=1,9
                    if ((x3.ne.options_col(1)).and.(x3.ne.options_col(2)).and.(x3.ne.options_col(3))) then
                      lgrid(options_row(1),x3,x1) = .false.
                    endif
                  enddo
                endif
c three options in the same column
                if ((options_col(1).eq.options_col(2)).and.(options_col(1).eq.options_col(3))) then
                  do x3=1,9
                    if ((x3.ne.options_row(1)).and.(x3.ne.options_row(2)).and.(x3.ne.options_row(3))) then
                      lgrid(x3,options_col(1),x1) = .false.
                    endif
                  enddo
                endif
              endif
            enddo
          enddo  
          
c loop over the numbers and check if there is an x-wing over two rows
c note: we don't need to do analogous code for the columns since this is looking for a box
          do x1=1,9
c initialize the parameters used for the x-wings
            do x2=1,9
              num_choices(x2) = 0
              do x3=1,9
                location(x2,x3) = 0
              enddo
            enddo
            
            do x2=1,9
              do x3=1,9
                if (lgrid(x2,x3,x1)) then
                  num_choices(x2) = num_choices(x2) + 1
                  location(x2,num_choices(x2)) = x3
                endif
              enddo
            enddo
c check to see if a pair of rows have the same number in the same column            
            do x2=1,9
              if (num_choices(x2).eq.2) then
                do x3=x2+1,9
                  if (num_choices(x3).eq.2) then
                    if ((location(x2,1).eq.location(x3,1)).and.(location(x2,2).eq.location(x3,2))) then
                      do x4=1,9
                        if ((x4.ne.x2).and.(x4.ne.x3)) then
                          lgrid(x4,location(x2,1),x1) = .false.
                          lgrid(x4,location(x2,2),x1) = .false.
                        endif
                      enddo
                    endif
                  endif
                enddo
              endif
            enddo
          enddo

c loop over each row and see if there is a visible pair of options
          do x1=1,9
c initialize the parameters
            do x2=1,9
              num_choices(x2) = 0
              do x3=1,9
                location(x2,x3) = 0
              enddo
            enddo
c loop over each column and number to tally the numbers that are possible            
            do x2=1,9
              do x3=1,9
                if (lgrid(x1,x2,x3)) then
                  num_choices(x2) = num_choices(x2) + 1
                  location(x2,num_choices(x2)) = x3
                endif
              enddo
            enddo
c check if there is a pair of columns with the same two options
            do x2=1,9
              if (num_choices(x2).eq.2) then
                do x3=x2+1,9
                  if (num_choices(x3).eq.2) then
                    if ((location(x2,1).eq.location(x3,1)).and.(location(x2,2).eq.location(x3,2))) then
                      do x4=1,9
                        if ((x4.ne.x2).and.(x4.ne.x3)) then
                          lgrid(x1,x4,location(x2,1)) = .false.
                          lgrid(x1,x4,location(x2,2)) = .false.
                        endif
                      enddo
                    endif
                  endif
                enddo
              endif
            enddo
          enddo

c loop over each row and see if there is an invisible pair of options
          do x1=1,9
c initialize the parameters
            do x2=1,9
              num_choices(x2) = 0
              do x3=1,9
                location(x2,x3) = 0
              enddo
            enddo
c loop over each number to tally the columns that are possible            
            do x2=1,9
              do x3=1,9
                if (lgrid(x1,x3,x2)) then
                  num_choices(x2) = num_choices(x2) + 1
                  location(x2,num_choices(x2)) = x3
                endif
              enddo
            enddo
c check if there is a pair of numbers with the same two options for columns
            do x2=1,9
              if (num_choices(x2).eq.2) then
                do x3=x2+1,9
                  if (num_choices(x3).eq.2) then
                    if ((location(x2,1).eq.location(x3,1)).and.(location(x2,2).eq.location(x3,2))) then
                      do x4=1,9
                        if ((x4.ne.x2).and.(x4.ne.x3)) then
                          lgrid(x1,location(x2,1),x4) = .false.
                          lgrid(x1,location(x2,2),x4) = .false.
                        endif
                      enddo
                    endif
                  endif
                enddo
              endif
            enddo
          enddo                  



c loop over each column and see if there is a visible pair of options
          do x1=1,9
c initialize the parameters
            do x2=1,9
              num_choices(x2) = 0
              do x3=1,9
                location(x2,x3) = 0
              enddo
            enddo
c loop over each row and number to tally the numbers that are possible            
            do x2=1,9
              do x3=1,9
                if (lgrid(x2,x1,x3)) then
                  num_choices(x2) = num_choices(x2) + 1
                  location(x2,num_choices(x2)) = x3
                endif
              enddo
            enddo
c check if there is a pair of columns with the same two options
            do x2=1,9
              if (num_choices(x2).eq.2) then
                do x3=x2+1,9
                  if (num_choices(x3).eq.2) then
                    if ((location(x2,1).eq.location(x3,1)).and.(location(x2,2).eq.location(x3,2))) then
                      do x4=1,9
                        if ((x4.ne.x2).and.(x4.ne.x3)) then
                          lgrid(x4,x1,location(x2,1)) = .false.
                          lgrid(x4,x1,location(x2,2)) = .false.
                        endif
                      enddo
                    endif
                  endif
                enddo
              endif
            enddo
          enddo

c loop over each column and see if there is an invisible pair of options
          do x1=1,9
c initialize the parameters
            do x2=1,9
              num_choices(x2) = 0
              do x3=1,9
                location(x2,x3) = 0
              enddo
            enddo
c loop over each number to tally the rows that are possible            
            do x2=1,9
              do x3=1,9
                if (lgrid(x3,x1,x2)) then
                  num_choices(x2) = num_choices(x2) + 1
                  location(x2,num_choices(x2)) = x3
                endif
              enddo
            enddo
c check if there is a pair of numbers with the same two options for columns
            do x2=1,9
              if (num_choices(x2).eq.2) then
                do x3=x2+1,9
                  if (num_choices(x3).eq.2) then
                    if ((location(x2,1).eq.location(x3,1)).and.(location(x2,2).eq.location(x3,2))) then
                      do x4=1,9
                        if ((x4.ne.x2).and.(x4.ne.x3)) then
                          lgrid(location(x2,1),x1,x4) = .false.
                          lgrid(location(x2,2),x1,x4) = .false.
                        endif
                      enddo
                    endif
                  endif
                enddo
              endif
            enddo
          enddo                 

c loop over each 3x3 square and see if there is a visible pair of options
          do x1=1,9
c initialize the parameters
            do x2=1,9
              num_choices(x2) = 0
              do x3=1,9
                location(x2,x3) = 0
              enddo
            enddo
c loop over each spot in the 3x3 square and number to tally the numbers that are possible            
            do x2=1,9
              do x3=1,9
                if (lgrid(square_row(x1,x2),square_col(x1,x2),x3)) then
                  num_choices(x2) = num_choices(x2) + 1
                  location(x2,num_choices(x2)) = x3
                endif
              enddo
            enddo
c check if there is a pair of spots with the same two options
            do x2=1,9
              if (num_choices(x2).eq.2) then
                do x3=x2+1,9
                  if (num_choices(x3).eq.2) then
                    if ((location(x2,1).eq.location(x3,1)).and.(location(x2,2).eq.location(x3,2))) then
                      do x4=1,9
                        if ((x4.ne.x2).and.(x4.ne.x3)) then
                          lgrid(square_row(x1,x4),square_col(x1,x4),location(x2,1)) = .false.
                          lgrid(square_row(x1,x4),square_col(x1,x4),location(x2,2)) = .false.
                        endif
                      enddo
                    endif
                  endif
                enddo
              endif
            enddo
          enddo
          
c loop over each 3x3 square and see if there is an ivisible pair of options
          do x1=1,9
c initialize the parameters
            do x2=1,9
              num_choices(x2) = 0
              do x3=1,9
                location(x2,x3) = 0
              enddo
            enddo
c loop over each number and see which spots in the 3x3 square are possible         
            do x2=1,9
              do x3=1,9
                if (lgrid(square_row(x1,x3),square_col(x1,x3),x2)) then
                  num_choices(x2) = num_choices(x2) + 1
                  location(x2,num_choices(x2)) = x3
                endif
              enddo
            enddo
c check if there is a pair of numbers that appear in the same spots
            do x2=1,9
              if (num_choices(x2).eq.2) then
                do x3=x2+1,9
                  if (num_choices(x3).eq.2) then
                    if ((location(x2,1).eq.location(x3,1)).and.(location(x2,2).eq.location(x3,2))) then
                      do x4=1,9
                        if ((x4.ne.x2).and.(x4.ne.x3)) then
                          lgrid(square_row(x1,location(x2,1)),square_col(x1,location(x2,1)),x4) = .false.
                          lgrid(square_row(x1,location(x2,2)),square_col(x1,location(x2,2)),x4) = .false.
                        endif
                      enddo
                    endif
                  endif
                enddo
              endif
            enddo
          enddo          
          
c count up the number of zeros and trues in grid and lgrid respectively to see if the algorithm is making progress
          num_zeros = 0
          num_trues = 0
          do x1=1,9
            do x2=1,9
              if (grid(x1,x2).eq.0) then
                done = .false.
                num_zeros = num_zeros + 1
              endif
              do x3=1,9
                if (lgrid(x1,x2,x3)) num_trues = num_trues + 1
              enddo
            enddo
          enddo

c if it isn't then print out how far the program got
          if ((num_trues.eq.last_num_trues).and.(.not.done)) then
            write(*,*) 'ALGORITHM NOT FINISHING SUDOKU'
            write(*,*) 'grid right now'
            do x1=1,9
              write(*,fmt='(9(I2))') (grid(x1,x2), x2=1,9)
            enddo
            do x1=1,9
              write(*,*) 'number ',x1
              do x2=1,9
                write(*,*) (lgrid(x2,x3,x1), x3=1,9)
              enddo
            enddo
            pause
          endif
        enddo
        
C        write(*,*) 'GRID ',num_grids
        write(*,*) 'solution'
        do x1=1,9
          write(*,fmt='(9(I2))') (grid(x1,x2), x2=1,9)
        enddo

C        result = result + grid(1,1)*100 + grid(1,2)*10 + grid(1,3)
C        write(*,*) (grid(1,1)*100 + grid(1,2)*10 + grid(1,3)), result
C      enddo

      end
        
        
        