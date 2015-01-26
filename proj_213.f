c-------------------------------------------------------------------------------------------------c
      program project_euler_213
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A 30×30 grid of squares contains 900 fleas, initially one flea per square.                     c
c  When a bell is rung, each flea jumps to an adjacent square at random (usually 4 possibilities, c
c  except for fleas on the edge of the grid or at the corners).                                   c
c                                                                                                 c
c  What is the expected number of unoccupied squares after 50 rings of the bell? Give your answer c
c  rounded to six decimal places.                                                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      double precision prob_grid(900,900), transition(900,900), next_grid(900,900), sub_result, result

c initialize the probability grid and transition grid
      do x1=1,900
        do x2=1,900
          prob_grid(x1,x2) = 0.d0
          transition(x1,x2) = 0.d0
        enddo
        prob_grid(x1,x1) = 1.d0
      enddo
      
c grid: (1,1), List: 1 can only transition to (1,2):2 and (2,1):31
      transition(1,2) = 1.d0/2.d0
      transition(1,31) = 1.d0/2.d0
      
c grid: (1,30), List:30 can only transition to (1,29):29 and (2,30):60
      transition(30,29) = 1.d0/2.d0
      transition(30,60) = 1.d0/2.d0
      
c grid: (30,1), List:871 can only transition to (29,1):841 and (30,2):872
      transition(871,841) = 1.d0/2.d0
      transition(871,872) = 1.d0/2.d0
      
c grid: (30,30), List:900 can only transition to (29,30):870 and (30,29):899
      transition(900,870) = 1.d0/2.d0
      transition(900,899) = 1.d0/2.d0

      do x1=2,29

c grid: (1,2..29), List: 2..29 can transition left(x-1), right(x+1), down(x+30)
        transition(x1,x1-1) = 1.d0/3.d0
        transition(x1,x1+1) = 1.d0/3.d0
        transition(x1,x1+30) = 1.d0/3.d0
        
c grid: (30,2..29), List: 872..899 can transition left(x-1), right(x+1), up(x-30)
        transition((870+x1),(870+x1)-1) = 1.d0/3.d0
        transition((870+x1),(870+x1)+1) = 1.d0/3.d0
        transition((870+x1),(870+x1)-30) = 1.d0/3.d0
        
c grid: (2..29,1), list: 31,61,..,841 can transition up(x-30), right(x+1), down(x+30)
        transition((30*(x1-1)+1),(30*(x1-1)+1)-30) = 1.d0/3.d0
        transition((30*(x1-1)+1),(30*(x1-1)+1)+1) = 1.d0/3.d0
        transition((30*(x1-1)+1),(30*(x1-1)+1)+30) = 1.d0/3.d0

c grid: (2..29,30), list: 60,90,..,870 can transition up(x-30), left(x-1), down(x+30)
        transition((30*x1),(30*x1)-30) = 1.d0/3.d0
        transition((30*x1),(30*x1)-1) = 1.d0/3.d0
        transition((30*x1),(30*x1)+30) = 1.d0/3.d0

      enddo  

c everything else in the grid (2..29,2..29) can transition up(x-30), left(x-1), right(x+1), down(x+30)
      do x1=2,29
        do x2=2,29
          transition((30*(x1-1)+x2),(30*(x1-1)+x2)-30) = 1.d0/4.d0
          transition((30*(x1-1)+x2),(30*(x1-1)+x2)-1) = 1.d0/4.d0
          transition((30*(x1-1)+x2),(30*(x1-1)+x2)+1) = 1.d0/4.d0
          transition((30*(x1-1)+x2),(30*(x1-1)+x2)+30) = 1.d0/4.d0
        enddo
      enddo

C c print out the transition to see if things are working
C       do x1=871,900
C         write(*,fmt='(900(SE10.5))') (transition(x1,x2), x2=1,900)
C         write(*,*) '--------------------------------------------------'
C         read(*,*)
C       enddo
          
c loop over 50 iterations
      do x1=1,50
        
c multiply the probability grid with the transition grid
        do x2=1,900
          do x3=1,900
            next_grid(x2,x3) = 0.d0
            do x4=1,900
              next_grid(x2,x3) = next_grid(x2,x3) + prob_grid(x2,x4)*transition(x4,x3)
            enddo
          enddo
        enddo
        
c save the next_grid to the prob_grid
        do x2=1,900
          do x3=1,900
            prob_grid(x2,x3) = next_grid(x2,x3)
          enddo
        enddo
        
C c print out the prob_grid to see if things are working
C         do x2=1,10
C           write(*,fmt='(10(SE10.5))') (prob_grid(30*(x2-1)+1,x3), x3=1,10)
C         enddo
C         read(*,*)
        
      enddo
      
c the prob_grid now shows for each row the probability that the initial flea ends up in a certain bin
c the prob that the bin is empty for each flea is 1-prob_grid
      do x1=1,900
        do x2=1,900
          prob_grid(x1,x2) = 1.d0 - prob_grid(x1,x2)
        enddo
      enddo
      
c tally up all the probabilities
      result = 0.d0
      do x1=1,900
        sub_result = 1.d0
        do x2=1,900
          sub_result = sub_result*prob_grid(x2,x1)
        enddo
        result = result + sub_result
      enddo
      
      write(*,*) 'The expected number of empty cells is ',result
      
      end