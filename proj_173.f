c-------------------------------------------------------------------------------------------------c
      program project_euler_173
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  We shall define a square lamina to be a square outline with a square "hole" so that the shape  c
c  possesses vertical and horizontal symmetry. For example, using exactly thirty-two square tiles c
c  we can form two different square laminae:                                                      c
c                                                                                                 c
c  With one-hundred tiles, and not necessarily using all of the tiles at one time, it is possible c
c  to form forty-one different square laminae.                                                    c
c                                                                                                 c
c  Using up to one million tiles how many different square laminae can be formed?                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 core, index, squares, max_squares, result
      
c initialize
      result = 0
      core = 1
      max_squares = 1000000
      
c loop over all the possible square holes
      do core=1,249999
c      do core=1,1
      
        squares = 4*core+4
        index = core + 2
        
c keep adding layers until we run out of squares
        do while(squares.le.1000000)
c          write(*,*) index, squares
c          read(*,*)
          result = result + 1
          squares = squares + 4*index+4
          index = index + 2
          
        enddo
      enddo
          

      write(*,*) 'result = ',result
      
      end