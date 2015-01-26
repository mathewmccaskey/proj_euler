c-------------------------------------------------------------------------------------------------c
      program project_euler_15
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to    c
c  the bottom right corner.                                                                       c
c                                                                                                 c
c  How many routes are there through a 20×20 grid?                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 grid, paths, num_paths
      
c initialize the grid
      grid = 20
      paths = 0
      
c loop over all the paths along the diagronal
      do x1=0,grid
        num_paths = 1
        if (x1.eq.0 .or. x1.eq.grid) then
          num_paths = 1
        else
          do x2=grid,max(x1,grid-x1)+1,-1
            num_paths = num_paths*x2
          enddo
          num_paths = num_paths/factorial(min(x1,grid-x1))
        endif
        
        write(*,*) x1, num_paths
        paths = paths + num_paths**2
      enddo
      
      write(*,*) 'The number of paths for a ',grid,' square grid is ',paths
      
      end