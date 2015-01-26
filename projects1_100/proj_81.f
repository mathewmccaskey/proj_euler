c-------------------------------------------------------------------------------------------------c
      program project_euler_81
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by     c
c  only moving to the right and down, is indicated in bold red and is equal to 2427.              c
c                                                                                                 c	
c  131	673	234	103	18                                                                            c
c  201	96	342	965	150                                                                           c
c  630	803	746	422	111                                                                           c
c  537	699	497	121	956                                                                           c
c  805	732	524	37	331                                                                           c
c                                                                                                 c
c  Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K     c
c  text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving   c
c  right and down.                                                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 grid_size, grid(80,80), grid_triangle(159,159)
      
c read in the original grid
      grid_size = 80
      open(unit=42,file='input_81.txt')
c      open(unit=42,file='input_81_test.txt')
      do x1=1,grid_size
        read(42,*) (grid(x1,x2), x2=1,grid_size)
      enddo
      close(42)
C       do x1=1,grid_size
C         write(*,fmt='(5(I7))') (grid(x1,x2), x2=1,grid_size)
C       enddo
C       write(*,*) '---------------------------------------------------'
            
c initialize the triangle
      do x1=1,2*grid_size-1
        do x2=1,2*grid_size-1
          grid_triangle(x1,x2) = 1000000
        enddo
      enddo
      
c rotate the grid 45 degrees
      do x1=1,grid_size
        do x2=1,x1
          grid_triangle(x1,x2) = grid(x1-x2+1,x2)
        enddo
      enddo
      
      do x1=1,grid_size-1
        do x2=1,grid_size-x1
          grid_triangle(grid_size+x1,x1+x2) = grid(grid_size+1-x2,x1+x2)
        enddo
      enddo
      
C c check to see if everything worked
C       do x1=1,2*grid_size-1
C         write(*,fmt='(5(I7))') (grid_triangle(x1,x2), x2=1,grid_size)
C       enddo

c start from the second to last line and add the largest number from the line below
      do x1=2*grid_size-1,1,-1
        do x2=1,x1
          grid_triangle(x1,x2) = grid_triangle(x1,x2) + min(grid_triangle(x1+1,x2),grid_triangle(x1+1,x2+1))
        enddo
      enddo
      
      write(*,*) 'The minimum path from one corner of square to the other is ',grid_triangle(1,1)
      
      end