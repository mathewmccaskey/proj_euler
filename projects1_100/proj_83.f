c-------------------------------------------------------------------------------------------------c
      program project_euler_83
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  NOTE: This problem is a significantly more challenging version of Problem 81.                  c
c                                                                                                 c
c  In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by     c
c  moving left, right, up, and down, is indicated in bold red and is equal to 2297.               c
c                                                                                                 c
c  131	673	234	103	18                                                                            c
c  201	96	342	965	150                                                                           c
c  630	803	746	422	111                                                                           c
c  537	699	497	121	956                                                                           c
c  805	732	524	37	331                                                                           c
c                                                                                                 c
c  Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K     c
c  text file containing a 80 by 80 matrix, from the top left to the bottom right by moving left,  c
c  right, up, and down.                                                                           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program
      integer*8 grid(80,80), prev_x(80,80), prev_y(80,80), summ(80,80), grid_size, node_x, node_y, min_node
      integer*8 path_x(1000), path_y(1000), num_path
      logical visited(80,80)

c read in the original grid
      grid_size = 80
      open(unit=42,file='input_83.txt')
C       open(unit=42,file='input_83_test.txt')
      do x1=1,grid_size
        read(42,*) (grid(x1,x2), x2=1,grid_size)
      enddo
      close(42)
C       do x1=1,grid_size
C         write(*,fmt='(5(I7))') (grid(x1,x2), x2=1,grid_size)
C       enddo
C       write(*,*) '---------------------------------------------------'
      
c initilzation
      do x1=1,grid_size
        do x2=1,grid_size
          summ(x1,x2) = 10**10
          visited(x1,x2) = .false.
        enddo
      enddo
      
      summ(1,1) = grid(1,1)
      
      do while(.not.(visited(grid_size,grid_size)))

c find the node with the smallest sum        
        min_node = 10**10
        do x1=1,grid_size
          do x2=1,grid_size
            if ((summ(x1,x2).lt.min_node).and.(.not.(visited(x1,x2)))) then
              min_node = summ(x1,x2)
              node_x = x1
              node_y = x2
            endif
          enddo
        enddo
        
c check to see if we can move in each direction
c up
        if (node_x.ne.1) then
          if ((summ(node_x,node_y)+grid(node_x-1,node_y)).lt.summ(node_x-1,node_y)
     .          .and.(.not.(visited(node_x-1,node_y)))) then
            summ(node_x-1,node_y) = summ(node_x,node_y)+grid(node_x-1,node_y)
            prev_x(node_x-1,node_y) = node_x
            prev_y(node_x-1,node_y) = node_y
          endif
        endif

c right
        if (node_y.ne.grid_size) then
          if ((summ(node_x,node_y)+grid(node_x,node_y+1)).lt.summ(node_x,node_y+1)
     .          .and.(.not.(visited(node_x,node_y+1)))) then
            summ(node_x,node_y+1) = summ(node_x,node_y)+grid(node_x,node_y+1)
            prev_x(node_x,node_y+1) = node_x
            prev_y(node_x,node_y+1) = node_y
          endif
        endif
        
c down
        if (node_x.ne.grid_size) then
          if ((summ(node_x,node_y)+grid(node_x+1,node_y)).lt.summ(node_x+1,node_y)
     .          .and.(.not.(visited(node_x+1,node_y)))) then
            summ(node_x+1,node_y) = summ(node_x,node_y)+grid(node_x+1,node_y)
            prev_x(node_x+1,node_y) = node_x
            prev_y(node_x+1,node_y) = node_y
          endif
        endif

c left
        if (node_y.ne.1) then
          if ((summ(node_x,node_y)+grid(node_x,node_y-1)).lt.summ(node_x,node_y-1)
     .          .and.(.not.(visited(node_x,node_y-1)))) then
            summ(node_x,node_y-1) = summ(node_x,node_y)+grid(node_x,node_y-1)
            prev_x(node_x,node_y-1) = node_x
            prev_y(node_x,node_y-1) = node_y
          endif
        endif
        
c now this point has been visited set the flag
        visited(node_x,node_y) = .true.

      enddo
      
      write(*,*) 'The minimum sum is ',summ(grid_size,grid_size)

c Time to write out the path
      num_path = 1
      path_x(num_path) = grid_size
      path_y(num_path) = grid_size
      do while ((path_x(num_path).ne.1).or.(path_y(num_path).ne.1))
        num_path = num_path + 1
        path_x(num_path) = prev_x(path_x(num_path-1),path_y(num_path-1))
        path_y(num_path) = prev_y(path_x(num_path-1),path_y(num_path-1))
      enddo
      
      write(*,*) 'Here is the min path with ',num_path,' nodes'
      do x1=num_path,1,-1
        write(*,*) path_x(x1), path_y(x1), grid(path_x(x1),path_y(x1))
      enddo

      end