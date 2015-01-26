c-------------------------------------------------------------------------------------------------c
      program project_euler_82
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  NOTE: This problem is a more challenging version of Problem 81.                                c
c                                                                                                 c
c  The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column    c
c  and finishing in any cell in the right column, and only moving up, down, and right, is         c
c  indicated in red and bold; the sum is equal to 994.                                            c
c                                                                                                 c
c  131	673	234	103	18                                                                            c
c  201	96	342	965	150                                                                           c
c  630	803	746	422	111                                                                           c
c  537	699	497	121	956                                                                           c
c  805	732	524	37	331                                                                           c
c                                                                                                 c
c  Find the minimal path sum, in matrix.txt (right click and 'Save Link/Target As...'), a 31K     c
c  text file containing a 80 by 80 matrix, from the left column to the right column.              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program
      integer*8 grid(80,80), prev_x(80,80), prev_y(80,80), summ(80,80), grid_size, node_x, node_y, min_node
      integer*8 path_x(1000), path_y(1000), num_path, min_path, min_path_start, min_path_end
      logical visited(80,80), visited_all_right

c read in the original grid
      grid_size = 80
      open(unit=42,file='input_82.txt')
C       open(unit=42,file='input_82_test.txt')
      do x1=1,grid_size
        read(42,*) (grid(x1,x2), x2=1,grid_size)
      enddo
      close(42)
C       do x1=1,grid_size
C         write(*,fmt='(5(I7))') (grid(x1,x2), x2=1,grid_size)
C       enddo
C       write(*,*) '---------------------------------------------------'

c initialize the minimum global path
      min_path = 10**10

c loop over the initial point on the left hand side of the grid
      do x1=1,grid_size

c initilzation
        do x2=1,grid_size
          do x3=1,grid_size
            summ(x2,x3) = 10**10
            visited(x2,x3) = .false.
          enddo
        enddo
      
        summ(x1,1) = grid(x1,1)
        visited_all_right = .false.
      
        do while(.not.(visited_all_right))

c find the node with the smallest sum        
          min_node = 10**10
          do x2=1,grid_size
            do x3=1,grid_size
              if ((summ(x2,x3).lt.min_node).and.(.not.(visited(x2,x3)))) then
                min_node = summ(x2,x3)
                node_x = x2
                node_y = x3
              endif
            enddo
          enddo
        
c check to see if we can move up, right, or down
c up
          if (node_x.ne.1) then
            if ((summ(node_x,node_y)+grid(node_x-1,node_y)).lt.summ(node_x-1,node_y)
     .            .and.(.not.(visited(node_x-1,node_y)))) then
              summ(node_x-1,node_y) = summ(node_x,node_y)+grid(node_x-1,node_y)
              prev_x(node_x-1,node_y) = node_x
              prev_y(node_x-1,node_y) = node_y
            endif
          endif

c right
          if (node_y.ne.grid_size) then
            if ((summ(node_x,node_y)+grid(node_x,node_y+1)).lt.summ(node_x,node_y+1)
     .            .and.(.not.(visited(node_x,node_y+1)))) then
              summ(node_x,node_y+1) = summ(node_x,node_y)+grid(node_x,node_y+1)
              prev_x(node_x,node_y+1) = node_x
              prev_y(node_x,node_y+1) = node_y
            endif
          endif
        
c down
          if (node_x.ne.grid_size) then
            if ((summ(node_x,node_y)+grid(node_x+1,node_y)).lt.summ(node_x+1,node_y)
     .            .and.(.not.(visited(node_x+1,node_y)))) then
              summ(node_x+1,node_y) = summ(node_x,node_y)+grid(node_x+1,node_y)
              prev_x(node_x+1,node_y) = node_x
              prev_y(node_x+1,node_y) = node_y
            endif
          endif
        
c now this point has been visited set the flag
          visited(node_x,node_y) = .true.

          visited_all_right = .true.
          do x2=1,grid_size
            if (.not.(visited(x2,grid_size))) visited_all_right = .false.
          enddo

        enddo
        
c check to find which ending point is the smallest compared to the global minimum
        do x2=1,grid_size
          if ((summ(x2,grid_size)).lt.min_path) then
            min_path = summ(x2,grid_size)
            min_path_start = x1
            min_path_end = x2
            
c save the path so we can write it out in the end
            num_path = 1
            path_x(num_path) = x2
            path_y(num_path) = grid_size
            do while ((path_x(num_path).ne.x1).or.(path_y(num_path).ne.1))
              num_path = num_path + 1
              path_x(num_path) = prev_x(path_x(num_path-1),path_y(num_path-1))
              path_y(num_path) = prev_y(path_x(num_path-1),path_y(num_path-1))
            enddo
          endif
        enddo
      enddo

      write(*,*) 'The minimum sum is ',min_path
      write(*,*) 'The path starts from row ',min_path_start,' on the left side'
      write(*,*) 'The path ends on row ',min_path_end,' on the right side'

c Time to write out the path
 
      write(*,*) 'Here is the min path with ',num_path,' nodes'
      do x1=num_path,1,-1
        write(*,*) path_x(x1), path_y(x1), grid(path_x(x1),path_y(x1))
      enddo

      end