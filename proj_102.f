c-------------------------------------------------------------------------------------------------c
      program project_euler_102
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Three distinct points are plotted at random on a Cartesian plane, for which -1000 ≤ x, y ≤     c
c  1000, such that a triangle is formed.                                                          c
c                                                                                                 c
c  Consider the following two triangles:                                                          c
c                                                                                                 c
c  A(-340,495), B(-153,-910), C(835,-947)                                                         c
c                                                                                                 c
c  X(-175,41), Y(-421,-714), Z(574,-645)                                                          c
c                                                                                                 c
c  It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.       c
c                                                                                                 c
c  Using triangles.txt (right click and 'Save Link/Target As...'), a 27K text file containing     c
c  the co-ordinates of one thousand "random" triangles, find the number of triangles for which    c
c  the interior contains the origin.                                                              c
c                                                                                                 c
c  NOTE: The first two examples in the file represent the triangles in the example given above.   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this progrma only
      integer*8 x_coord(3), y_coord(3), n_triangles
      logical left_zero, right_zero
      double precision zero
      
c initialize the triangles
      n_triangles = 0
      
c open up the file for reading
      open(unit=42,file='input_102.txt')
      
      do x1=1,1000
        read(42,*) (x_coord(x2),y_coord(x2), x2=1,3)

c initialize the flags
        left_zero = .false.
        right_zero = .false.
        
c find the zeros of the lines created by the different combinations of points
        do x2=1,3
          if (x2.lt.3) then
            x3=x2+1
          else
            x3=1
          endif
          
          if (y_coord(x2).ne.y_coord(x3)) then
            zero = -dble(y_coord(x2))*dble(x_coord(x3)-x_coord(x2))/dble(y_coord(x3)-y_coord(x2))+dble(x_coord(x2))
            
            if ((abs(zero-x_coord(x2)).le.abs(x_coord(x2)-x_coord(x3)))
     .            .and.(abs(zero-x_coord(x3)).le.abs(x_coord(x2)-x_coord(x3)))) then
              if (zero.lt.0.d0) then
                left_zero = .true.
              else
                right_zero = .true.
              endif
            endif
          endif
        enddo
        
        if ((left_zero).and.(right_zero)) n_triangles = n_triangles + 1
        
      enddo
      
      write(*,*) 'The number of triangles that include the origin is ',n_triangles
      
      end