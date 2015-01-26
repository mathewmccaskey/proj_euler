c-------------------------------------------------------------------------------------------------c
      program project_euler_91
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to     c
c  the origin, O(0,0), to form ΔOPQ.                                                              c
c                                                                                                 c
c  There are exactly fourteen triangles containing a right angle that can be formed when each     c
c  co-ordinate lies between 0 and 2 inclusive; that is,                                           c
c  0 ≤ x1, y1, x2, y2 ≤ 2.                                                                        c
c                                                                                                 c
c  Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?                    c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c  Parameters used in this program only
      integer*8 cord1(2), cord2(2), diff1(2), num_right
      
      num_right = 0
      do x1=0,50
        do x2=0,50
          do x3=0,50
            do x4=0,50
              if (.not.(((x1.eq.0).and.(x2.eq.0)).or.((x3.eq.0).and.(x4.eq.0)))) then
                if ((x1.ne.x3).or.(x2.ne.x4)) then
                  cord1(1) = x1
                  cord1(2) = x2
                  cord2(1) = x3
                  cord2(2) = x4
                  diff1(1) = x1-x3
                  diff1(2) = x2-x4
                  if (((cord1(1)*cord2(1) + cord1(2)*cord2(2)).eq.0).or.
     .                ((cord1(1)*diff1(1) + cord1(2)*diff1(2)).eq.0).or.
     .                ((cord2(1)*diff1(1) + cord2(2)*diff1(2)).eq.0)) then
                    num_right = num_right + 1
c                    write(*,fmt='(3I,2(3I,1X),5X,2(3I,1X))') num_right,x1,x2,x3,x4
c                    write(*,*) (cord1(1)*cord2(1) + cord1(2)*cord2(2))
c                    write(*,*) (cord1(1)*diff1(1) + cord1(2)*diff1(2))
c                    write(*,*) (cord2(1)*diff1(1) + cord2(2)*diff1(2))
c                    read(*,*)
                  endif
                endif
              endif
            enddo
          enddo
        enddo    
      enddo
      
      write(*,*) 'The number of right trangles is ', num_right/2
      
      end    