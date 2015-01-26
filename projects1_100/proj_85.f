c-------------------------------------------------------------------------------------------------c
      program project_euler_85
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains         c
c  eighteen rectangles:                                                                           c
c                                                                                                 c
c  Although there exists no rectangular grid that contains exactly two million rectangles, find   c
c  the area of the grid with the nearest solution.                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num_rects, test, twomil
      integer*8 min_rect, min_len, min_wid

c initialize the result
      min_rect = 0
      twomil = 2000000

      do x1=1,1200
        do x2=1,3000/x1
          test = num_rects(x1,x2)
          if (abs(twomil-test).lt.abs(twomil-min_rect)) then
            min_rect = test
            min_len = x1
            min_wid = x2
            write(*,*) x1,x2,min_rect
          endif
        enddo
      enddo
      
      write(*,*) 'The area with the number of rectagles closest to 2 million is ',min_len*min_wid
              
      end



c-------------------------------------------------------------------------------------------------c
      function num_rects(len,wid)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Given the length and the width this calculates the number of rectangles.                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c input parameters
      integer*8 len, wid, num_rects
      integer*8 x1, x2
      
      num_rects = 0
      do x1=0,len-1
        do x2=0,wid-1
          num_rects = num_rects + (len-x1)*(wid-x2)
        enddo
      enddo
      
      return
      end