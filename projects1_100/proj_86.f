c-------------------------------------------------------------------------------------------------c
      program project_euler_86
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits    c
c  in the opposite corner. By travelling on the surfaces of the room the shortest "straight line" c
c  distance from S to F is 10 and the path is shown on the diagram.                               c
c                                                                                                 c
c  However, there are up to three "shortest" path candidates for any given cuboid and the         c
c  shortest route doesn't always have integer length.                                             c
c                                                                                                 c
c  By considering all cuboid rooms with integer dimensions, up to a maximum size of M by M by M,  c
c  there are exactly 2060 cuboids for which the shortest route has integer length when M=100,     c
c  and this is the least value of M for which the number of solutions first exceeds two thousand; c
c  the number of solutions is 1975 when M=99.                                                     c
c                                                                                                 c
c  Find the least value of M such that the number of solutions first exceeds one million.         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c  Parameters used in this program only
      integer*8 M, l, w, h, solutions, hypsq
      
      M = 1
      solutions = 0
      
      do while(solutions.le.1000000)
        l = M
        do w=1,M
          do h=w,M
            hypsq = l**2 + (w+h)**2
            if (dsqrt(dble(hypsq)).eq.dble(int(dsqrt(dble(hypsq))))) then
              solutions = solutions + 1
            endif
          enddo
        enddo
        
        write(*,*) M,solutions

        M = M + 1
      enddo
      
      write(*,*) 'The answer should already be written'
      
      end