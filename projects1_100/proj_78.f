c-------------------------------------------------------------------------------------------------c
      program project_euler_78
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Let p(n) represent the number of different ways in which n coins can be separated into piles.  c
c  For example, five coins can separated into piles in exactly seven different ways, so p(5)=7.   c
c                                                                                                 c
c  OOOOO                                                                                          c
c  OOOO   O                                                                                       c
c  OOO   OO                                                                                       c
c  OOO   O   O                                                                                    c
c  OO   OO   O                                                                                    c
c  OO   O   O   O                                                                                 c
c  O   O   O   O   O                                                                              c
c                                                                                                 c
c  Find the least value of n for which p(n) is divisible by one million.                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c Parameters used in this program only
      integer*8 nums(1000), signs(1000), p(100000), index

      do x1=1,500
        nums(2*x1-1) = x1*(3*x1-1)/2
        nums(2*x1) = x1*(3*x1+1)/2
        signs(2*x1-1) = (-1)**(x1+1)
        signs(2*x1) = (-1)**(x1+1)
      enddo
      
      p(0) = 1
      do x1=1,100000
        p(x1) = 0
        do x2=1,1000
          if (x1.ge.nums(x2)) then
            p(x1) = p(x1) + signs(x2)*p(x1-nums(x2))
c do a quick mod 10 million
            p(x1) = p(x1) - 1000000*(p(x1)/1000000)
          endif
        enddo
        write(*,*) x1, p(x1)
        if (p(x1).eq.0) then
          write(*,*) 'DONE'
          stop
        endif
      enddo
      
      end