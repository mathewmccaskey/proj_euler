c-------------------------------------------------------------------------------------------------c
      program project_euler_108
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  In the following equation x, y, and n are positive integers.                                   c
c             1     1     1                                                                       c
c             -  +  -  =  -                                                                       c
c             x     y     n                                                                       c
c                                                                                                 c
c  For n = 4 there are exactly three distinct solutions:                                          c
c  x=5, y=20                                                                                      c
c  x=6, y=12                                                                                      c
c  x=8, y=8                                                                                       c
c                                                                                                 c
c  What is the least value of n for which the number of distinct solutions exceeds one-thousand?  c
c                                                                                                 c
c  NOTE: This problem is an easier version of problem 110; it is strongly advised that you solve  c
c  this one first.                                                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameter used in this program only
      integer*8 n, nsq, x, y, num_solutions
      logical done
      
c initialize some parameters
      done = .false.
      n = 1
      
      do while(.not. done)
        num_solutions = 0
        nsq = n**2
        do x1=1,int(dsqrt(dble(nsq)))
          if (((nsq/x1)*x1).eq.nsq) then
            num_solutions = num_solutions + 1
c solution 1
            x = n+x1
            y = n+nsq/x1
c            write(*,*) 'x=',x,' y=',y

c solutions are positive integers so this isn't valid
C c solution 2
C             x = n-x1
C             y = n-nsq/x1
C             if ((x.ne.0).and.(y.ne.0)) then
C               num_solutions = num_solutions + 1
C               write(*,*) 'x=',x,' y=',y
C             endif
          endif
        enddo
        
        if (num_solutions.gt.1000) then
          done = .true.
          write(*,*) 'Found it at n=',n
        endif
        
        write(*,*) n,num_solutions
c        read(*,*)
        
        n=n+1
      enddo
      
      end