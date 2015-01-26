c-------------------------------------------------------------------------------------------------c
      program project_euler_323
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Let y_0, y_1, y_2,... be a sequence of random unsigned 32 bit integers                         c
c  (i.e. 0 ≤ y_i < 2^32, every value equally likely).                                             c
c                                                                                                 c
c  For the sequence xi the following recursion is given:                                          c
c                                                                                                 c
c      x_0 = 0 .and.                                                                              c
c      x_i = x_(i-1) | y_(i-1), for i > 0. ( | is the bitwise-OR operator)                        c
c                                                                                                 c
c  It can be seen that eventually there will be an index N such that x_i = 2^32 -1 (a bit-pattern c
c  of all ones) for all i ≥ N.                                                                    c
c                                                                                                 c
c  Find the expected value of N.                                                                  c
c  Give your answer rounded to 10 digits after the decimal point.                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      double precision P_old, P_new, result
      integer*8 index
      logical done
      
c initialize everything
      result = 0.d0
      p_old = 0.d0
      done = .false.
      index = 1
      
c loooooooop
      do while(.not.done)
        p_new = (1.d0-(0.5d0)**index)**32
        result = result + index*(p_new-p_old)
        
        if ((index*(p_new-p_old)) .lt. 1.0d-11) done = .true.
        
        index = index + 1
        p_old = p_new
        write(*,*) index, p_new, result
      enddo
      
      write(*,*) 'result = ',result
      end