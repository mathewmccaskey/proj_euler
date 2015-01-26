c-------------------------------------------------------------------------------------------------c
      program project_euler_39
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there     c
c  are exactly three solutions for p = 120.                                                       c
c                                                                                                 c
c  {20,48,52}, {24,45,51}, {30,40,50}                                                             c
c                                                                                                 c
c  For which value of p ≤ 1000, is the number of solutions maximised?                             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 max_solutions, p_max, n_solutions, p
      
      max_solutions = 0
      do p=3,1000
        n_solutions = 0
        do x1=1,p
          do x2=x1+1,p
            if ((2*x1-2*p)*(2*x2-2*p).eq.(2*p**2)) then
              n_solutions = n_solutions + 1
            endif
          enddo
        enddo
        
        if (n_solutions .gt. max_solutions) then
          max_solutions = n_solutions
          p_max = p
        endif
      enddo
      
      write(*,*) 'The perimeter with ',max_solutions,' solutions is: ',p_max
      
      end