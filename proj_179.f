c-------------------------------------------------------------------------------------------------c
      program project_euler_179
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same number of        c
c  positive divisors. For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has          c
c  1, 3, 5, 15.                                                                                   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 result, num_fac_old, num_fac_new
      
c initialize the result
      result = 0
      num_fac_old = 1
      
      do x1=2,10**7
        
        if (((x1/10000)*10000).eq.x1) then
          write(*,*) x1,result
        endif
        
        num_fac_new = 0
        do x2=1,int(dsqrt(dble(x1)))
          if ((((x1/x2)*x2).eq.x1).and.(x1/x2.ne.x2)) then
            num_fac_new = num_fac_new + 2
          else if ((((x1/x2)*x2).eq.x1).and.(x1/x2.eq.x2)) then
            num_fac_new = num_fac_new + 1
          endif
        enddo
        
        if (num_fac_new.eq.num_fac_old) then
          result = result + 1
c          write(*,*) x1-1,x1
        endif
        
        num_fac_old = num_fac_new
      enddo
      
      write(*,*) 'Number of integers where n and n+1 have the same number of divisors ',result
      
      end