c-------------------------------------------------------------------------------------------------c
      program project_euler_120
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Let r be the remainder when (a−1)^n + (a+1)^n is divided by a^2.                               c
c                                                                                                 c
c  For example, if a = 7 and n = 3, then r = 42: 6^3 + 8^3 = 728 ≡ 42 mod 49. And as n varies,    c
c  so too will r, but for a = 7 it turns out that rmax = 42.                                      c
c                                                                                                 c
c  For 3 ≤ a ≤ 1000, find ∑ rmax.                                                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 sum
      
c initialize the result
      sum = 0
      
c for a being even the general result for r_max is a(a-2), for a odd, a(a-1)
      do x1=3,1000
        if (((x1/2)*2).eq.x1) then
          sum = sum + (x1*(x1-2))
        else
          sum = sum + (x1*(x1-1))
        endif
      enddo
      
      write(*,*) 'The sum is ',sum
      
      end