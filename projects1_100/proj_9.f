c-------------------------------------------------------------------------------------------------c
      program project_euler_9
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,                 c
c  a^2 + b^2 = c^2                                                                                c
c                                                                                                 c
c  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.                                                    c
c                                                                                                 c
c  There exists exactly one Pythagorean triplet for which a + b + c = 1000.                       c
c  Find the product abc.                                                                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
      do x1=1,998
        do x2=1,999 - x1
          x3 = 1000 - x1 - x2
          if ((x1**2+x2**2).eq.(x3**2)) then
            write(*,*) 'a = ',x1
            write(*,*) 'b = ',x2
            write(*,*) 'c = ',x3
            write(*,*) 'abx = ',x1*x2*x3
            stop
          endif
        enddo
      enddo
      
      end