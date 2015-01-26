c-------------------------------------------------------------------------------------------------c
      program project_euler_138
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Consider the isosceles triangle with base length, b = 16, and legs, L = 17.                    c
c                                                                                                 c
c  By using the Pythagorean theorem it can be seen that the height of the triangle,               c
c  h = √(17^2 − 8^2) = 15, which is one less than the base length.                                c
c                                                                                                 c
c  With b = 272 and L = 305, we get h = 273, which is one more than the base length, and this is  c
c  the second smallest isosceles triangle with the property that h = b ± 1.                       c
c                                                                                                 c
c  Find ∑ L for the twelve smallest isosceles triangles for which h = b ± 1 and b, L are positive c
c  integers.                                                                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 L_old, b_old, L_new, b_new, total
      
c parameters from the diophantine equation solver
      integer*8 P,Q,K,R,S,L
      
c initiailze parameters
      total = 0
      L_old = 17
      b_old = 16
      total = L_old

c parameters from the diophantine equation solver
      P = -9
      Q = -10
      K = 8
      R = -8
      S = -9
      L = 8
      
      do x1=1,11
        L_new = P*L_old + Q*b_old + K
        b_new = R*L_old + S*b_old + L
        
        total = total + abs(L_new)
        write(*,*) abs(L_new), abs(b_new)
        
        L_old = L_new
        b_old = b_new
      enddo
            
      write(*,*) 'The sum of all the Ls are: ', total
      
      end