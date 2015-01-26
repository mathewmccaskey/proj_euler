c-------------------------------------------------------------------------------------------------c
      program project_euler_18
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Find the maximum total from top to bottom of an input triangle of 2 digit numbers.             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer num_lines, tri(100,100)
      
c read in the triangle
      open(unit=42, file='input_18.txt')
      read(42,*) num_lines
      do x1=1,num_lines
        read(42,*) (tri(x1,x2), x2=1,x1)
      enddo
      close(42)

c start from the second to last line and add the largest number from the line below
      do x1=num_lines-1,1,-1
        do x2=1,x1
          tri(x1,x2) = tri(x1,x2) + max(tri(x1+1,x2),tri(x1+1,x2+1))
        enddo
      enddo
      
      write(*,*) 'The largest path in the triangle is ',tri(1,1)
      
      end