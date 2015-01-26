c-------------------------------------------------------------------------------------------------c
      program project_euler_11
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  What is the greatest product of four adjacent numbers in any direction (up, down, left,        c
c  right, or diagonally) in the 20×20 grid?                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer grid(20,20), prod, max_prod

c read in the grid from the input file
      open(unit=42,file='input_11.txt')
      do x1=1,20
        read(42,*) (grid(x1,x2), x2=1,20)
      enddo
      
c initialize the maximum product
      max_prod = 0
      
c look at all horizontal combos
      do x1=1,20
        do x2=1,17
          prod = grid(x1,x2)*grid(x1,x2+1)*grid(x1,x2+2)*grid(x1,x2+3)
          if (prod .gt. max_prod) max_prod = prod
        enddo
      enddo

c look at all vertical combos
      do x2=1,20
        do x1=1,17
          prod = grid(x1,x2)*grid(x1,x2+1)*grid(x1,x2+2)*grid(x1,x2+3)
          if (prod .gt. max_prod) max_prod = prod
        enddo
      enddo

c look at all forward diagonals 
      do x1=4,20
        do x2=1,17
          prod = grid(x1,x2)*grid(x1-1,x2+1)*grid(x1-2,x2+2)*grid(x1-3,x2+3)
          if (prod .gt. max_prod) max_prod = prod
        enddo
      enddo
      
c look at all backward diagonals 
      do x1=1,17
        do x2=1,17
          prod = grid(x1,x2)*grid(x1+1,x2+1)*grid(x1+2,x2+2)*grid(x1+3,x2+3)
          if (prod .gt. max_prod) max_prod = prod
        enddo
      enddo
      
      write(*,*) 'The maximum product in the grid is ',max_prod
      
      end