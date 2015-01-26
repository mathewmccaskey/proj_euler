c-------------------------------------------------------------------------------------------------c
      program project_euler_8
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Find the greatest product of five consecutive digits in the 1000-digit number given in a file. c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer digit(1000), prod, max_prod
      
c read in the 1000 digit number
      open(unit=42, file='input_8.txt')
      read(42,fmt='(1000(I1))') (digit(x1), x1=1,1000)
      close(42)

c initialize the maximum product
      max_prod = 0
      
c loop over all the combinations to find the max product
      do x1=1,996
        prod = digit(x1)*digit(x1+1)*digit(x1+2)*digit(x1+3)*digit(x1+4)
        if (prod.gt.max_prod) max_prod = prod
      enddo
      
      write(*,*) 'The maximum product of the 1000 digit number is ',max_prod
      
      end