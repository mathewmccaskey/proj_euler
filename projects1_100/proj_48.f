c-------------------------------------------------------------------------------------------------c
      program project_euler_48
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.                                       c
c                                                                                                 c
c  Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 big_sum(1000), len_sum, big_prod(1000), len_prod, dummy1(1000), len_dummy1, dummy2(1000)
      integer*8 dummy3(1000), len_dummy3
      
c initialize all the variables
      len_sum = 0
      big_sum(1) = 0
      len_prod = 0
      
c loop over all the integer from 1 to 1000
      do x1=1,1000
        big_prod(1) = x1
        len_prod = 1

        do x2=1,x1-1
          len_dummy1 = len_prod
          do x3=1,len_dummy1
            dummy1(x3) = big_prod(x3)
          enddo
          
          dummy2(1) = x1
          
          call big_number_product(dummy1,len_dummy1,dummy2,1,big_prod,len_prod)

c take the product mod 10^10
          if (len_prod.gt.10) then
            do x3=11,len_prod
              big_prod(x3) = 0
            enddo
            len_prod = 10
          endif
        enddo
        
        len_dummy3 = len_sum
        do x2=1,len_dummy3
          dummy3(x2) = big_sum(x2)
        enddo
        
        call big_number_sum(dummy3,len_dummy3,big_prod,len_prod,big_sum,len_sum)        
      enddo
      
      write(*,*) 'The last 10 digits of the sum is:'
      write(*,fmt='(10(I1))') (big_sum(x1), x1=10,1,-1)
      
      end