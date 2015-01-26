c-------------------------------------------------------------------------------------------------c
      program project_euler_56
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost    c
c  unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the      c
c  digits in each number is only 1.                                                               c
c                                                                                                 c
c  Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital    c
c  sum?                                                                                           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 a, b, max_sum, max_a, max_b 
      integer*8 big_num1(1000), ndig1, big_num2(1000), ndig2, big_num3(1000), ndig3, summ

c initializing
      max_sum = 0
    
c loop over a and b
      do a=1,100
        do b=1,100
          ndig1 = 1
          big_num1(1) = a
          
          do x1=1,b-1
            ndig2 = 1
            big_num2(1) = a
            call big_number_product(big_num1,ndig1,big_num2,ndig2,big_num3,ndig3)
            ndig1 = ndig3
            do x2=1,ndig1
              big_num1(x2) = big_num3(x2)
            enddo
          enddo
          
          summ = 0
          do x1=1,ndig1
            summ = summ + big_num1(x1)
          enddo
          
          if (summ.gt.max_sum) then
            max_a = a
            max_b = b
            max_sum = summ
          endif
        enddo
      enddo
      
      write(*,*) 'The largest digit sum is ',max_sum
      write(*,*) max_a,' ^ ',max_b
      
      end