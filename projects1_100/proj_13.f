c-------------------------------------------------------------------------------------------------c
      program project_euler_13
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 big_nums(100,1000), num1(1000), num2(1000), num3(1000), len_num1, len_num2, len_num3
      
c read in the numbers
      open(unit=42, file='input_13.txt')
      do x1=1,100
        read(42,fmt='(50(I1))') (big_nums(x1,x2), x2=50,1,-1)
      enddo
      
c now add the numbers
      do x1=1,1000
        num1(x1) = big_nums(1,x1)
        num2(x1) = big_nums(2,x1)
      enddo
      call big_number_sum(num1,50,num2,50,num3,len_num3)
C       write(*,fmt='(50(I1))') (num1(x2), x2=50,1,-1)
C       write(*,fmt='(50(I1))') (num2(x2), x2=50,1,-1)
C       write(*,fmt='(50(I1))') (num3(x2), x2=len_num3,1,-1)
C       write(*,*) '--------------------------------------------'
C       pause
      
      do x1=3,100
        do x2=1,1000
          num1(x2) = num3(x2)
          num2(x2) = big_nums(x1,x2)
        enddo
        len_num1 = len_num3

        call big_number_sum(num1,len_num1,num2,50,num3,len_num3)        
C         write(*,fmt='(52(I1))') (num1(x3), x3=len_num1,1,-1)
C         write(*,fmt='(50(I1))') (num2(x3), x3=50,1,-1)
C         write(*,fmt='(52(I1))') (num3(x3), x3=len_num3,1,-1)        
C         write(*,*) '-----------------------------------------------'
C         pause
        
      enddo

      write(*,*) 'Sum of 100 50-digit numbers'
      write(*,fmt='(53(I1))') (num3(x1), x1=len_num3,1,-1)
      write(*,*) len_num3
      
      end  