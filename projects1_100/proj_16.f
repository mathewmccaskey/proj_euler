c-------------------------------------------------------------------------------------------------c
      program project_euler_16
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.                              c
c                                                                                                 c
c  What is the sum of the digits of the number 2^1000?                                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num1(1000), num2(1000), num3(1000), len_num1, len_num2, len_num3, sum
      
c initialize the product
      num1(1) = 2
      len_num1 = 1
      num2(1) = 2
      len_num2 = 1
      call big_number_product(num1,len_num1,num2,len_num2,num3,len_num3)
C       write(*,fmt='(50(I1))') (num1(x2), x2=len_num1,1,-1)
C       write(*,fmt='(50(I1))') (num2(x2), x2=len_num2,1,-1)
C       write(*,fmt='(50(I1))') (num3(x2), x2=len_num3,1,-1)
C       write(*,*) '--------------------------------------------'
C       pause      
      
      do x1=3,1000
        do x2=1,len_num3
          num1(x2) = num3(x2)
        enddo
        len_num1 = len_num3

        call big_number_product(num1,len_num1,num2,len_num2,num3,len_num3)     
C         write(*,fmt='(50(I1))') (num1(x2), x2=len_num1,1,-1)
C         write(*,fmt='(50(I1))') (num2(x2), x2=len_num2,1,-1)
C         write(*,fmt='(50(I1))') (num3(x2), x2=len_num3,1,-1)
C         write(*,*) '--------------------------------------------'
C         pause 
      enddo
      
      write(*,*) '2^1000 is'
      write(*,fmt='(302(I1))') (num3(x1), x1=len_num3,1,-1)

c sum all the digits
      sum = 0
      do x1=1,len_num3
        sum = sum + num3(x1)
      enddo
      write(*,*) 'The sum of all the digits is ',sum
      
      end
    