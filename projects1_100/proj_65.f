c-------------------------------------------------------------------------------------------------c
      program project_euler_64
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  What is most surprising is that the important mathematical constant,                           c
c  e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].                                              c
c                                                                                                 c
c  The first ten terms in the sequence of convergents for e are:                                  c
c  2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...                          c
c                                                                                                 c
c  The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.                       c
c                                                                                                 c
c Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 a_n(0:99), len_num, num(1000), len_denom, denom(1000), result, n_start
      integer*8 dummy1(1000), len_dummy1, dummy2(1000), len_dummy2 
      
      a_n(0) = 2
      do x1=1,33
        a_n(3*x1-2) = 1
        a_n(3*x1-1) = 2*x1
        a_n(3*x1) = 1
      enddo
      
      len_num = 1
      len_denom = 1
      
      n_start = 99
      num(1) = a_n(n_start)*a_n(n_start-1)+1
      denom(1) = a_n(n_start)
C       write(*,fmt='(4(I1))') (num(x2), x2=len_num,1,-1)
C       write(*,fmt='(4(I1))') (denom(x2), x2=len_denom,1,-1)
      
      do x1=n_start-2,0,-1
C         write(*,*) 'a_n ',a_n(x1)
        
        do x2=1,len_num
          dummy2(x2) = num(x2)
          num(x2) = num(x2)*a_n(x1)
        enddo
        len_dummy2 = len_num

        call big_number_sum(num,len_num,denom,len_denom,dummy1,len_dummy1)    
        do x2=1,len_dummy1
          num(x2) = dummy1(x2)
        enddo
        len_num = len_dummy1
        do x2=1,len_dummy2
          denom(x2) = dummy2(x2)
        enddo
        len_denom = len_dummy2

C         write(*,fmt='(4(I1))') (num(x2), x2=len_num,1,-1)
C         write(*,fmt='(4(I1))') (denom(x2), x2=len_denom,1,-1)
      enddo
      
      result = 0
      do x1=1,len_num
        result = result + num(x1)
      enddo

      write(*,fmt='(100(I1))') (num(x1), x1=len_num,1,-1)
      write(*,fmt='(100(I1))') (denom(x1), x1=len_denom,1,-1)
      write(*,*) 'The sum of the digits of the numerator of the 100th convergent of the continued fraction for e ',result
      
      end