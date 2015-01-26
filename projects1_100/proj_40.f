c-------------------------------------------------------------------------------------------------c
      program project_euler_40
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  An irrational decimal fraction is created by concatenating the positive integers:              c
c                                                                                                 c
c  0.123456789101112131415161718192021...                                                         c
c                                                                                                 c
c  It can be seen that the 12th digit of the fractional part is 1.                                c
c                                                                                                 c
c  If dn represents the nth digit of the fractional part, find the value of the following         c
c  expression.                                                                                    c
c                                                                                                 c
c  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000                                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 index, int, digit(4), n_digit, result
      
c loop over all integers until we get 1 million digits
      result = 1
      index = 0
      int = 1
      do while (index.lt.1000000)

        call get_digits(int,n_digit,digit)
        do x1=1,n_digit
          index = index + 1
          
          do x2=1,6
            if (index.eq.(10**x2)) then
              result = result * digit(x1)
              write(*,*) result
            endif
          enddo
        enddo
        
        int = int + 1
c        write(*,*) index
      enddo
     
      write(*,*) 'Result is ',result
      
      end