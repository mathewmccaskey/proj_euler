c-------------------------------------------------------------------------------------------------c
      program project_euler_17
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are    c
c  3 + 3 + 5 + 4 + 4 = 19 letters used in total.                                                  c
c                                                                                                 c
c  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how      c
c  many letters would be used?                                                                    c
c                                                                                                 c
c  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains  c
c  23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when        c
c  writing out numbers is in compliance with British usage.                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c keep track of the total
      integer*8 num_chars, digit(3), num_digits
      
      num_chars = 0
      do x1=1000,1,-1
        if (x1 .eq. 1000) then
          num_chars = num_chars + 11
        else 
          call get_digits(x1,num_digits,digit)
          
c 3 digits
          if (num_digits.eq.3) then
            if (digit(1).eq.1) num_chars = num_chars + 13
            if (digit(1).eq.2) num_chars = num_chars + 13
            if (digit(1).eq.3) num_chars = num_chars + 15
            if (digit(1).eq.4) num_chars = num_chars + 14
            if (digit(1).eq.5) num_chars = num_chars + 14
            if (digit(1).eq.6) num_chars = num_chars + 13
            if (digit(1).eq.7) num_chars = num_chars + 15
            if (digit(1).eq.8) num_chars = num_chars + 15
            if (digit(1).eq.9) num_chars = num_chars + 14
            if ((digit(2).eq.0).and.(digit(3).eq.0)) then
              num_chars = num_chars - 3
              num_digits = 0
              digit(1) = -1
              digit(2) = -1
              digit(3) = -1
            endif
            
            num_digits = num_digits-1
            digit(1) = digit(2)
            digit(2) = digit(3)
          endif

c two digits          
          if (num_digits.eq.2) then
            if (digit(1).eq.1) then
              if (digit(2).eq.0) num_chars = num_chars + 3
              if (digit(2).eq.1) num_chars = num_chars + 6
              if (digit(2).eq.2) num_chars = num_chars + 6
              if (digit(2).eq.3) num_chars = num_chars + 8
              if (digit(2).eq.4) num_chars = num_chars + 8
              if (digit(2).eq.5) num_chars = num_chars + 7
              if (digit(2).eq.6) num_chars = num_chars + 7
              if (digit(2).eq.7) num_chars = num_chars + 9
              if (digit(2).eq.8) num_chars = num_chars + 8
              if (digit(2).eq.9) num_chars = num_chars + 8
              
              num_digits = 0
              digit(1) = -1
              digit(2) = -1
            endif
      
            if (digit(1).eq.2) num_chars = num_chars + 6
            if (digit(1).eq.3) num_chars = num_chars + 6
            if (digit(1).eq.4) num_chars = num_chars + 5
            if (digit(1).eq.5) num_chars = num_chars + 5
            if (digit(1).eq.6) num_chars = num_chars + 5
            if (digit(1).eq.7) num_chars = num_chars + 7
            if (digit(1).eq.8) num_chars = num_chars + 6
            if (digit(1).eq.9) num_chars = num_chars + 6
          
            num_digits = num_digits - 1
            digit(1) = digit(2)
          endif

c one digits
          if (num_digits.eq.1) then
            if (digit(1).eq.1) num_chars = num_chars + 3
            if (digit(1).eq.2) num_chars = num_chars + 3
            if (digit(1).eq.3) num_chars = num_chars + 5
            if (digit(1).eq.4) num_chars = num_chars + 4
            if (digit(1).eq.5) num_chars = num_chars + 4
            if (digit(1).eq.6) num_chars = num_chars + 3
            if (digit(1).eq.7) num_chars = num_chars + 5
            if (digit(1).eq.8) num_chars = num_chars + 5
            if (digit(1).eq.9) num_chars = num_chars + 4
          endif
        endif
      enddo
      
      write(*,*) 'Total number of characters used = ',num_chars
      
      end