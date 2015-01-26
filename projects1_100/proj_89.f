c-------------------------------------------------------------------------------------------------c
      program project_euler_89
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The rules for writing Roman numerals allow for many ways of writing each number (see About     c
c  Roman Numerals...). However, there is always a "best" way of writing a particular number.      c
c                                                                                                 c
c  For example, the following represent all of the legitimate ways of writing the number sixteen: c
c                                                                                                 c
c  IIIIIIIIIIIIIIII                                                                               c
c  VIIIIIIIIIII                                                                                   c
c  VVIIIIII                                                                                       c
c  XIIIIII                                                                                        c
c  VVVI                                                                                           c
c  XVI                                                                                            c
c                                                                                                 c
c  The last example being considered the most efficient, as it uses the least number of numerals. c
c                                                                                                 c
c  The 11K text file, input_89.txt (right click and 'Save Link/Target As...'), contains one       c
c  thousand numbers written in valid, but not necessarily minimal, Roman numerals; that is, they  c
c  are arranged in descending units and obey the subtractive pair rule (see About Roman           c
c  Numerals... for the definitive rules for this problem).                                        c
c                                                                                                 c
c  Find the number of characters saved by writing each of these in their minimal form.            c
c                                                                                                 c
c  Note: You can assume that all the Roman numerals in the file contain no more than four         c
c  consecutive identical units.                                                                   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c paramters used in this program only
      character(30) roman(1000), new_roman(1000)
      integer*8 length(1000), new_length(1000), number(1000), dummy, saved_digits

c initialize the result
      saved_digits = 0

c read all the roman numerals
      open(unit=42,file='input_89.txt')
      do x1=1,1000
        read(42,*) roman(x1)
      enddo
      close(42)
      
c go through each roman numeral and find the number of characters and numerical value
      do x1=1,1000
        length(x1) = index(roman(x1),' ')-1
        number(x1) = 0
        do x2=1,length(x1)
          if (roman(x1)(x2:x2).eq.'M') number(x1) = number(x1) + 1000
          if (roman(x1)(x2:x2).eq.'D') number(x1) = number(x1) + 500
          
c C can appear before D and M for a subtractive pair
          if (roman(x1)(x2:x2).eq.'C') then
            if ((roman(x1)(x2+1:x2+1).eq.'D').or.(roman(x1)(x2+1:x2+1).eq.'M')) then
              number(x1) = number(x1) - 100
            else
              number(x1) = number(x1) + 100
            endif
          endif
          
c X can appear before L and C for a subtractive pair
          if (roman(x1)(x2:x2).eq.'L') number(x1) = number(x1) + 50
          if (roman(x1)(x2:x2).eq.'X') then
            if ((roman(x1)(x2+1:x2+1).eq.'L').or.(roman(x1)(x2+1:x2+1).eq.'C')) then
              number(x1) = number(x1) - 10
            else
              number(x1) = number(x1) + 10
            endif
          endif
          
c I can appear before V and X for a subtractive pair
          if (roman(x1)(x2:x2).eq.'V') number(x1) = number(x1) + 5
          if (roman(x1)(x2:x2).eq.'I') then
            if ((roman(x1)(x2+1:x2+1).eq.'V').or.(roman(x1)(x2+1:x2+1).eq.'X')) then
              number(x1) = number(x1) - 1
            else
              number(x1) = number(x1) + 1
            endif
          endif
        enddo
        
c once we have the number we can convert it to the smallest value
        new_length(x1) = 0
        dummy = number(x1)

c thousands digit
        do while(dummy.ge.1000)
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'M'
          new_length(x1) = new_length(x1) + 1
          dummy = dummy - 1000
        enddo

c hundreds digits
        if (dummy.ge.900) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'CM'
          new_length(x1) = new_length(x1) + 2
          dummy = dummy - 900
        endif
        if (dummy.ge.500) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'D'
          new_length(x1) = new_length(x1) + 1
          dummy = dummy - 500
        endif
        if (dummy.ge.400) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'CD'
          new_length(x1) = new_length(x1) + 2
          dummy = dummy - 400
        endif
        do while(dummy.ge.100)
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'C'
          new_length(x1) = new_length(x1) + 1
          dummy = dummy - 100
        enddo

c tens digit        
        if (dummy.ge.90) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'XC'
          new_length(x1) = new_length(x1) + 2
          dummy = dummy - 90
        endif
        if (dummy.ge.50) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'L'
          new_length(x1) = new_length(x1) + 1
          dummy = dummy - 50
        endif
        if (dummy.ge.40) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'XL'
          new_length(x1) = new_length(x1) + 2
          dummy = dummy - 40
        endif
        do while(dummy.ge.10)
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'X'
          new_length(x1) = new_length(x1) + 1
          dummy = dummy - 10
        enddo

c ones digit        
        if (dummy.ge.9) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'IX'
          new_length(x1) = new_length(x1) + 2
          dummy = dummy - 9
        endif
        if (dummy.ge.5) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'V'
          new_length(x1) = new_length(x1) + 1
          dummy = dummy - 5
        endif
        if (dummy.ge.4) then
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'IV'
          new_length(x1) = new_length(x1) + 2
          dummy = dummy - 4
        endif
        do while(dummy.ge.1)
          new_roman(x1) = new_roman(x1)(1:new_length(x1))//'I'
          new_length(x1) = new_length(x1) + 1
          dummy = dummy - 1
        enddo

        if ((length(x1)-new_length(x1)).gt.3) then
          write(*,fmt='(A30)') roman(x1)
          write(*,*) number(x1), length(x1)
          write(*,fmt='(A30)') new_roman(x1)
          write(*,*) number(x1), new_length(x1)
        endif
        saved_digits = saved_digits + length(x1) - new_length(x1)
      enddo
      
      write(*,*) 'The number of saved digits writing the roman numerals correctly is ',saved_digits
      end