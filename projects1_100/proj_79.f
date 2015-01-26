c-------------------------------------------------------------------------------------------------c
      program project_euler_79
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A common security method used for online banking is to ask the user for three random           c
c  characters from a passcode. For example, if the passcode was 531278, they may ask for the 2nd, c
c  3rd, and 5th characters; the expected reply would be: 317.                                     c
c                                                                                                 c
c  The text file, input_79.txt, contains fifty successful login attempts.                         c
c                                                                                                 c
c  Given that the three characters are always asked for in order, analyse the file so as to       c
c  determine the shortest possible secret passcode of unknown length.                             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 codes(50,3), which_digits(0:9), ndigits, digit(10), digit_test(8)
      logical found_result, code_works
      
c read in all the keycode attempts
      open(unit=42,file='input_79.txt')
      do x1=1,50
        read(42,fmt='(3(I1))') (codes(x1,x2), x2=1,3)
      enddo
      close(42)
      
c find out which digits are used in the 50 attempts
      do x1=0,9
        which_digits(x1) = 0
      enddo
      do x1=1,50
        do x2=1,3
          if (which_digits(codes(x1,x2)).eq.0) which_digits(codes(x1,x2)) = 1
        enddo
      enddo

c put the digits that are used in a list      
      ndigits = 0
      x1 = 0
      do while(x1.le.9)
        if (which_digits(x1).eq.1) then
          ndigits = ndigits + 1
          digit(ndigits) = x1
        endif
        x1 = x1 + 1
      enddo
      write(*,*) 'minimum number of digits used ',ndigits
      write(*,fmt='(10(I2))') (digit(x1), x1=1,ndigits)

c get a permutation of the digits used (in this case 8 digits)
      do x1=1,8
        do x2=1,8
          if (x1.ne.x2) then
          do x3=1,8
            if ((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=1,8
              if ((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=1,8
                if ((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                do x6=1,8
                  if ((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                  do x7=1,8
                    if ((x1.ne.x7).and.(x2.ne.x7).and.(x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                    do x8=1,8
                      if ((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8)
     .    .and.(x7.ne.x8)) then
     
      digit_test(1) = digit(x1)
      digit_test(2) = digit(x2)
      digit_test(3) = digit(x3)
      digit_test(4) = digit(x4)
      digit_test(5) = digit(x5)
      digit_test(6) = digit(x6)
      digit_test(7) = digit(x7)
      digit_test(8) = digit(x8)
c      write(*,fmt='(10(I2))') (digit_test(x10), x10=1,ndigits)
      
      found_result = .true.
      x10 = 1
      do while((found_result).and.(x10.le.50))
        code_works = .false.
        do x11=1,6
          do x12=x11+1,7
            do x13=x12+1,8
              if ((digit_test(x11).eq.codes(x10,1)).and.(digit_test(x12).eq.codes(x10,2)).and.
     .              (digit_test(x13).eq.codes(x10,3))) code_works = .true.
            enddo
          enddo
        enddo
        if (.not. code_works) then
          found_result = .false.
        else
          x10 = x10 + 1
        endif
      enddo
      
      if (found_result) then
        write(*,*) 'The correct code is:'
        write(*,fmt='(8(I1))') (digit_test(x9), x9=1,8)
        stop
      endif

                      endif
                    enddo
                    endif
                  enddo
                  endif
                enddo
                endif
              enddo
              endif
            enddo
            endif
          enddo
          endif
        enddo
      enddo      
      
      write(*,*) 'made it here for some reason'
      
      end