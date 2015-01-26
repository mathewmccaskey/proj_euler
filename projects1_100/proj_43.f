c-------------------------------------------------------------------------------------------------c
      program project_euler_43
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the     c
c  digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility      c
c  property.                                                                                      c
c                                                                                                 c
c  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:   c
c                                                                                                 c
c    d_2d_3d_4=406 is divisible by 2                                                              c
c    d_3d_4d_5=063 is divisible by 3                                                              c
c    d_4d_5d_6=635 is divisible by 5                                                              c
c    d_5d_6d_7=357 is divisible by 7                                                              c
c    d_6d_7d_8=572 is divisible by 11                                                             c
c    d_7d_8d_9=728 is divisible by 13                                                             c
c    d_8d_9d_10=289 is divisible by 17                                                            c
c                                                                                                 c
c  Find the sum of all 0 to 9 pandigital numbers with this property.                              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 digit(15), number
      logical correct_number
      
c initialize the digits
      do x1=1,15
        digit(x1) = 0
      enddo
      
c get a 10 digit pan-digital number
      do x1=9,0,-1
        do x2=9,0,-1
          if (x1.ne.x2) then
          do x3=9,0,-1
            if ((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=9,0,-1
              if ((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=9,0,-1
                if ((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                do x6=9,0,-1
                  if ((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                  do x7=9,0,-1
                    if ((x1.ne.x7).and.(x2.ne.x7).and.(x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                    do x8=9,0,-1
                      if ((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8)
     .    .and.(x7.ne.x8)) then
                      do x9=9,0,-1
                        if ((x1.ne.x9).and.(x2.ne.x9).and.(x3.ne.x9).and.(x4.ne.x9).and.(x5.ne.x9).and.(x6.ne.x9)
     .    .and.(x7.ne.x9).and.(x8.ne.x9)) then
                        do x10=9,0,-1
                          if ((x1.ne.x10).and.(x2.ne.x10).and.(x3.ne.x10).and.(x4.ne.x10).and.(x5.ne.x10)
     .    .and.(x6.ne.x10).and.(x7.ne.x10).and.(x8.ne.x10).and.(x9.ne.x10)) then

c now that we have a 10 digit pan-digital number now we check all the different criteria
      correct_number = .true.
c    d_2 d_3 d_4  is divisible by 2
      number = x2*100 + x3*10 + x4
      if (2*(number/2).ne.number) correct_number = .false.

c    d_3 d_4 d_5 is divisible by 3
      number = x3*100 + x4*10 + x5
      if (3*(number/3).ne.number) correct_number = .false.
      
c    d_4 d_5 d_6 is divisible by 5
      number = x4*100 + x5*10 + x6
      if (5*(number/5).ne.number) correct_number = .false.
      
c    d_5 d_6 d_7 is divisible by 7
      number = x5*100 + x6*10 + x7
      if (7*(number/7).ne.number) correct_number = .false.
      
c    d_6 d_7 d_8 is divisible by 11
      number = x6*100 + x7*10 + x8
      if (11*(number/11).ne.number) correct_number = .false.
      
c    d_7 d_8 d_9 is divisible by 13
      number = x7*100 + x8*10 + x9
      if (13*(number/13).ne.number) correct_number = .false.
      
c    d_8 d_9 d_10 is divisible by 17
      number = x8*100 + x9*10 + x10
      if (17*(number/17).ne.number) correct_number = .false.
      
      if(correct_number) then
        write(*,fmt='(10(I1))') x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
        digit(1) = digit(1) + x10
        digit(2) = digit(2) + x9
        digit(3) = digit(3) + x8
        digit(4) = digit(4) + x7
        digit(5) = digit(5) + x6
        digit(6) = digit(6) + x5
        digit(7) = digit(7) + x4
        digit(8) = digit(8) + x3
        digit(9) = digit(9) + x2
        digit(10) = digit(10) + x1
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
            endif
          enddo
          endif
        enddo
      enddo

c after the loop is done then we clean up the final number
      do x1=1,10
        do while (digit(x1).ge.10)
          digit(x1) = digit(x1) - 10
          digit(x1+1) = digit(x1+1) + 1
        enddo
      enddo
      
      write(*,*) 'The sum of all the correct 10-digit pandigital numbers is '
      write(*,fmt='(15(I1))') (digit(x1), x1=15,1,-1)

      end