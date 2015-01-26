c-------------------------------------------------------------------------------------------------c
      program project_euler_59
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,                c
c  where each “_” is a single digit.                                                              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 digits1(1000), digits2(1000), digits3(1000), ndigits1, ndigits2, ndigits3, base
      logical found_result
      
c initialize the base
      base = 10
      
c the limits of the result can be found by taking the square roots of extremes
c 1020304050607080900 ** 0.5 = 1010101010.101010
c 1929394959697989990 ** 0.5 = 1389026623.106264
c Since the last digit of the square is 0 then the square root must end in 0
c therefore 1010101020 < x < 1389026630 and we can increment by 10's
      digits1(1) = 0
      digits1(2) = 2
      digits1(3) = 0
      digits1(4) = 1
      digits1(5) = 0
      digits1(6) = 1
      digits1(7) = 0
      digits1(8) = 1
      digits1(9) = 0
      digits1(10) = 1
      ndigits1 = 10
      
      found_result = .false.
      do while(.not.found_result)

c        write(*,fmt='(10(I1))') (digits1(x1), x1=ndigits1,1,-1)
c copy over the digits to a second set of varialbles
        ndigits2 = ndigits1
        do x1=1,ndigits2
          digits2(x1) = digits1(x1)
        enddo

c square the number        
        call big_number_product(digits1,ndigits1,digits2,ndigits2,digits3,ndigits3)
        if ((digits3(3).eq.9).and.(digits3(5).eq.8).and.(digits3(7).eq.7).and.(digits3(9).eq.6).and.
     .        (digits3(11).eq.5).and.(digits3(13).eq.4).and.(digits3(15).eq.3).and.(digits3(17).eq.2).and.
     .        (digits3(19).eq.1)) then
          found_result = .true.
          write(*,fmt='(10(I1))') (digits1(x1), x1=ndigits1,1,-1)
          write(*,fmt='(19(I1))') (digits3(x1), x1=ndigits3,1,-1)
          stop
        endif
        
c increment by 10
        digits1(2) = digits1(2) + 1
        do x1=2,9
          if (digits1(x1).ge.10) then
            digits1(x1) = digits1(x1) - 10
            digits1(x1+1) = digits1(x1+1) + 1
          endif
        enddo
      enddo
      
      end