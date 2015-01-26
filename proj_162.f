c-------------------------------------------------------------------------------------------------c
      program project_euler_162
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  In the hexadecimal number system numbers are represented using 16 different digits:            c
c  0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F                                                                c
c                                                                                                 c
c  The hexadecimal number AF when written in the decimal number system equals 10x16+15=175.       c
c                                                                                                 c
c  In the 3-digit hexadecimal numbers 10A, 1A0, A10, and A01 the digits 0,1 and A are all present.c
c  Like numbers written in base ten we write hexadecimal numbers without leading zeroes.          c
c                                                                                                 c
c  How many hexadecimal numbers containing at most sixteen hexadecimal digits exist with all of   c
c  the digits 0,1, and A present at least once?                                                   c
c  Give your answer as a hexadecimal number.                                                      c
c                                                                                                 c
c  (A,B,C,D,E and F in upper case, without any leading or trailing code that marks the number as  c
c  hexadecimal and without leading zeroes , e.g. 1A3F and not: 1a3f and not 0x1a3f and not $1A3F  c
c  and not #1A3F and not 0000001A3F)                                                              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program
      integer*8 result(16), x10, x11, x12, x13, x14, x15, x16, i
      
c initialize the result
      do x1=1,16
        result(x1) = 0
      enddo
      
c loop over all the integers making sure that each integer is unique
      do x1=0,15
        do x2=0,15
      if (x1.ne.x2) then
          do x3=0,15
      if ((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=0,15
      if ((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=0,15
      if ((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                do x6=0,15
      if ((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                  do x7=0,15
      if ((x1.ne.x7).and.(x2.ne.x7).and.(x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                    do x8=0,15
      if ((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8).and.(x7.ne.x8)) then
                      do x9=0,15
      if ((x1.ne.x9).and.(x2.ne.x9).and.(x3.ne.x9).and.(x4.ne.x9).and.(x5.ne.x9).and.(x6.ne.x9).and.(x7.ne.x9)
     .    .and.(x8.ne.x9)) then
                        do x10=0,15
      if ((x1.ne.x10).and.(x2.ne.x10).and.(x3.ne.x10).and.(x4.ne.x10).and.(x5.ne.x10).and.(x6.ne.x10).and.(x7.ne.x10)
     .    .and.(x8.ne.x10).and.(x9.ne.x10)) then
                          do x11=0,15
      if ((x1.ne.x11).and.(x2.ne.x11).and.(x3.ne.x11).and.(x4.ne.x11).and.(x5.ne.x11).and.(x6.ne.x11).and.(x7.ne.x11)
     .    .and.(x8.ne.x11).and.(x9.ne.x11).and.(x10.ne.x11)) then
                            do x12=0,15
      if ((x1.ne.x12).and.(x2.ne.x12).and.(x3.ne.x12).and.(x4.ne.x12).and.(x5.ne.x12).and.(x6.ne.x12).and.(x7.ne.x12)
     .    .and.(x8.ne.x12).and.(x9.ne.x12).and.(x10.ne.x12).and.(x11.ne.x12)) then
                              do x13=0,15
      if ((x1.ne.x13).and.(x2.ne.x13).and.(x3.ne.x13).and.(x4.ne.x13).and.(x5.ne.x13).and.(x6.ne.x13).and.(x7.ne.x13)
     .    .and.(x8.ne.x13).and.(x9.ne.x13).and.(x10.ne.x13).and.(x11.ne.x13).and.(x12.ne.x13)) then
                                do x14=0,15
      if ((x1.ne.x14).and.(x2.ne.x14).and.(x3.ne.x14).and.(x4.ne.x14).and.(x5.ne.x14).and.(x6.ne.x14).and.(x7.ne.x14)
     .    .and.(x8.ne.x14).and.(x9.ne.x14).and.(x10.ne.x14).and.(x11.ne.x14).and.(x12.ne.x14).and.(x13.ne.x14)) then
                                  do x15=0,15
      if ((x1.ne.x15).and.(x2.ne.x15).and.(x3.ne.x15).and.(x4.ne.x15).and.(x5.ne.x15).and.(x6.ne.x15).and.(x7.ne.x15)
     .    .and.(x8.ne.x15).and.(x9.ne.x15).and.(x10.ne.x15).and.(x11.ne.x15).and.(x12.ne.x15).and.(x13.ne.x15)
     .    .and.(x14.ne.x15)) then
                                    do x16=0,15
      if ((x1.ne.x16).and.(x2.ne.x16).and.(x3.ne.x16).and.(x4.ne.x16).and.(x5.ne.x16).and.(x6.ne.x16).and.(x7.ne.x16)
     .    .and.(x8.ne.x16).and.(x9.ne.x16).and.(x10.ne.x16).and.(x11.ne.x16).and.(x12.ne.x16).and.(x13.ne.x16)
     .    .and.(x14.ne.x16).and.(x15.ne.x16)) then
                                      result(1) = result(1) + 1
                                      do i=1,16
                                        if (result(i).eq.16) then
                                          result(i) = result(i) - 16
                                          result(i+1) = result(i+1) + 1
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
      
      write(*,fmt='(16(2I,1X))') (result(x1), x1=16,1,-1)
      
      end