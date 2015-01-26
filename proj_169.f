c-------------------------------------------------------------------------------------------------c
      program project_euler_169
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Define f(0)=1 and f(n) to be the number of different ways n can be expressed as a sum of       c
c  integer powers of 2 using each power no more than twice.                                       c
c                                                                                                 c
c  For example, f(10)=5 since there are five different ways to express 10:                        c
c                                                                                                 c
c  1 + 1 + 8                                                                                      c
c  1 + 1 + 4 + 4                                                                                  c
c  1 + 1 + 2 + 2 + 4                                                                              c
c  2 + 4 + 4                                                                                      c
c  2 + 8                                                                                          c
c                                                                                                 c
c  What is f(10^25)?                                                                              c               
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 num(100), test_num(100), carry(100)
      common/passing/ num, test_num, carry
      integer*8 answer
      
c initializing the result and 10^25 in base 2
      answer = 0
c 10^25 in base 2 is:      
c 100001000101100101010001011000010100000000010100100001001010000000000000000000000000_2
      do x1=1,100
        num(x1) = 0
        carry(x1) = 0
      enddo
      num(26) = 1
      num(28) = 1
      num(31) = 1
      num(36) = 1
      num(39) = 1
      num(41) = 1
      num(51) = 1
      num(53) = 1
      num(58) = 1
      num(59) = 1
      num(61) = 1
      num(65) = 1
      num(67) = 1
      num(69) = 1
      num(72) = 1
      num(73) = 1
      num(75) = 1
      num(79) = 1
      num(84) = 1

c loop over all the individual digits and check that the simplified num matches the answer
c digits 1-10
      do x1=0,2
        call adjust_digits(1,x1)
        if (test_num(1).eq.num(1)) then
        do x2=0,2
          call adjust_digits(2,x2)
          if (test_num(2).eq.num(2)) then
          do x3=0,2     
            call adjust_digits(3,x3)
            if (test_num(3).eq.num(3)) then
            do x4=0,2
              call adjust_digits(4,x4)
              if (test_num(4).eq.num(4)) then
              do x5=0,2
                call adjust_digits(5,x5)
                if (test_num(5).eq.num(5)) then
                do x6=0,2
                  call adjust_digits(6,x6)
                  if (test_num(6).eq.num(6)) then
                  do x7=0,2
                    call adjust_digits(7,x7)
                    if (test_num(7).eq.num(7)) then
                    do x8=0,2
                      call adjust_digits(8,x8)
                      if (test_num(8).eq.num(8)) then
                      do x9=0,2
                        call adjust_digits(9,x9)
                        if (test_num(9).eq.num(9)) then
                        do x10=0,2
                          call adjust_digits(10,x10)
                          if (test_num(10).eq.num(10)) then
c digits 11-20
      do x11=0,2
        call adjust_digits(11,x11)
        if (test_num(11).eq.num(11)) then
        do x12=0,2
          call adjust_digits(12,x12)
          if (test_num(12).eq.num(12)) then
          do x13=0,2     
            call adjust_digits(13,x13)
            if (test_num(13).eq.num(13)) then
            do x14=0,2
              call adjust_digits(14,x14)
              if (test_num(14).eq.num(14)) then
              do x15=0,2
                call adjust_digits(15,x15)
                if (test_num(15).eq.num(15)) then
                do x16=0,2
                  call adjust_digits(16,x16)
                  if (test_num(16).eq.num(16)) then
                  do x17=0,2
                    call adjust_digits(17,x17)
                    if (test_num(17).eq.num(17)) then
                    do x18=0,2
                      call adjust_digits(18,x18)
                      if (test_num(18).eq.num(18)) then
                      do x19=0,2
                        call adjust_digits(19,x19)
                        if (test_num(19).eq.num(19)) then
                        do x20=0,2
                          call adjust_digits(20,x20)
                          if (test_num(20).eq.num(20)) then
c digits 21-30
      do x21=0,2
        call adjust_digits(21,x21)
        if (test_num(21).eq.num(21)) then
        do x22=0,2
          call adjust_digits(22,x22)
          if (test_num(22).eq.num(22)) then
          do x23=0,2     
            call adjust_digits(23,x23)
            if (test_num(23).eq.num(23)) then
            do x24=0,2
              call adjust_digits(24,x24)
              if (test_num(24).eq.num(24)) then
              do x25=0,2
                call adjust_digits(25,x25)
                if (test_num(25).eq.num(25)) then
                do x26=0,2
                  call adjust_digits(26,x26)
                  if (test_num(26).eq.num(26)) then
                  do x27=0,2
                    call adjust_digits(27,x27)
                    if (test_num(27).eq.num(27)) then
                    do x28=0,2
                      call adjust_digits(28,x28)
                      if (test_num(28).eq.num(28)) then
                      do x29=0,2
                        call adjust_digits(29,x29)
                        if (test_num(29).eq.num(29)) then
                        do x30=0,2
                          call adjust_digits(30,x30)
                          if (test_num(30).eq.num(30)) then
c digits 31-40
      do x31=0,2
        call adjust_digits(31,x31)
        if (test_num(31).eq.num(31)) then
        do x32=0,2
          call adjust_digits(32,x32)
          if (test_num(32).eq.num(32)) then
          do x33=0,2     
            call adjust_digits(33,x33)
            if (test_num(33).eq.num(33)) then
            do x34=0,2
              call adjust_digits(34,x34)
              if (test_num(34).eq.num(34)) then
              do x35=0,2
                call adjust_digits(35,x35)
                if (test_num(35).eq.num(35)) then
                do x36=0,2
                  call adjust_digits(36,x36)
                  if (test_num(36).eq.num(36)) then
                  do x37=0,2
                    call adjust_digits(37,x37)
                    if (test_num(37).eq.num(37)) then
                    do x38=0,2
                      call adjust_digits(38,x38)
                      if (test_num(38).eq.num(38)) then
                      do x39=0,2
                        call adjust_digits(39,x39)
                        if (test_num(39).eq.num(39)) then
                        do x40=0,2
                          call adjust_digits(40,x40)
                          if (test_num(40).eq.num(40)) then
c digits 41-50
      do x41=0,2
        call adjust_digits(41,x41)
        if (test_num(41).eq.num(41)) then
        do x42=0,2
          call adjust_digits(42,x42)
          if (test_num(42).eq.num(42)) then
          do x43=0,2     
            call adjust_digits(43,x43)
            if (test_num(43).eq.num(43)) then
            do x44=0,2
              call adjust_digits(44,x44)
              if (test_num(44).eq.num(44)) then
              do x45=0,2
                call adjust_digits(45,x45)
                if (test_num(45).eq.num(45)) then
                do x46=0,2
                  call adjust_digits(46,x46)
                  if (test_num(46).eq.num(46)) then
                  do x47=0,2
                    call adjust_digits(47,x47)
                    if (test_num(47).eq.num(47)) then
                    do x48=0,2
                      call adjust_digits(48,x48)
                      if (test_num(48).eq.num(48)) then
                      do x49=0,2
                        call adjust_digits(49,x49)
                        if (test_num(49).eq.num(49)) then
                        do x50=0,2
                          call adjust_digits(50,x50)
                          if (test_num(50).eq.num(50)) then
c digits 51-60
      do x51=0,2
        call adjust_digits(51,x51)
        if (test_num(51).eq.num(51)) then
        do x52=0,2
          call adjust_digits(52,x52)
          if (test_num(52).eq.num(52)) then
          do x53=0,2     
            call adjust_digits(53,x53)
            if (test_num(53).eq.num(53)) then
            do x54=0,2
              call adjust_digits(54,x54)
              if (test_num(54).eq.num(54)) then
              do x55=0,2
                call adjust_digits(55,x55)
                if (test_num(55).eq.num(55)) then
                do x56=0,2
                  call adjust_digits(56,x56)
                  if (test_num(56).eq.num(56)) then
                  do x57=0,2
                    call adjust_digits(57,x57)
                    if (test_num(57).eq.num(57)) then
                    do x58=0,2
                      call adjust_digits(58,x58)
                      if (test_num(58).eq.num(58)) then
                      do x59=0,2
                        call adjust_digits(59,x59)
                        if (test_num(59).eq.num(59)) then
                        do x60=0,2
                          call adjust_digits(60,x60)
                          if (test_num(60).eq.num(60)) then
c digits 61-70
      do x61=0,2
        call adjust_digits(61,x61)
        if (test_num(61).eq.num(61)) then
        do x62=0,2
          call adjust_digits(62,x62)
          if (test_num(62).eq.num(62)) then
          do x63=0,2     
            call adjust_digits(63,x63)
            if (test_num(63).eq.num(63)) then
            do x64=0,2
              call adjust_digits(64,x64)
              if (test_num(64).eq.num(64)) then
              do x65=0,2
                call adjust_digits(65,x65)
                if (test_num(65).eq.num(65)) then
                do x66=0,2
                  call adjust_digits(66,x66)
                  if (test_num(66).eq.num(66)) then
                  do x67=0,2
                    call adjust_digits(67,x67)
                    if (test_num(67).eq.num(67)) then
                    do x68=0,2
                      call adjust_digits(68,x68)
                      if (test_num(68).eq.num(68)) then
                      do x69=0,2
                        call adjust_digits(69,x69)
                        if (test_num(69).eq.num(69)) then
                        do x70=0,2
                          call adjust_digits(70,x70)
                          if (test_num(70).eq.num(70)) then
c digits 71-80
      do x71=0,2
        call adjust_digits(71,x71)
        if (test_num(71).eq.num(71)) then
        do x72=0,2
          call adjust_digits(72,x72)
          if (test_num(72).eq.num(72)) then
          do x73=0,2     
            call adjust_digits(73,x73)
            if (test_num(73).eq.num(73)) then
            do x74=0,2
              call adjust_digits(74,x74)
              if (test_num(74).eq.num(74)) then
              do x75=0,2
                call adjust_digits(75,x75)
                if (test_num(75).eq.num(75)) then
                do x76=0,2
                  call adjust_digits(76,x76)
                  if (test_num(76).eq.num(76)) then
                  do x77=0,2
                    call adjust_digits(77,x77)
                    if (test_num(77).eq.num(77)) then
                    do x78=0,2
                      call adjust_digits(78,x78)
                      if (test_num(78).eq.num(78)) then
                      do x79=0,2
                        call adjust_digits(79,x79)
                        if (test_num(79).eq.num(79)) then
                        do x80=0,2
                          call adjust_digits(80,x80)
                          if (test_num(80).eq.num(80)) then
c digits 81-85
      do x81=0,2
        call adjust_digits(81,x81)
        if (test_num(81).eq.num(81)) then
        do x82=0,2
          call adjust_digits(82,x82)
          if (test_num(82).eq.num(82)) then
          do x83=0,2     
            call adjust_digits(83,x83)
            if (test_num(83).eq.num(83)) then
            do x84=0,2
              call adjust_digits(84,x84)
              if (test_num(84).eq.num(84)) then
              do x85=0,0
                call adjust_digits(85,x85)
                if (test_num(85).eq.num(85)) then
                  answer = answer + 1
                  if (((answer/10000)*10000).eq.answer) then
                    write(*,*) answer
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
c end digits 71-80
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
c end digits 61-70
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
c end digits 51-60
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
c end digits 41-50
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
c end digits 31-40
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
c end digits 21-30
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
c end digits 11-20
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
c end digits 1-10
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
      
      write(*,*) 'number of possible ways is ',answer

      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      subroutine adjust_digits(index,value)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine takes the index of the digit that we're working on and what value of the digit c
c  to get the test_num digits so we can compare to 10^25 in base 2.                               c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters
      integer*8 index, value
      
c common blocks
      integer*8 num(100), test_num(100), carry(100)
      common/passing/ num, test_num, carry
      
      if (value+carry(index).ge.2) then
        test_num(index) = value+carry(index)-2
        carry(index+1) = 1
      else
        test_num(index) = value+carry(index)
        carry(index+1) = 0
      endif
      
      return
      end