c-------------------------------------------------------------------------------------------------c
      program project_euler_191
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A particular school offers cash rewards to children with good attendance and punctuality. If   c
c  they are absent for three consecutive days or late on more than one occasion then they forfeit c
c  their prize.                                                                                   c
c                                                                                                 c
c  During an n-day period a trinary string is formed for each child consisting of L's (late), O's c
c  (on time), and A's (absent).                                                                   c
c                                                                                                 c
c  Although there are eighty-one trinary strings for a 4-day period that can be formed, exactly   c
c  forty-three strings would lead to a prize:                                                     c
c                                                                                                 c
c      OOOO OOOA OOOL OOAO OOAA OOAL OOLO OOLA OAOO OAOA                                          c
c      OAOL OAAO OAAL OALO OALA OLOO OLOA OLAO OLAA AOOO                                          c
c      AOOA AOOL AOAO AOAA AOAL AOLO AOLA AAOO AAOA AAOL                                          c
c      AALO AALA ALOO ALOA ALAO ALAA LOOO LOOA LOAO LOAA                                          c
c      LAOO LAOA LAAO                                                                             c
c                                                                                                 c
c  How many "prize" strings exist over a 30-day period?                                           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 totals(30), result
      
c initialize parameters
      do x1=1,30
        totals(x1) = 0
      enddo
      
c looooooooops
c on time = 1, absent = 2
      do x1=1,2
        totals(1) = totals(1) + 1
        do x2=1,2
          totals(2) = totals(2) + 1
          do x3=1,2
          if ((x1.eq.1).or.(x2.eq.1).or.(x3.eq.1)) then
            totals(3) = totals(3) + 1
            do x4=1,2
            if ((x2.eq.1).or.(x3.eq.1).or.(x4.eq.1)) then
              totals(4) = totals(4) + 1
              do x5=1,2
              if ((x3.eq.1).or.(x4.eq.1).or.(x5.eq.1)) then
                totals(5) = totals(5) + 1
                do x6=1,2
                if ((x4.eq.1).or.(x5.eq.1).or.(x6.eq.1)) then
                  totals(6) = totals(6) + 1
                  do x7=1,2
                  if ((x5.eq.1).or.(x6.eq.1).or.(x7.eq.1)) then
                    totals(7) = totals(7) + 1
                    do x8=1,2
                    if ((x6.eq.1).or.(x7.eq.1).or.(x8.eq.1)) then
                      totals(8) = totals(8) + 1
                      do x9=1,2
                      if ((x7.eq.1).or.(x8.eq.1).or.(x9.eq.1)) then
                        totals(9) = totals(9) + 1
                        do x10=1,2
                        if ((x8.eq.1).or.(x9.eq.1).or.(x10.eq.1)) then
                          totals(10) = totals(10) + 1

      do x11=1,2
      if ((x9.eq.1).or.(x10.eq.1).or.(x11.eq.1)) then
        totals(11) = totals(11) + 1
        do x12=1,2
        if ((x10.eq.1).or.(x11.eq.1).or.(x12.eq.1)) then
          totals(12) = totals(12) + 1
          do x13=1,2
          if ((x11.eq.1).or.(x12.eq.1).or.(x13.eq.1)) then
            totals(13) = totals(13) + 1
            do x14=1,2
            if ((x12.eq.1).or.(x13.eq.1).or.(x14.eq.1)) then
              totals(14) = totals(14) + 1
              do x15=1,2
              if ((x13.eq.1).or.(x14.eq.1).or.(x15.eq.1)) then
                totals(15) = totals(15) + 1
                do x16=1,2
                if ((x14.eq.1).or.(x15.eq.1).or.(x16.eq.1)) then
                  totals(16) = totals(16) + 1
                  do x17=1,2
                  if ((x15.eq.1).or.(x16.eq.1).or.(x17.eq.1)) then
                    totals(17) = totals(17) + 1
                    do x18=1,2
                    if ((x16.eq.1).or.(x17.eq.1).or.(x18.eq.1)) then
                      totals(18) = totals(18) + 1
                      do x19=1,2
                      if ((x17.eq.1).or.(x18.eq.1).or.(x19.eq.1)) then
                        totals(19) = totals(19) + 1
                        do x20=1,2
                        if ((x18.eq.1).or.(x19.eq.1).or.(x20.eq.1)) then
                          totals(20) = totals(20) + 1

      do x21=1,2
      if ((x19.eq.1).or.(x20.eq.1).or.(x21.eq.1)) then
        totals(21) = totals(21) + 1
        do x22=1,2
        if ((x20.eq.1).or.(x21.eq.1).or.(x22.eq.1)) then
          totals(22) = totals(22) + 1
          do x23=1,2
          if ((x21.eq.1).or.(x22.eq.1).or.(x23.eq.1)) then
            totals(23) = totals(23) + 1
            do x24=1,2
            if ((x22.eq.1).or.(x23.eq.1).or.(x24.eq.1)) then
              totals(24) = totals(24) + 1
              do x25=1,2
              if ((x23.eq.1).or.(x24.eq.1).or.(x25.eq.1)) then
                totals(25) = totals(25) + 1
                do x26=1,2
                if ((x24.eq.1).or.(x25.eq.1).or.(x26.eq.1)) then
                  totals(26) = totals(26) + 1
                  do x27=1,2
                  if ((x25.eq.1).or.(x26.eq.1).or.(x27.eq.1)) then
                    totals(27) = totals(27) + 1
                    do x28=1,2
                    if ((x26.eq.1).or.(x27.eq.1).or.(x28.eq.1)) then
                      totals(28) = totals(28) + 1
                      do x29=1,2
                      if ((x27.eq.1).or.(x28.eq.1).or.(x29.eq.1)) then
                        totals(29) = totals(29) + 1
                        do x30=1,2
                        if ((x28.eq.1).or.(x29.eq.1).or.(x30.eq.1)) then
                          totals(30) = totals(30) + 1

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
      enddo
      
c time to tabulate the result
      result = 0
      do x1=1,14
        result = result + totals(x1)*totals(29-x1)
      enddo
      result = result + totals(29)
      result = 2*(result) + totals(30)
      
      write(*,*) 'The result is ',result
      
      end