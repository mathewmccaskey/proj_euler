c-------------------------------------------------------------------------------------------------c
      program project_euler_24
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A permutation is an ordered arrangement of objects. For example, 3124 is one possible          c
c  permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or  c
c  alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2   c
c  are:                                                                                           c
c                                                                                                 c
c  012   021   102   120   201   210                                                              c
c                                                                                                 c
c  What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9? c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none

c counting integers
      integer x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, counter
      logical valid_combo
      
c initialize the counter
      counter = 0
      
      do x1=0,9
        do x2=0,9
          if (x1.ne.x2) then
          do x3=0,9
            if ((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=0,9
              if ((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=0,9
                if ((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                do x6=0,9
                  if ((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                  do x7=0,9
                    if ((x1.ne.x7).and.(x2.ne.x7).and.(x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                    do x8=0,9
                      if ((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8)
     .      .and.(x7.ne.x8)) then
                      do x9=0,9
                        if ((x1.ne.x9).and.(x2.ne.x9).and.(x3.ne.x9).and.(x4.ne.x9).and.(x5.ne.x9).and.(x6.ne.x9)
     .      .and.(x7.ne.x9).and.(x8.ne.x9)) then      
                        do x10=0,9
                          if ((x1.ne.x10).and.(x2.ne.x10).and.(x3.ne.x10).and.(x4.ne.x10).and.(x5.ne.x10).and.(x6.ne.x10)
     .      .and.(x7.ne.x10).and.(x8.ne.x10).and.(x9.ne.x10)) then
                            counter = counter + 1
                           if (counter.eq.1000000) then                          
c                            if (counter.eq.10) then
                              write(*,fmt='(10(I1))') x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
                              pause
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
      
      end