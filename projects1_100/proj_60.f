c-------------------------------------------------------------------------------------------------c
      program project_euler_60
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and              c
c  concatenating them in any order the result will always be prime. For example, taking 7 and     c
c  109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest    c
c  sum for a set of four primes with this property.                                               c
c                                                                                                 c
c  Find the lowest sum for a set of five primes for which any two primes concatenate to produce   c
c  another prime.                                                                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 primes(4202), p_index, power(4202), result
      integer*8 test1, test2, test3, test4, test5, test6, test7, test8
      logical combo1, combo2, combo3, combo4, combo5, combo6, combo7, combo8

c set up the primes
      p_index = 0
      x1 = 3
      do while(p_index.lt.4202)
        if(is_prime(x1)) then
          p_index = p_index + 1
          primes(p_index) = x1
          
          if (x1.lt.10) then
            power(p_index) = 1
          else if (x1.lt.100) then
            power(p_index) = 2
          else if (x1.lt.1000) then
            power(p_index) = 3
          else if (x1.lt.10000) then
            power(p_index) = 4
          else if (x1.lt.100000) then
            power(p_index) = 5
          endif
        endif
        x1 = x1 + 2
      enddo
      result = 1000000000

c loop over the primes to find one pair of primes that work
      do x1=1,4202-4
        do x2=x1+1,4202-3
          if (primes(x1)+primes(x2).gt.result) exit
          test1 = primes(x1)*10**(power(x2)) + primes(x2)
          test2 = primes(x2)*10**(power(x1)) + primes(x1)

c check to see if test1 is prime
          combo1 = .true.
          do x9=1,4202
            if (((test1/primes(x9))*primes(x9).eq.test1).and.(primes(x9).lt.test1)) then
              combo1 = .false.
              exit
            endif
            if (primes(x9).gt.test1) exit
          enddo
          
          if (combo1) then
c check to see if test2 is prime
            combo2 = .true.
            do x9=1,4202
              if (((test2/primes(x9))*primes(x9).eq.test2).and.(primes(x9).lt.test2)) then
                combo2 = .false.
                exit
              endif
              if (primes(x9).gt.test2) exit
            enddo
          endif

c loop over the third prime          
          if ((combo1.and.combo2).and.
     .        ((primes(x1)+primes(x2)).lt.result)) then
            do x3=x2+1,4202-2
              if ((primes(x1)+primes(x2)+primes(x3)).gt.result) exit
              test1 = primes(x1)*10**(power(x3)) + primes(x3)
              test2 = primes(x3)*10**(power(x1)) + primes(x1)
              test3 = primes(x2)*10**(power(x3)) + primes(x3)
              test4 = primes(x3)*10**(power(x2)) + primes(x2)

c check to see if test1 is prime
              combo1 = .true.
              do x9=1,4202
                if (((test1/primes(x9))*primes(x9).eq.test1).and.(primes(x9).lt.test1)) then
                  combo1 = .false.
                  exit
                endif
                if (primes(x9).gt.test1) exit
              enddo
          
              if (combo1) then
c check to see if test2 is prime
                combo2 = .true.
                do x9=1,4202
                  if (((test2/primes(x9))*primes(x9).eq.test2).and.(primes(x9).lt.test2)) then
                    combo2 = .false.
                    exit
                  endif
                  if (primes(x9).gt.test2) exit
                enddo
              endif

              if (combo1.and.combo2) then
c check to see if test3 is prime
                combo3 = .true.
                do x9=1,4202
                  if (((test3/primes(x9))*primes(x9).eq.test3).and.(primes(x9).lt.test3)) then
                    combo3 = .false.
                    exit
                  endif
                  if (primes(x9).gt.test3) exit
                enddo
              endif
          
              if (combo1.and.combo2.and.combo3) then
c check to see if test4 is prime
                combo4 = .true.
                do x9=1,4202
                  if (((test4/primes(x9))*primes(x9).eq.test4).and.(primes(x9).lt.test4)) then
                    combo4 = .false.
                    exit
                  endif
                  if (primes(x9).gt.test4) exit
                enddo
              endif

c loop over the 4th prime              
              if ((combo1.and.combo2.and.combo3.and.combo4).and.
     .            ((primes(x1)+primes(x2)+primes(x3)).lt.result)) then
                do x4=x3+1,4202-1
                  if ((primes(x1)+primes(x2)+primes(x3)+primes(x4)).gt.result) exit
                  test1 = primes(x1)*10**(power(x4)) + primes(x4)
                  test2 = primes(x4)*10**(power(x1)) + primes(x1)
                  test3 = primes(x2)*10**(power(x4)) + primes(x4)
                  test4 = primes(x4)*10**(power(x2)) + primes(x2)
                  test5 = primes(x3)*10**(power(x4)) + primes(x4)
                  test6 = primes(x4)*10**(power(x3)) + primes(x3)

c check to see if test1 is prime
                  combo1 = .true.
                  do x9=1,4202
                    if (((test1/primes(x9))*primes(x9).eq.test1).and.(primes(x9).lt.test1)) then
                      combo1 = .false.
                      exit
                    endif
                    if (primes(x9).gt.test1) exit
                  enddo
          
                  if (combo1) then
c check to see if test2 is prime
                    combo2 = .true.
                    do x9=1,4202
                      if (((test2/primes(x9))*primes(x9).eq.test2).and.(primes(x9).lt.test2)) then
                        combo2 = .false.
                        exit
                      endif
                      if (primes(x9).gt.test2) exit
                    enddo
                  endif

                  if (combo1.and.combo2) then
c check to see if test3 is prime
                    combo3 = .true.
                    do x9=1,4202
                      if (((test3/primes(x9))*primes(x9).eq.test3).and.(primes(x9).lt.test3)) then
                        combo3 = .false.
                        exit
                      endif
                      if (primes(x9).gt.test3) exit
                    enddo
                  endif
          
                  if (combo1.and.combo2.and.combo3) then
c check to see if test4 is prime
                    combo4 = .true.
                    do x9=1,4202
                      if (((test4/primes(x9))*primes(x9).eq.test4).and.(primes(x9).lt.test4)) then
                        combo4 = .false.
                        exit
                      endif
                      if (primes(x9).gt.test4) exit
                    enddo
                  endif
                  
                  if (combo1.and.combo2.and.combo3.and.combo4) then
c check to see if test5 is prime
                    combo5 = .true.
                    do x9=1,4202
                      if (((test5/primes(x9))*primes(x9).eq.test5).and.(primes(x9).lt.test5)) then
                        combo5 = .false.
                        exit
                      endif
                      if (primes(x9).gt.test5) exit
                    enddo
                  endif
          
                  if (combo1.and.combo2.and.combo3.and.combo4.and.combo5) then
c check to see if test6 is prime
                    combo6 = .true.
                    do x9=1,4202
                      if (((test6/primes(x9))*primes(x9).eq.test6).and.(primes(x9).lt.test6)) then
                        combo6 = .false.
                        exit
                      endif
                      if (primes(x9).gt.test6) exit
                    enddo
                  endif

c loop over the 5th prime                  
                  if ((combo1.and.combo2.and.combo3.and.combo4.and.combo5.and.combo6).and.
     .                ((primes(x1)+primes(x2)+primes(x3)+primes(x4)).lt.result)) then
                    write(*,fmt='(4(10I,1X))') primes(x1),primes(x2),primes(x3),primes(x4)
                    do x5=x4+1,4202
                      if ((primes(x1)+primes(x2)+primes(x3)+primes(x4)+primes(x5)).gt.result) exit
                      test1 = primes(x1)*10**(power(x5)) + primes(x5)
                      test2 = primes(x5)*10**(power(x1)) + primes(x1)
                      test3 = primes(x2)*10**(power(x5)) + primes(x5)
                      test4 = primes(x5)*10**(power(x2)) + primes(x2)
                      test5 = primes(x3)*10**(power(x5)) + primes(x5)
                      test6 = primes(x5)*10**(power(x3)) + primes(x3)                   
                      test7 = primes(x4)*10**(power(x5)) + primes(x5)
                      test8 = primes(x5)*10**(power(x4)) + primes(x4)
                      
c check to see if test1 is prime
                      combo1 = .true.
                      do x9=1,4202
                        if (((test1/primes(x9))*primes(x9).eq.test1).and.(primes(x9).lt.test1)) then
                          combo1 = .false.
                          exit
                        endif
                        if (primes(x9).gt.test1) exit
                      enddo
          
                      if (combo1) then
c check to see if test2 is prime
                        combo2 = .true.
                        do x9=1,4202
                          if (((test2/primes(x9))*primes(x9).eq.test2).and.(primes(x9).lt.test2)) then
                            combo2 = .false.
                            exit
                          endif
                          if (primes(x9).gt.test2) exit
                        enddo
                      endif

                      if (combo1.and.combo2) then
c check to see if test3 is prime
                        combo3 = .true.
                        do x9=1,4202
                          if (((test3/primes(x9))*primes(x9).eq.test3).and.(primes(x9).lt.test3)) then
                            combo3 = .false.
                            exit
                          endif
                          if (primes(x9).gt.test3) exit
                        enddo
                      endif
          
                      if (combo1.and.combo2.and.combo3) then
c check to see if test4 is prime
                        combo4 = .true.
                        do x9=1,4202
                          if (((test4/primes(x9))*primes(x9).eq.test4).and.(primes(x9).lt.test4)) then
                            combo4 = .false.
                            exit
                          endif
                          if (primes(x9).gt.test4) exit
                        enddo
                      endif
                  
                      if (combo1.and.combo2.and.combo3.and.combo4) then
c check to see if test5 is prime
                        combo5 = .true.
                        do x9=1,4202
                          if (((test5/primes(x9))*primes(x9).eq.test5).and.(primes(x9).lt.test5)) then
                            combo5 = .false.
                            exit
                          endif
                          if (primes(x9).gt.test5) exit
                        enddo
                      endif
          
                      if (combo1.and.combo2.and.combo3.and.combo4.and.combo5) then
c check to see if test6 is prime
                        combo6 = .true.
                        do x9=1,4202
                          if (((test6/primes(x9))*primes(x9).eq.test6).and.(primes(x9).lt.test6)) then
                            combo6 = .false.
                            exit
                          endif
                          if (primes(x9).gt.test6) exit
                        enddo
                      endif
                  
                      if (combo1.and.combo2.and.combo3.and.combo4.and.combo5.and.combo6) then
c check to see if test7 is prime
                        combo7 = .true.
                        do x9=1,4202
                          if (((test7/primes(x9))*primes(x9).eq.test7).and.(primes(x9).lt.test7)) then
                            combo7 = .false.
                            exit
                          endif
                          if (primes(x9).gt.test7) exit
                        enddo
                      endif
          
                      if (combo1.and.combo2.and.combo3.and.combo4.and.combo5.and.combo6.and.combo7) then
c check to see if test8 is prime
                        combo8 = .true.
                        do x9=1,4202
                          if (((test8/primes(x9))*primes(x9).eq.test8).and.(primes(x9).lt.test8)) then
                            combo8 = .false.
                            exit
                          endif
                          if (primes(x9).gt.test8) exit
                        enddo
                      endif
                  
                      if (combo1.and.combo2.and.combo3.and.combo4.and.combo5.and.combo6.and.combo7.and.combo8) then
                        write(*,*) 'SOLUTION FOUND'
                        write(*,*) primes(x1),primes(x2),primes(x3),primes(x4),primes(x5)
                        write(*,*) 'sum is ',primes(x1)+primes(x2)+primes(x3)+primes(x4)+primes(x5)
                        if (primes(x1)+primes(x2)+primes(x3)+primes(x4)+primes(x5).lt.result) then
                          result = primes(x1)+primes(x2)+primes(x3)+primes(x4)+primes(x5)
                        endif
                      endif
                    enddo
                  endif
                enddo
              endif
            enddo
          endif
        enddo
      enddo
      
      write(*,*) 'The minimum sum of primes is ',result
      
      end