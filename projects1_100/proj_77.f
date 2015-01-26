c-------------------------------------------------------------------------------------------------c
      program project_euler_77
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It is possible to write ten as the sum of primes in exactly five different ways:               c
c                                                                                                 c
c  7 + 3                                                                                          c
c  5 + 5                                                                                          c
c  5 + 3 + 2                                                                                      c
c  3 + 3 + 2 + 2                                                                                  c
c  2 + 2 + 2 + 2 + 2                                                                              c
c                                                                                                 c
c  What is the first value which can be written as the sum of primes in over five thousand        c
c  different ways?                                                                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c  Parameters used in this program
      integer*8 p(25), index, target, num_combos, test
      logical done
      
c  tabulate the primes
      index = 0
      do x1=1,100
        if (is_prime(x1)) then
          index = index + 1
          p(index) = x1
        endif
      enddo
      
c  Loop over all the numbers to find the prime combos
      target = 2
      done = .false.
      do while(.not.done)
        num_combos = 0
        do x1=0,target/p(1)
          test = x1*p(1)
          do x2=0,(target-test)/p(2)
            test = x1*p(1) + x2*p(2)
            do x3=0,(target-test)/p(3)
              test = x1*p(1) + x2*p(2) + x3*p(3)
              do x4=0,(target-test)/p(4)
                test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4)
                do x5=0,(target-test)/p(5)
                  test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5)
                  do x6=0,(target-test)/p(6)
                    test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6)
                    do x7=0,(target-test)/p(7)
                      test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7)
                      do x8=0,(target-test)/p(8)
                        test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
                        do x9=0,(target-test)/p(9)
                          test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9)
                          do x10=0,(target-test)/p(10)
                            test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10)

        do x11=0,(target-test)/p(11)
          test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11)
          do x12=0,(target-test)/p(12)
            test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12)
            do x13=0,(target-test)/p(13)
              test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13)
              do x14=0,(target-test)/p(14)
                test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14)
                do x15=0,(target-test)/p(15)
                  test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15)
                  do x16=0,(target-test)/p(16)
                    test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
                    do x17=0,(target-test)/p(17)
                      test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17)
                      do x18=0,(target-test)/p(18)
                        test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18)
                        do x19=0,(target-test)/p(19)
                          test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18) + x19*p(19)
                          do x20=0,(target-test)/p(20)
                            test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18) + x19*p(19) + x20*p(20)

        do x21=0,(target-test)/p(21)
          test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18) + x19*p(19) + x20*p(20) + x21*p(21)
          do x22=0,(target-test)/p(22)
            test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18) + x19*p(19) + x20*p(20) + x21*p(21) + x22*p(22)
            do x23=0,(target-test)/p(23)
              test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18) + x19*p(19) + x20*p(20) + x21*p(21) + x22*p(22) + x23*p(23)
              do x24=0,(target-test)/p(24)
                test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18) + x19*p(19) + x20*p(20) + x21*p(21) + x22*p(22) + x23*p(23) + x24*p(24)
                do x25=0,(target-test)/p(25)
                  test = x1*p(1) + x2*p(2) + x3*p(3) + x4*p(4) + x5*p(5) + x6*p(6) + x7*p(7) + x8*p(8)
     . + x9*p(9) + x10*p(10) + x11*p(11) + x12*p(12) + x13*p(13) + x14*p(14) + x15*p(15) + x16*p(16)
     . + x17*p(17) + x18*p(18) + x19*p(19) + x20*p(20) + x21*p(21) + x22*p(22) + x23*p(23) + x24*p(24)
     . + x25*p(25)
                  
                  if (test.eq.target) num_combos = num_combos + 1
                   
                enddo
              enddo
            enddo
          enddo
        enddo
                          enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
        
                          enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
            
        write(*,*) target, num_combos
        if ((num_combos.gt.5000).or.(target.eq.100)) then
          write(*,*) 'DONE'
          done = .true.
        else
          target = target + 1
        endif
      enddo

      end