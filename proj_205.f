c-------------------------------------------------------------------------------------------------c
      program project_euler_205
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.               c
c  Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.               c
c                                                                                                 c
c  Peter and Colin roll their dice and compare totals: the highest total wins. The result is a    c
c  draw if the totals are equal.                                                                  c
c                                                                                                 c
c  What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer rounded to     c
c  seven decimal places in the form 0.abcdefg                                                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer d6_combo(36), d4_combo(36)
      double precision prob
      
c initilaize the combos
      do x1=1,36
        d6_combo(x1) = 0
        d4_combo(x1) = 0
      enddo
      
c tabulate the number of combinations for d6's and d4's
      do x1=1,6
        do x2=1,6
          do x3=1,6
            do x4=1,6
              do x5=1,6
                do x6=1,6
                  d6_combo(x1+x2+x3+x4+x5+x6) = d6_combo(x1+x2+x3+x4+x5+x6) + 1
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      
      do x1=1,4
        do x2=1,4
          do x3=1,4
            do x4=1,4
              do x5=1,4
                do x6=1,4
                  do x7=1,4
                    do x8=1,4
                      do x9=1,4
                        d4_combo(x1+x2+x3+x4+x5+x6+x7+x8+x9) = d4_combo(x1+x2+x3+x4+x5+x6+x7+x8+x9) + 1
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo

c the d4's go from 9-36 while the d6's go from 6-36
c we loop over all the d4 values and add up the probabilities that it's greater than a d6 roll
      prob = 0.d0
      do x1=9,36
        do x2=6,x1-1
          prob = prob + d4_combo(x1)*d6_combo(x2)/((4.d0**9)*(6.d0**6))
        enddo
      enddo
      
      write(*,*) 'The probability that 4 9-sided die will beat 6 6-sided die is ',prob
      
      end