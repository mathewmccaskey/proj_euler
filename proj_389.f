c-------------------------------------------------------------------------------------------------c
      program project_euler_389
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  An unbiased single 4-sided die is thrown and its value, T, is noted.                           c
c  T unbiased 6-sided dice are thrown and their scores are added together. The sum, C, is noted.  c
c  C unbiased 8-sided dice are thrown and their scores are added together. The sum, O, is noted.  c
c  O unbiased 12-sided dice are thrown and their scores are added together. The sum, D, is noted. c
c  D unbiased 20-sided dice are thrown and their scores are added together. The sum, I, is noted. c
c  Find the variance of I, and give your answer rounded to 4 decimal places.                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 max_d4_dice, max_d6_dice, max_d8_dice, max_d12_dice, max_d20_dice
      double precision ave, var, test
      double precision prob_d4(1,4), prob_d6(4,24), prob_d8(24,192), prob_d12(192,2304)
      double precision prob_roll_d4(1), prob_roll_d6(4), prob_roll_d8(24), prob_roll_d12(192), prob_roll_d20(2304)

c initialize some parameters
      max_d4_dice = 1
      max_d6_dice = max_d4_dice*4
      max_d8_dice = max_d6_dice*6
      max_d12_dice = max_d8_dice*8
      max_d20_dice = max_d12_dice*12

      do x1=1,max_d4_dice
        prob_roll_d4(x1) = 0.d0
        do x2=1,max_d6_dice
          prob_d4(x1,x2) = 0.d0
        enddo
      enddo

      do x1=1,max_d6_dice
        prob_roll_d6(x1) = 0.d0
        do x2=1,max_d8_dice
          prob_d6(x1,x2) = 0.d0
        enddo
      enddo

      do x1=1,max_d8_dice
        prob_roll_d8(x1) = 0.d0
        do x2=1,max_d12_dice
          prob_d8(x1,x2) = 0.d0
        enddo
      enddo

      do x1=1,max_d12_dice
        prob_roll_d12(x1) = 0.d0
        do x2=1,max_d20_dice
          prob_d12(x1,x2) = 0.d0
        enddo
      enddo
      
      do x1=1,max_d20_dice
        prob_roll_d20(x1) = 0.d0
      enddo
      
c generate the probabilities      
c D4
c You start off with 1 d4 so the probability to roll 1 d4 is 1
      prob_roll_d4(1) = 1.d0

c base probability for 1 d4
      do x1=1,4
        prob_d4(1,x1) = 1.d0/4.d0
      enddo
      
c d6
c calculate the probabilities for rolling a certain number of d6's
      do x1=1,max_d6_dice
        do x2=1,max_d4_dice
          prob_roll_d6(x1) = prob_roll_d6(x1) + prob_roll_d4(x2)*prob_d4(x2,x1)
        enddo
      enddo

c base probability for 1 d6      
      do x1=1,6
        prob_d6(1,x1) = 1.d0/6.d0
      enddo
      
      do x1=2,max_d6_dice       
        do x2=1,6
          do x3=1,max_d8_dice-x2
            prob_d6(x1,x2+x3) = prob_d6(x1,x2+x3) + prob_d6(1,x2)*prob_d6(x1-1,x3)
          enddo
        enddo
      enddo

c d8
c calculate the probabilities for rolling a certain number of d8's      
      do x1=1,max_d8_dice
        do x2=1,max_d6_dice
          prob_roll_d8(x1) = prob_roll_d8(x1) + prob_roll_d6(x2)*prob_d6(x2,x1)
        enddo
      enddo

c base probability for 1 d8
      do x1=1,8
        prob_d8(1,x1) = 1.d0/8.d0
      enddo
      
      do x1=2,max_d8_dice
        do x2=1,8
          do x3=1,max_d12_dice-x2
            prob_d8(x1,x2+x3) = prob_d8(x1,x2+x3) + prob_d8(1,x2)*prob_d8(x1-1,x3)
          enddo
        enddo
      enddo

c d12
c calculate the probabilities for rolling a certain number of d12's      
      do x1=1,max_d12_dice
        do x2=1,max_d8_dice
          prob_roll_d12(x1) = prob_roll_d12(x1) + prob_roll_d8(x2)*prob_d8(x2,x1)
        enddo
      enddo

c base probability for 1 d12
      do x1=1,12
        prob_d12(1,x1) = 1.d0/12.d0
      enddo
      
      do x1=2,max_d12_dice
        do x2=1,12
          do x3=1,max_d20_dice-x2
            prob_d12(x1,x2+x3) = prob_d12(x1,x2+x3) + prob_d12(1,x2)*prob_d12(x1-1,x3)
          enddo
        enddo
      enddo

c d20
c calculate the probabilities for rolling a certain number of d20's      
      do x1=1,max_d20_dice
        do x2=1,max_d12_dice
          prob_roll_d20(x1) = prob_roll_d20(x1) + prob_roll_d12(x2)*prob_d12(x2,x1)
        enddo
      enddo

c test to make sure that the probabilities add to 1
      test = 0.d0
      do x1=1,max_d6_dice
        test = test + prob_roll_d6(x1)
      enddo
      write(*,*) 'Probability to roll the d6 dice sums to ',test

      test = 0.d0
      do x1=1,max_d8_dice
        test = test + prob_roll_d8(x1)
      enddo
      write(*,*) 'Probability to roll the d8 dice sums to ',test
      
      test = 0.d0
      do x1=1,max_d12_dice
        test = test + prob_roll_d12(x1)
      enddo
      write(*,*) 'Probability to roll the d12 dice sums to ',test

      test = 0.d0
      do x1=1,max_d20_dice
        test = test + prob_roll_d20(x1)
      enddo
      write(*,*) 'Probability to roll the d20 dice sums to ',test
      
c time to calculate the average and variance of all these d20's
      ave = 0.d0
      var = 0.d0
      do x1=1,max_d20_dice
        ave = ave + prob_roll_d20(x1)*x1*(20.d0+1.d0)/2.d0
        var = var + prob_roll_d20(x1)*(x1*(20.d0**2-1.d0)/12.d0 + (x1*(20.d0+1)/2.d0)**2)
      enddo

C       ave = 0.d0
C       var = 0.d0
C       do x1=1,max_d4_dice
C         ave = ave + prob_roll_d4(x1)*x1*(4.d0+1.d0)/2.d0
C         var = var + prob_roll_d4(x1)*(x1*(4.d0**2-1.d0)/12.d0 + (x1*(4.d0+1)/2.d0)**2)
C       enddo      
      write(*,*) var, ave
      var = var - ave**2
      
      write(*,*) 'The variance of this whole mess is ',var
        
      end