c-------------------------------------------------------------------------------------------------c
      program project_euler_93
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the    c
c  four arithmetic operations (+, −, *, /) and brackets/parentheses, it is possible to form       c
c  different positive integer targets.                                                            c
c                                                                                                 c
c  For example,                                                                                   c
c                                                                                                 c
c  8 = (4 * (1 + 3)) / 2                                                                          c
c  14 = 4 * (3 + 1 / 2)                                                                           c
c  19 = 4 * (2 + 3) − 1                                                                           c
c  36 = 3 * 4 * (2 + 1)                                                                           c
c                                                                                                 c
c  Note that concatenations of the digits, like 12 + 34, are not allowed.                         c
c                                                                                                 c
c  Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of   c
c  which 36 is the maximum, and each of the numbers 1 to 28 can be obtained before encountering   c
c  the first non-expressible number.                                                              c
c                                                                                                 c
c  Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive  c
c  positive integers, 1 to n, can be obtained, giving your answer as a string: abcd.              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      double precision targets(16896), index, num(4)
      integer*8 max_target, results(4), target, this_series
      logical series, found_target

c initialize the results
      max_target = 0
      do x1=1,4
        results(x1) = 0
      enddo
      
c choose the digits
      do x1=1,6
        do x2=x1+1,7
          do x3=x2+1,8
            do x4=x3+1,9

c initialize the possible targets 
              index = 1
              do x5=1,16896
                targets(x5) = 0
              enddo
              num(1) = dble(x1)
              num(2) = dble(x2)
              num(3) = dble(x3)
              num(4) = dble(x4)
              
c loop over the possible combinations of numbers
              do x5=1,4
                do x6=1,4
                  if (x5.ne.x6) then
                  do x7=1,4
                    if ((x5.ne.x7).and.(x6.ne.x7)) then
                    do x8=1,4
                      if ((x5.ne.x8).and.(x6.ne.x8).and.(x7.ne.x8)) then

c start with +
c +,+,+                            
                        targets(index)    =  num(x5)  +  num(x6)  +  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) +  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  +  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  + (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  +  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  +  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) + (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) +  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  +  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  +  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  +  (num(x7) +  num(x8)))
                        index = index+11

c +,+,-                              
                        targets(index)    =  num(x5)  +  num(x6)  +  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) +  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  +  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  + (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  +  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  +  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) + (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) +  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  +  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  +  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  +  (num(x7) -  num(x8)))
                        index = index+11

c +,+,*
                        targets(index)    =  num(x5)  +  num(x6)  +  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) +  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  +  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  + (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  +  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  +  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) + (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) +  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  +  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  +  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  +  (num(x7) *  num(x8)))
                        index = index+11

c +,+,/
                        targets(index)    =  num(x5)  +  num(x6)  +  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) +  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  +  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  + (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  +  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  +  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) + (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) +  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  +  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  +  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  +  (num(x7) /  num(x8)))
                        index = index+11

c +,-,+
                        targets(index)    =  num(x5)  +  num(x6)  -  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) -  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  -  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  - (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  -  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  -  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) - (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) -  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  -  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  -  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  -  (num(x7) +  num(x8)))
                        index = index+11

c +,-,-
                        targets(index)    =  num(x5)  +  num(x6)  -  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) -  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  -  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  - (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  -  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  -  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) - (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) -  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  -  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  -  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  -  (num(x7) -  num(x8)))
                        index = index+11

c +,-,*
                        targets(index)    =  num(x5)  +  num(x6)  -  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) -  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  -  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  - (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  -  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  -  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) - (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) -  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  -  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  -  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  -  (num(x7) *  num(x8)))
                        index = index+11

c +,-,/
                        targets(index)    =  num(x5)  +  num(x6)  -  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) -  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  -  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  - (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  -  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  -  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) - (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) -  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  -  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  -  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  -  (num(x7) /  num(x8)))
                        index = index+11

c +,*,+
                        targets(index)    =  num(x5)  +  num(x6)  *  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) *  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  *  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  * (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  *  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  *  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) * (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) *  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  *  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  *  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  *  (num(x7) +  num(x8)))
                        index = index+11
                        
c +,*,-
                        targets(index)    =  num(x5)  +  num(x6)  *  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) *  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  *  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  * (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  *  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  *  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) * (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) *  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  *  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  *  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  *  (num(x7) -  num(x8)))
                        index = index+11
c +,*,*
                        targets(index)    =  num(x5)  +  num(x6)  *  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) *  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  *  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  * (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  *  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  *  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) * (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) *  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  *  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  *  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  *  (num(x7) *  num(x8)))
                        index = index+11
                        
c +,*,/
                        targets(index)    =  num(x5)  +  num(x6)  *  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) *  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  *  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  * (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  *  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  *  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) * (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) *  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  *  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  *  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  *  (num(x7) /  num(x8)))
                        index = index+11
                        
c +,/,+
                        targets(index)    =  num(x5)  +  num(x6)  /  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) /  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  /  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  / (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  /  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  /  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) / (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) /  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  /  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  /  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  /  (num(x7) +  num(x8)))
                        index = index+11
                        
c +,/,-
                        targets(index)    =  num(x5)  +  num(x6)  /  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) /  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  /  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  / (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  /  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  /  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) / (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) /  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  /  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  /  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  /  (num(x7) -  num(x8)))
                        index = index+11
                        
c +,/,*
                        targets(index)    =  num(x5)  +  num(x6)  /  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) /  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  /  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  / (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  /  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  /  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) / (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) /  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  /  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  /  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  /  (num(x7) *  num(x8)))
                        index = index+11
                        
c +,/,/
                        targets(index)    =  num(x5)  +  num(x6)  /  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  +  num(x6)) /  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  + (num(x6)  /  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  +  num(x6)  / (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  +  num(x6)  /  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  + (num(x6)  /  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  +  num(x6)) / (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  +  num(x6)) /  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  + (num(x6)  /  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  +((num(x6)  /  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  + (num(x6)  /  (num(x7) /  num(x8)))
                        index = index+11

c start with -
c -,+,+                            
                        targets(index)    =  num(x5)  -  num(x6)  +  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) +  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  +  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  + (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  +  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  +  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) + (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) +  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  +  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  +  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  +  (num(x7) +  num(x8)))
                        index = index+11

c -,+,-                              
                        targets(index)    =  num(x5)  -  num(x6)  +  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) +  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  +  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  + (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  +  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  +  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) + (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) +  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  +  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  +  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  +  (num(x7) -  num(x8)))
                        index = index+11

c -,+,*
                        targets(index)    =  num(x5)  -  num(x6)  +  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) +  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  +  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  + (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  +  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  +  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) + (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) +  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  +  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  +  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  +  (num(x7) *  num(x8)))
                        index = index+11

c -,+,/
                        targets(index)    =  num(x5)  -  num(x6)  +  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) +  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  +  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  + (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  +  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  +  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) + (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) +  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  +  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  +  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  +  (num(x7) /  num(x8)))
                        index = index+11

c -,-,+
                        targets(index)    =  num(x5)  -  num(x6)  -  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) -  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  -  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  - (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  -  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  -  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) - (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) -  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  -  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  -  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  -  (num(x7) +  num(x8)))
                        index = index+11

c -,-,-
                        targets(index)    =  num(x5)  -  num(x6)  -  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) -  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  -  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  - (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  -  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  -  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) - (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) -  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  -  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  -  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  -  (num(x7) -  num(x8)))
                        index = index+11

c -,-,*
                        targets(index)    =  num(x5)  -  num(x6)  -  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) -  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  -  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  - (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  -  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  -  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) - (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) -  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  -  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  -  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  -  (num(x7) *  num(x8)))
                        index = index+11

c -,-,/
                        targets(index)    =  num(x5)  -  num(x6)  -  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) -  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  -  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  - (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  -  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  -  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) - (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) -  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  -  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  -  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  -  (num(x7) /  num(x8)))
                        index = index+11

c -,*,+
                        targets(index)    =  num(x5)  -  num(x6)  *  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) *  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  *  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  * (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  *  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  *  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) * (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) *  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  *  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  *  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  *  (num(x7) +  num(x8)))
                        index = index+11
c -,*,-
                        targets(index)    =  num(x5)  -  num(x6)  *  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) *  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  *  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  * (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  *  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  *  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) * (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) *  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  *  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  *  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  *  (num(x7) -  num(x8)))
                        index = index+11
c -,*,*
                        targets(index)    =  num(x5)  -  num(x6)  *  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) *  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  *  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  * (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  *  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  *  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) * (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) *  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  *  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  *  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  *  (num(x7) *  num(x8)))
                        index = index+11
c -,*,/
                        targets(index)    =  num(x5)  -  num(x6)  *  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) *  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  *  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  * (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  *  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  *  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) * (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) *  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  *  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  *  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  *  (num(x7) /  num(x8)))
                        index = index+11
c -,/,+
                        targets(index)    =  num(x5)  -  num(x6)  /  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) /  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  /  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  / (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  /  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  /  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) / (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) /  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  /  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  /  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  /  (num(x7) +  num(x8)))
                        index = index+11
c -,/,-
                        targets(index)    =  num(x5)  -  num(x6)  /  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) /  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  /  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  / (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  /  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  /  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) / (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) /  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  /  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  /  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  /  (num(x7) -  num(x8)))
                        index = index+11
c -,/,*
                        targets(index)    =  num(x5)  -  num(x6)  /  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) /  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  /  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  / (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  /  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  /  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) / (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) /  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  /  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  /  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  /  (num(x7) *  num(x8)))
                        index = index+11
c -,/,/
                        targets(index)    =  num(x5)  -  num(x6)  /  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  -  num(x6)) /  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  - (num(x6)  /  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  -  num(x6)  / (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  -  num(x6)  /  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  - (num(x6)  /  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  -  num(x6)) / (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  -  num(x6)) /  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  - (num(x6)  /  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  -((num(x6)  /  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  - (num(x6)  /  (num(x7) /  num(x8)))
                        index = index+11

c start with *
c *,+,+                            
                        targets(index)    =  num(x5)  *  num(x6)  +  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) +  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  +  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  + (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  +  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  +  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) + (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) +  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  +  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  +  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  +  (num(x7) +  num(x8)))
                        index = index+11

c *,+,-                              
                        targets(index)    =  num(x5)  *  num(x6)  +  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) +  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  +  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  + (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  +  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  +  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) + (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) +  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  +  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  +  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  +  (num(x7) -  num(x8)))
                        index = index+11

c *,+,*
                        targets(index)    =  num(x5)  *  num(x6)  +  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) +  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  +  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  + (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  +  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  +  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) + (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) +  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  +  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  +  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  +  (num(x7) *  num(x8)))
                        index = index+11

c *,+,/
                        targets(index)    =  num(x5)  *  num(x6)  +  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) +  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  +  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  + (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  +  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  +  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) + (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) +  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  +  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  +  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  +  (num(x7) /  num(x8)))
                        index = index+11

c *,-,+
                        targets(index)    =  num(x5)  *  num(x6)  -  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) -  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  -  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  - (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  -  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  -  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) - (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) -  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  -  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  -  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  -  (num(x7) +  num(x8)))
                        index = index+11

c *,-,-
                        targets(index)    =  num(x5)  *  num(x6)  -  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) -  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  -  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  - (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  -  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  -  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) - (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) -  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  -  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  -  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  -  (num(x7) -  num(x8)))
                        index = index+11

c *,-,*
                        targets(index)    =  num(x5)  *  num(x6)  -  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) -  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  -  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  - (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  -  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  -  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) - (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) -  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  -  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  -  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  -  (num(x7) *  num(x8)))
                        index = index+11

c *,-,/
                        targets(index)    =  num(x5)  *  num(x6)  -  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) -  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  -  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  - (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  -  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  -  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) - (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) -  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  -  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  -  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  -  (num(x7) /  num(x8)))
                        index = index+11

c *,*,+
                        targets(index)    =  num(x5)  *  num(x6)  *  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) *  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  *  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  * (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  *  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  *  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) * (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) *  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  *  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  *  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  *  (num(x7) +  num(x8)))
                        index = index+11
c *,*,-
                        targets(index)    =  num(x5)  *  num(x6)  *  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) *  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  *  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  * (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  *  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  *  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) * (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) *  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  *  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  *  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  *  (num(x7) -  num(x8)))
                        index = index+11
c *,*,*
                        targets(index)    =  num(x5)  *  num(x6)  *  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) *  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  *  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  * (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  *  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  *  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) * (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) *  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  *  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  *  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  *  (num(x7) *  num(x8)))
                        index = index+11
c *,*,/
                        targets(index)    =  num(x5)  *  num(x6)  *  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) *  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  *  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  * (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  *  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  *  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) * (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) *  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  *  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  *  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  *  (num(x7) /  num(x8)))
                        index = index+11
c *,/,+
                        targets(index)    =  num(x5)  *  num(x6)  /  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) /  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  /  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  / (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  /  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  /  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) / (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) /  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  /  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  /  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  /  (num(x7) +  num(x8)))
                        index = index+11
c *,/,-
                        targets(index)    =  num(x5)  *  num(x6)  /  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) /  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  /  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  / (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  /  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  /  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) / (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) /  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  /  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  /  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  /  (num(x7) -  num(x8)))
                        index = index+11
c *,/,*
                        targets(index)    =  num(x5)  *  num(x6)  /  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) /  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  /  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  / (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  /  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  /  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) / (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) /  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  /  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  /  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  /  (num(x7) *  num(x8)))
                        index = index+11
c *,/,/
                        targets(index)    =  num(x5)  *  num(x6)  /  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  *  num(x6)) /  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  * (num(x6)  /  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  *  num(x6)  / (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  *  num(x6)  /  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  * (num(x6)  /  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  *  num(x6)) / (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  *  num(x6)) /  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  * (num(x6)  /  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  *((num(x6)  /  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  * (num(x6)  /  (num(x7) /  num(x8)))
                        index = index+11

c start with /
c /,+,+                            
                        targets(index)    =  num(x5)  /  num(x6)  +  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) +  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  +  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  + (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  +  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  +  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) + (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) +  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  +  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  +  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  +  (num(x7) +  num(x8)))
                        index = index+11

c /,+,-                              
                        targets(index)    =  num(x5)  /  num(x6)  +  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) +  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  +  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  + (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  +  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  +  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) + (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) +  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  +  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  +  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  +  (num(x7) -  num(x8)))
                        index = index+11

c /,+,*
                        targets(index)    =  num(x5)  /  num(x6)  +  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) +  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  +  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  + (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  +  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  +  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) + (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) +  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  +  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  +  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  +  (num(x7) *  num(x8)))
                        index = index+11

c /,+,/
                        targets(index)    =  num(x5)  /  num(x6)  +  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) +  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  +  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  + (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  +  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  +  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) + (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) +  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  +  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  +  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  +  (num(x7) /  num(x8)))
                        index = index+11

c /,-,+
                        targets(index)    =  num(x5)  /  num(x6)  -  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) -  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  -  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  - (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  -  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  -  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) - (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) -  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  -  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  -  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  -  (num(x7) +  num(x8)))
                        index = index+11

c /,-,-
                        targets(index)    =  num(x5)  /  num(x6)  -  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) -  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  -  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  - (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  -  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  -  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) - (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) -  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  -  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  -  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  -  (num(x7) -  num(x8)))
                        index = index+11

c /,-,*
                        targets(index)    =  num(x5)  /  num(x6)  -  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) -  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  -  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  - (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  -  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  -  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) - (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) -  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  -  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  -  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  -  (num(x7) *  num(x8)))
                        index = index+11

c /,-,/
                        targets(index)    =  num(x5)  /  num(x6)  -  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) -  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  -  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  - (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  -  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  -  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) - (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) -  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  -  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  -  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  -  (num(x7) /  num(x8)))
                        index = index+11

c /,*,+
                        targets(index)    =  num(x5)  /  num(x6)  *  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) *  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  *  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  * (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  *  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  *  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) * (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) *  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  *  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  *  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  *  (num(x7) +  num(x8)))
                        index = index+11
c /,*,-
                        targets(index)    =  num(x5)  /  num(x6)  *  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) *  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  *  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  * (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  *  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  *  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) * (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) *  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  *  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  *  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  *  (num(x7) -  num(x8)))
                        index = index+11
c /,*,*
                        targets(index)    =  num(x5)  /  num(x6)  *  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) *  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  *  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  * (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  *  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  *  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) * (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) *  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  *  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  *  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  *  (num(x7) *  num(x8)))
                        index = index+11
c /,*,/
                        targets(index)    =  num(x5)  /  num(x6)  *  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) *  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  *  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  * (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  *  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  *  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) * (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) *  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  *  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  *  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  *  (num(x7) /  num(x8)))
                        index = index+11
c /,/,+
                        targets(index)    =  num(x5)  /  num(x6)  /  num(x7)  +  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) /  num(x7)  +  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  /  num(x7)) +  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  / (num(x7)  +  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  /  num(x7)) +  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  /  num(x7)  +  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) / (num(x7)  +  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) /  num(x7)) +  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  /  num(x7)))+  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  /  num(x7)) +  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  /  (num(x7) +  num(x8)))
                        index = index+11
c /,/,-
                        targets(index)    =  num(x5)  /  num(x6)  /  num(x7)  -  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) /  num(x7)  -  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  /  num(x7)) -  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  / (num(x7)  -  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  /  num(x7)) -  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  /  num(x7)  -  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) / (num(x7)  -  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) /  num(x7)) -  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  /  num(x7)))-  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  /  num(x7)) -  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  /  (num(x7) -  num(x8)))
                        index = index+11
c /,/,*
                        targets(index)    =  num(x5)  /  num(x6)  /  num(x7)  *  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) /  num(x7)  *  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  /  num(x7)) *  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  / (num(x7)  *  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  /  num(x7)) *  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  /  num(x7)  *  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) / (num(x7)  *  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) /  num(x7)) *  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  /  num(x7)))*  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  /  num(x7)) *  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  /  (num(x7) *  num(x8)))
                        index = index+11
c /,/,/
                        targets(index)    =  num(x5)  /  num(x6)  /  num(x7)  /  num(x8)
                        targets(index+1)  = (num(x5)  /  num(x6)) /  num(x7)  /  num(x8)
                        targets(index+2)  =  num(x5)  / (num(x6)  /  num(x7)) /  num(x8)
                        targets(index+3)  =  num(x5)  /  num(x6)  / (num(x7)  /  num(x8))
                        targets(index+4)  = (num(x5)  /  num(x6)  /  num(x7)) /  num(x8)
                        targets(index+5)  =  num(x5)  / (num(x6)  /  num(x7)  /  num(x8))
                        targets(index+6)  = (num(x5)  /  num(x6)) / (num(x7)  /  num(x8))
                        targets(index+7)  =((num(x5)  /  num(x6)) /  num(x7)) /  num(x8)
                        targets(index+8)  = (num(x5)  / (num(x6)  /  num(x7)))/  num(x8)
                        targets(index+9)  =  num(x5)  /((num(x6)  /  num(x7)) /  num(x8))
                        targets(index+10) =  num(x5)  / (num(x6)  /  (num(x7) /  num(x8)))
                        index = index+11

                      endif
                    enddo
                    endif
                  enddo
                  endif
                enddo
              enddo

c Holy shit that was a lot of code.  Now that we have all the targets we start looping to find the integers
              series = .true.
              target = 0
              do while(series)
                target = target + 1
                found_target = .false.
                do x5=1,16896
                  if (int(targets(x5)).eq.targets(x5)) then
                    if (targets(x5).eq.target) found_target = .true.
                  endif
                enddo
                
                if (.not.found_target) then
                  series = .false.
                  this_series = target-1
                endif
              enddo
              
              if (this_series.gt.max_target) then
                max_target = this_series
                results(1) = x1
                results(2) = x2
                results(3) = x3
                results(4) = x4
              endif
            enddo
          enddo
        enddo
      enddo
      
      write(*,*) 'The four numbers that give the largest series is:'
      write(*,fmt='(4(I1))') (results(x1), x1=1,4)
      write(*,*) 'The series goes from 1 to ',max_target
      
      end