c-------------------------------------------------------------------------------------------------c
      program project_euler_90
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Each of the six faces on a cube has a different digit (0 to 9) written on it; the same is      c
c  done to a second cube. By placing the two cubes side-by-side in different positions we can     c
c  form a variety of 2-digit numbers.                                                             c
c                                                                                                 c
c  For example, the square number 64 could be formed:                                             c
c                                                                                                 c
c  In fact, by carefully choosing the digits on both cubes it is possible to display all of the   c
c  square numbers below one-hundred: 01, 04, 09, 16, 25, 36, 49, 64, and 81.                      c
c                                                                                                 c
c  For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on one cube and     c
c  {1, 2, 3, 4, 8, 9} on the other cube.                                                          c
c                                                                                                 c
c  However, for this problem we shall allow the 6 or 9 to be turned upside-down so that an        c
c  arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows for all nine square numbers  c
c  to be displayed; otherwise it would be impossible to obtain 09.                                c
c                                                                                                 c
c  In determining a distinct arrangement we are interested in the digits on each cube, not the    c
c  order.                                                                                         c
c                                                                                                 c
c  {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}                                         c
c  {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}                                         c
c                                                                                                 c
c  But because we are allowing 6 and 9 to be reversed, the two distinct sets in the last example  c
c  both represent the extended set {1, 2, 3, 4, 5, 6, 9} for the purpose of forming 2-digit       c
c  numbers.                                                                                       c
c                                                                                                 c
c  How many distinct arrangements of the two cubes allow for all of the square numbers to be      c
c  displayed?                                                                                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c Parameters used in this program only
      logical die1(0:9), die2(0:9), good_pair
      integer*8 result
      
c initialize the result
      result = 0
      
c loop over all the possible combinations of dice
c die 1
      do x1=0,4
        do x2=x1+1,5
          do x3=x2+1,6
            do x4=x3+1,7
              do x5=x4+1,8
                do x6=x5+1,9
c die 2
                  do x11=0,4
                    do x12=x11+1,5
                      do x13=x12+1,6
                        do x14=x13+1,7
                          do x15=x14+1,8
                            do x16=x15+1,9
                              
c Set the boolean array for each die
                              do x7=0,9
                                die1(x7) = .false.
                                die2(x7) = .false.
                              enddo
                              die1(x1) = .true.
                              die1(x2) = .true.
                              die1(x3) = .true.
                              die1(x4) = .true.
                              die1(x5) = .true.
                              die1(x6) = .true.
                              if (die1(9)) die1(6) = .true.
                              if (die1(6)) die1(9) = .true.
                              die2(x11) = .true.
                              die2(x12) = .true.
                              die2(x13) = .true.
                              die2(x14) = .true.
                              die2(x15) = .true.
                              die2(x16) = .true.
                              if (die2(9)) die2(6) = .true.
                              if (die2(6)) die2(9) = .true.
                              
c check to see if the squares can be completed
                              good_pair = .true.
                              if ((.not.(die1(0).and.die2(1))).and.(.not.(die1(1).and.die2(0)))) good_pair = .false.
                              if ((.not.(die1(0).and.die2(4))).and.(.not.(die1(4).and.die2(0)))) good_pair = .false.
                              if ((.not.(die1(0).and.die2(9))).and.(.not.(die1(9).and.die2(0)))) good_pair = .false.
                              if ((.not.(die1(1).and.die2(6))).and.(.not.(die1(6).and.die2(1)))) good_pair = .false.
                              if ((.not.(die1(2).and.die2(5))).and.(.not.(die1(5).and.die2(2)))) good_pair = .false.
                              if ((.not.(die1(3).and.die2(6))).and.(.not.(die1(6).and.die2(3)))) good_pair = .false.
                              if ((.not.(die1(4).and.die2(9))).and.(.not.(die1(9).and.die2(4)))) good_pair = .false.
                              if ((.not.(die1(6).and.die2(4))).and.(.not.(die1(4).and.die2(6)))) good_pair = .false.
                              if ((.not.(die1(8).and.die2(1))).and.(.not.(die1(1).and.die2(8)))) good_pair = .false.
                              
                              if (good_pair) result = result + 1
                              
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
      
      write(*,*) 'The number of dice combos is ',result/2
      
      end    