c-------------------------------------------------------------------------------------------------c
      program project_euler_68
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line       c
c  adding to nine.                                                                                c
c                                                                                                 c
c  Working clockwise, and starting from the group of three with the numerically lowest external   c
c  node (4,3,2 in this example), each solution can be described uniquely. For example, the above  c
c  solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.                                     c
c                                                                                                 c
c  It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are   c
c  eight solutions in total.                                                                      c
c                                                                                                 c
c  Total	Solution Set                                                                            c
c  9	4,2,3; 5,3,1; 6,1,2                                                                         c
c  9	4,3,2; 6,2,1; 5,1,3                                                                         c
c  10	2,3,5; 4,5,1; 6,1,3                                                                         c
c  10	2,5,3; 6,3,1; 4,1,5                                                                         c
c  11	1,4,6; 3,6,2; 5,2,4                                                                         c
c  11	1,6,4; 5,4,2; 3,2,6                                                                         c
c  12	1,5,6; 2,6,4; 3,4,5                                                                         c
c  12	1,6,5; 3,5,4; 2,4,6                                                                         c
c                                                                                                 c
c  By concatenating each group it is possible to form 9-digit strings; the maximum string for a   c
c  3-gon ring is 432621513.                                                                       c
c                                                                                                 c
c  Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and       c
c  17-digit strings. What is the maximum 16-digit string for a "magic" 5-gon ring?                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c  Parameters used in this program only
      integer*8 total, max_number, result(16), compare(16), index
      logical found_max
      
c  Initialize the result
      do x1=1,16
        result(x1) = 0
      enddo  

c  Get a permutation of all 10 numbers
c  Since we are looking for a 16 number magic 5-gon ring, 10 can't be in the middle
c  i.e. x2, x3, x5, x7, and x9 are not equal to 10
      do x1=1,10
        do x2=1,9
      if (x1.ne.x2) then
          do x3=1,9
      if ((x1.ne.x3).and.(x2.ne.x3)) then
        
c  The first three numbers sets the total that defines the magic 5-gon ring
            total = x1+x2+x3
            do x4=x1+1,10
      if ((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=1,9

c  The next two numbers gets the next set of 3 (x3,x4,x5)
      if ((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5).and.((x3+x4+x5).eq.total)) then
                do x6=x1+1,10
      if ((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                  do x7=1,9
                    
c  same for x5,x6,x7
      if ((x1.ne.x7).and.(x2.ne.x7).and.(x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)
     .    .and.((x5+x6+x7).eq.total)) then
                    do x8=x1+1,10
      if ((x1.ne.x8).and.(x2.ne.x8).and.(x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8)
     .    .and.(x7.ne.x8)) then
                      do x9=1,9

c  same for x7,x8,x9
      if ((x1.ne.x9).and.(x2.ne.x9).and.(x3.ne.x9).and.(x4.ne.x9).and.(x5.ne.x9).and.(x6.ne.x9)
     .    .and.(x7.ne.x9).and.(x8.ne.x9).and.((x7+x8+x9).eq.total)) then
                        do x10=x1+1,10

c  finally for x10,x9,x2 
      if ((x1.ne.x10).and.(x2.ne.x10).and.(x3.ne.x10).and.(x4.ne.x10).and.(x5.ne.x10).and.(x6.ne.x10)
     .    .and.(x7.ne.x10).and.(x8.ne.x10).and.(x9.ne.x10).and.((x9+x10+x2).eq.total)) then

        write(*,*) 'Total ',total
        write(*,fmt='(3(I2,1X),1X,3(I2,1X),1X,3(I2,1X),1X,3(I2,1X),1X,3(I2,1X))') x1,x2,x3, x4,x3,x5, x6,x5,x7, 
     .      x8,x7,x9, x10,x9,x2 
                        
c check to see if this number is the biggest combination
        index = 16
c x1,x2,x3 
        if (x1.eq.10) then
          compare(index) = 1
          compare(index-1) = 0
          index = index - 2
        else
          compare(index) = x1
          index = index - 1
        endif
        compare(index) = x2
        index = index - 1
        compare(index) = x3
        index = index - 1

c x4,x3,x5 
        if (x4.eq.10) then
          compare(index) = 1
          compare(index-1) = 0
          index = index - 2
        else
          compare(index) = x4
          index = index - 1
        endif
        compare(index) = x3
        index = index - 1
        compare(index) = x5
        index = index - 1  

c x6,x5,x7 
        if (x6.eq.10) then
          compare(index) = 1
          compare(index-1) = 0
          index = index - 2
        else
          compare(index) = x6
          index = index - 1
        endif
        compare(index) = x5
        index = index - 1
        compare(index) = x7
        index = index - 1

c x8,x7,x9 
        if (x8.eq.10) then
          compare(index) = 1
          compare(index-1) = 0
          index = index - 2
        else
          compare(index) = x8
          index = index - 1
        endif
        compare(index) = x7
        index = index - 1
        compare(index) = x9
        index = index - 1
                                
c x10,x9,x2
        if (x10.eq.10) then
          compare(index) = 1
          compare(index-1) = 0
          index = index - 2
        else
          compare(index) = x10
          index = index - 1
        endif
        compare(index) = x9
        index = index - 1
        compare(index) = x2
        index = index - 1

        index = 16
        found_max = .false.
        do while(.not.found_max)
          if (compare(index).ne.result(index)) then
            found_max = .true.
          else
            index = index - 1
          endif
        enddo
          
        if (compare(index).gt.result(index)) then
          do x11=1,16
            result(x11) = compare(x11)
          enddo
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
      
      write(*,*) 'Result:'
      write(*,fmt='(16(I1))') (result(x1), x1=16,1,-1)
      
      end