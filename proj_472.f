c-------------------------------------------------------------------------------------------------c
      program project_euler_472
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  There are N seats in a row. N people come one after another to fill the seats according to the c
c  following rules:                                                                               c
c                                                                                                 c
c     -No person sits beside another.                                                             c
c     -The first person chooses any seat.                                                         c
c     -Each subsequent person chooses the seat furthest from anyone else already seated, as long  c
c     as it does not violate rule 1. If there is more than one choice satisfying this condition,  c
c     then the person chooses the leftmost choice.                                                c
c                                                                                                 c
c  Note that due to rule 1, some seats will surely be left unoccupied, and the maximum number of  c
c  people that can be seated is less than N (for N > 1).                                          c
c                                                                                                 c
c  Here are the possible seating arrangements for N = 15:                                         c
c                                                                                                 c
c  We see that if the first person chooses correctly, the 15 seats can seat up to 7 people.       c
c  We can also see that the first person has 9 choices to maximize the number of people that may  c
c  be seated.                                                                                     c
c                                                                                                 c
c  Let f(N) be the number of choices the first person has to maximize the number of occupants for c
c  N seats in a row. Thus, f(1) = 1, f(15) = 9, f(20) = 6, and f(500) = 16.                       c
c                                                                                                 c
c  Also, ∑f(N) = 83 for 1 ≤ N ≤ 20 and ∑f(N) = 13343 for 1 ≤ N ≤ 500.                             c
c                                                                                                 c
c  Find ∑f(N) for 1 ≤ N ≤ 10^12. Give the last 8 digits of your answer.                           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      logical used(500), available(500), done, mid_used
      integer*8 max_people(500), num_combos(500), num_people, max_dist, min_dist, choice
      integer*8 total, loop, max_x1, mod_x1, counter, power_2

c initialize ALL THE THINGS!
      total = 5
      do x1=1,500
        max_people(x1) = 0
        num_combos(x1) = 0
      enddo
      
c some of the results I'm just going to hard code because fuck you that's why
      max_people(1) = 1
      num_combos(1) = 1
      max_people(2) = 1
      num_combos(2) = 2
      max_people(3) = 2
      num_combos(3) = 2
C       do x1=1,3
C         write(*,*) x1,num_combos(x1)
C       enddo
      
c loop over the number of seats      
      do x1=4,15

c if the number of seats is even then just go to x1/2, else we need to include the middle
        if (((x1/2)*2).eq.x1) then
          
c loop over the number of choices for the first player 
          do x2=1,(x1/2)
          
c initialize the used variable
            do x3=1,x1
              used(x3) = .false.
              available(x3) = .true.
            enddo
            
            used(x2) = .true.
            available(x2) = .false.
            available(x2+1) = .false.
            if (x2.ne.1) available(x2-1) = .false.
            
            used(x1) = .true.
            available(x1) = .false.
            available(x1-1) = .false.
            
            num_people = 2
            done = .false.
            do while(.not.done)
              max_dist = 0
              done = .true.
              do x4=1,x1
                if (available(x4)) then
                  done = .false.
                  min_dist = 10000
                  do x5=1,x1
                    if (used(x5)) then
                      if ((abs(x5-x4)).lt.min_dist) min_dist = abs(x5-x4)
                    endif
                  enddo
                  
                  if (min_dist.gt.max_dist) then
                    max_dist = min_dist
                    choice = x4
                  endif
                endif
              enddo
              
              if (.not.done) then
                num_people = num_people + 1
                used(choice) = .true.
                available(choice) = .false.
                available(choice+1) = .false.
                available(choice-1) = .false.
              endif
            enddo
            
            if (num_people.eq.max_people(x1)) then
              num_combos(x1) = num_combos(x1) + 1
            else if (num_people.gt.max_people(x1)) then
              max_people(x1) = num_people
              num_combos(x1) = 1
            endif
          enddo

c adjust the number of combos due to the symmetry of the problem
          num_combos(x1) = 2*num_combos(x1)
      
        else
c loop over the number of choices for the first player 
          do x2=1,(x1/2+1)
          
c initialize the used variable
            do x3=1,x1
              used(x3) = .false.
              available(x3) = .true.
            enddo
            
            used(x2) = .true.
            available(x2) = .false.
            available(x2+1) = .false.
            if (x2.ne.1) available(x2-1) = .false.
            
            used(x1) = .true.
            available(x1) = .false.
            available(x1-1) = .false.
            
            num_people = 2
            done = .false.
            do while(.not.done)
              max_dist = 0
              done = .true.
              do x4=1,x1
                if (available(x4)) then
                  done = .false.
                  min_dist = 10000
                  do x5=1,x1
                    if (used(x5)) then
                      if ((abs(x5-x4)).lt.min_dist) min_dist = abs(x5-x4)
                    endif
                  enddo
                  
                  if (min_dist.gt.max_dist) then
                    max_dist = min_dist
                    choice = x4
                  endif
                endif
              enddo
              
              if (.not.done) then
                num_people = num_people + 1
                used(choice) = .true.
                available(choice) = .false.
                available(choice+1) = .false.
                available(choice-1) = .false.
              endif
            enddo
            
            if (num_people.eq.max_people(x1)) then
              num_combos(x1) = num_combos(x1) + 1
              if (x2.eq.(x1/2+1)) then
                mid_used = .true.
              else
                mid_used = .false.
              endif
            else if (num_people.gt.max_people(x1)) then
              max_people(x1) = num_people
              num_combos(x1) = 1
              if (x2.eq.(x1/2+1)) then
                mid_used = .true.
              else
                mid_used = .false.
              endif
            endif
          enddo

c adjust the number of combos due to the symmetry of the problem
          if (mid_used) then
            num_combos(x1) = 2*num_combos(x1)-1
          else
            num_combos(x1) = 2*num_combos(x1)
          endif
        endif
        
        write(*,*) x1,num_combos(x1)
        total = total + num_combos(x1)
      enddo

c Now that we've summed up all the totals up to 15, we're at a point where the pattern emerges and
c we can do some fast sums
      loop = 1
      x1 = 15
      max_x1 = 1
      do x2=1,12
        max_x1 = max_x1*10
      enddo
      mod_x1 = 1
      do x2=1,9
        mod_x1 = mod_x1*10
      enddo

C       do while((x1.le.max_x1).and.(loop.le.36))
C       do while((x1.le.max_x1).and.(loop.le.5))
      do while((x1.le.max_x1))

c first we have integers starting from 2^loop+2 -> 3 by 1's
        total = total + (2**loop - (2**loop/mod_x1)*mod_x1 + 2)/2
     .                * (2**loop - (2**loop/mod_x1)*mod_x1 + 3) - 3
        if (total.lt.0) then
          write(*,*) 'FAIL IN PART 1'
          read(*,*)
        endif
        x1 = x1 + 2**loop
                
        if (total.gt.mod_x1) then
          total = total - (total/mod_x1)*mod_x1
        endif
        if (x1.gt.max_x1) then
          write(*,*) 'stopped in part 1 with total = ',total
          write(*,*) 'x1 = ',x1,' loop = ',loop
          read(*,*)
        endif
        
c next we have an intermediate group of 3 numbers
        total = total + 16
        if (total.lt.0) then
          write(*,*) 'FAIL IN PART 2'
          read(*,*)
        endif
        x1 = x1 + 3
                
        if (total.gt.mod_x1) then
          total = total - (total/mod_x1)*mod_x1
        endif
        if (x1.gt.max_x1) then
          write(*,*) 'stopped in part 2 with total = ',total
          write(*,*) 'x1 = ',x1,' loop = ',loop
          read(*,*)
        endif
        
c next we have the humps
        do x2=1,loop
          total = total + (2**(x2+1) - (2**(x2+1)/mod_x1)*mod_x1)
     .                  * (2**x2 - (2**x2/mod_x1)*mod_x1 +3)
          if (total.lt.0) then
            write(*,*) 'FAIL IN PART 3'
            read(*,*)
          endif

          x1 = x1 + 2**(x2+1)
          
          if (total.gt.mod_x1) then
            total = total - (total/mod_x1)*mod_x1
          endif
          if (x1.gt.max_x1) then
            write(*,*) 'stopped in part 3 with total = ',total
            write(*,*) 'x1 = ',x1,' loop = ',loop
            write(*,*) 'hump = ',x2
            read(*,*)
          endif          
        enddo

c Finally we have the last hill which doesn't go back down
        total = total + (2**(loop+1) - (2**(loop+1)/mod_x1)*mod_x1 + 1)
     .                * (2**(loop+1) - (2**(loop+1)/mod_x1)*mod_x1 + 3)
        if (total.lt.0) then
          write(*,*) 'FAIL IN PART 4'
          read(*,*)
        endif
        
        x1 = x1 + 2**(loop+1) + 1
        
        if (total.gt.mod_x1) then
          total = total - (total/mod_x1)*mod_x1
        endif
        if (x1.gt.max_x1) then
          write(*,*) 'stopped in part 4 with total = ',total
          write(*,*) 'x1 = ',x1,' loop = ',loop
          read(*,*)
        endif

c increment the loop
        loop = loop + 1
        write(*,*) x1, total
c        read(*,*)
      enddo
      
      power_2 = 1
      do x3=1,37
C       do x3=1,6
        power_2 = power_2*2
      enddo
      counter = 0

      do while(x1.lt.max_x1)
        x1 = x1 + 1
        total = total + power_2+2 - counter
        if (total.lt.0) then
          write(*,*) 'FAILED AT THE END'
          read(*,*)
        endif
        if (total.gt.mod_x1) then
          total = total - (total/mod_x1)*mod_x1
        endif
        counter = counter + 1
      enddo
              
      write(*,*) 'counter = ',counter
      write(*,*) 'total = ',total
      end