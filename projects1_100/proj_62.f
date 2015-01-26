c-------------------------------------------------------------------------------------------------c
      program project_euler_51
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and   c
c  66430125 (405^3). In fact, 41063625 is the smallest cube which has exactly three permutations  c
c  of its digits which are also cube.                                                             c
c                                                                                                 c
c  Find the smallest cube for which exactly five permutations of its digits are cube.             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 counter(5500), min_number(5500), digit(5500,10), n_perms, dummy, power, number(12), type(10)
      logical found, match

c initialize everything
      n_perms = 0
      do x1=1,5500
        counter(x1) = 0
        min_number(x1) = 10000
        do x2=1,10
          digit(x1,x2) = 0
        enddo
      enddo

c low number 4642, high number 9999
c This will look for a cube with 12 digits
      do x1=4642,9999
        dummy = x1**3
        
        do x2=1,12
          number(12) = 0
        enddo
        
c carry over the digits 
        power = 1
        do while(power.le.12)
          number(power) = (dummy - (dummy/(10**power))*10**power)/(10**(power-1))
          dummy = dummy - number(power)*(10**(power-1))
          power = power + 1
        enddo

c break down the number into the type of digits
        do x2=1,10
          type(x2) = 0
        enddo
        do x2=1,12
          type(number(x2)) = type(number(x2)) + 1
        enddo
        
c see if the permutation has already been found 
        found = .false.
        do x2=1,n_perms
          match = .true.
          do x3=1,10
            if (digit(x2,x3).ne.type(x3)) match = .false.
          enddo
          
          if (match) then
            found = .true.
            counter(x2) = counter(x2) + 1
            exit
          endif
        enddo
        
        if (.not.found) then
          n_perms = n_perms + 1
          counter(n_perms) = 1
          min_number(n_perms) = x1
          do x2=1,10
            digit(n_perms,x2) = type(x2)
          enddo
        endif
      enddo

c loop over all the digit combinations and see which one has 5 permutations
      do x1=1,n_perms
        if (counter(x1).eq.5) then
          write(*,*) 'The smallest cube with 5 permuations is ',min_number(x1)**3
          exit
        endif
      enddo
      
      end