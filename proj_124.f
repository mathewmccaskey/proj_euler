c-------------------------------------------------------------------------------------------------c
      program project_euler_124
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The radical of n, rad(n), is the product of the distinct prime factors of n. For example,      c
c  504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.                                             c
c                                                                                                 c
c  If we calculate rad(n) for 1 ≤ n ≤ 10, then sort them on rad(n), and sorting on n if the       c
c  radical values are equal, we get:                                                              c
c                           Unsorted                    Sorted                                    c
c                        n          rad(n)         n     rad(n)      k                            c
c                        1            1            1       1         1                            c
c                        2            2            2       2         2                            c
c                        3            3            4       2         3                            c
c                        4            2            8       2         4                            c
c                        5            5            3       3         5                            c
c                        6            6            9       3         6                            c
c                        7            7            5       5         7                            c
c                        8            2            6       6         8                            c
c                        9            3            7       7         9                            c
c                       10           10           10      10        10                            c
c  Let E(k) be the kth element in the sorted n column; for example, E(4) = 8 and E(6) = 9.        c
c                                                                                                 c
c  If rad(n) is sorted for 1 ≤ n ≤ 100000, find E(10000).                                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 primes(9592), num_primes, num_sorted, sorted_rad(100000), sorted_k(100000), rad, spot
      logical found_spot

c get the primes
      num_primes = 0
      do x1=1,100000
        if (is_prime(x1)) then
          num_primes = num_primes + 1
          primes(num_primes) = x1
        endif
      enddo

c initalize the list 
      num_sorted = 2
      sorted_rad(1) = 1
      sorted_k(1) = 1
      sorted_rad(2) = 2
      sorted_k(2) = 2

c loop over the rest of the numbers      
      do x1=3,100000
c      do x1=3,10
        rad = 1        
        do x2=1,num_primes
          if (((x1/primes(x2))*primes(x2)) .eq. x1) then
            rad = rad*primes(x2)
          endif
        enddo

c find where this goes in the sorted list and add it
        if (rad.ge.sorted_rad(num_sorted)) then
          sorted_rad(num_sorted+1) = rad
          sorted_k(num_sorted+1) = x1
          num_sorted = num_sorted + 1
        else
          found_spot = .false.
          spot = num_sorted
          do while(.not.found_spot)
            spot = spot-1
            if (rad.ge.sorted_rad(spot)) then
              found_spot = .true.
              spot = spot + 1
            endif
          enddo
          
          do x2=num_sorted,spot,-1
            sorted_rad(x2+1) = sorted_rad(x2)
            sorted_k(x2+1) = sorted_k(x2)
          enddo
          sorted_rad(spot) = rad
          sorted_k(spot) = x1
          num_sorted = num_sorted + 1
        endif
      enddo

      open(unit=42,file='output.txt')
      do x1=1,1000
        write(42,*) sorted_k(x1), sorted_rad(x1)
      enddo
      close(42)
      
      write(*,*) 'E(10000) = ',sorted_k(10000)
c      do x1=1,10
c        write(*,*) sorted_k(x1), sorted_rad(x1)
c      enddo
c      write(*,*) 'E(4) = ',sorted_k(4)
      
      end