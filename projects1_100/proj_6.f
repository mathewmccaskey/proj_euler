c-------------------------------------------------------------------------------------------------c
      program project_euler_6
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The sum of the squares of the first ten natural numbers is,                                    c
c  1^2 + 2^2 + ... + 10^2 = 385                                                                   c
c                                                                                                 c
c  The square of the sum of the first ten natural numbers is,                                     c
c  (1 + 2 + ... + 10)^2 = 55^2 = 3025                                                             c
c                                                                                                 c
c  Hence the difference between the sum of the squares of the first ten natural numbers and the   c
c  square of the sum is 3025 − 385 = 2640.                                                        c
c                                                                                                 c
c  Find the difference between the sum of the squares of the first one hundred natural numbers    c
c  and the square of the sum.                                                                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 n_numbers, square1, square2, diff

c initialize the number of natural numbers we use and the squares
      n_numbers = 100
      square1 = 0
      square2 = 0

c loop over all the integers and add the squares
      do x1=1, n_numbers
        square1 = square1 + x1**2
      enddo
        
c The square of the sum is just the square of the traingular number
      square2 = (triangle(n_numbers))**2
      diff = square2 - square1
      
      write(*,*) 'The sum of squares is ',square1
      write(*,*) 'The square of sums is ',square2
      write(*,*) 'The difference between the square of sums and the sum of squares is ',diff
      
      end