c-------------------------------------------------------------------------------------------------c
      program project_euler_5
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without    c
c  any remainder.                                                                                 c
c                                                                                                 c
c  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to  c
c  20?                                                                                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this subroutine only
      integer*8 number
      logical works
      
c iniitialize our number and logical variable
      number = 1
      works = .false.
      
      do while(.not.(works))
        number = number + 1
        works = .true.
        do x1=1,20
          if (.not.(is_multiple(number, x1))) then
            works = .false.
          endif
        enddo
      enddo
      
      write(*,*) 'The number that has 1-20 as factors is ', number
      
      end