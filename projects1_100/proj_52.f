c-------------------------------------------------------------------------------------------------c
      program project_euler_52
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It can be seen that the number, 125874, and its double, 251748, contain exactly the same       c
c  digits, but in a different order.                                                              c
c                                                                                                 c
c  Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same      c
c  digits.                                                                                        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 number, base, digit(1000), ndigit, number_base, which_digit_base(0:9), which_digit(0:9)
      logical found_it
      
c initialize
      found_it = .false.
      number_base = 1
      base = 10
      
      do while(.not.(found_it))
        found_it = .true.
        number = number_base
        
        call get_digits(number,base,digit,ndigit)
        call get_which_digits(digit,ndigit,which_digit_base)
        
        do x1=2,6
          number = number_base*x1
          call get_digits(number,base,digit,ndigit)
          call get_which_digits(digit,ndigit,which_digit)
          
          do x2=0,9
            if (which_digit(x2).ne.which_digit_base(x2)) found_it = .false.
          enddo
          
          if (.not.(found_it)) then
            number_base = number_base + 1
            exit
          endif
        enddo
      enddo
      
      write(*,*) 'The smallest integer such that x, 2x, 3x, 4x, 5x, and 6x share the same digits is ',number_base
      
      end