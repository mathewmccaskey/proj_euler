c-------------------------------------------------------------------------------------------------c
      program project_euler_92
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A number chain is created by continuously adding the square of the digits in a number to form  c
c  a new number until it has been seen before.                                                    c
c                                                                                                 c
c  For example,                                                                                   c
c                                                                                                 c
c  44 → 32 → 13 → 10 → 1 → 1                                                                      c
c  85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89                                                c
c                                                                                                 c
c  Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most c
c  amazing is that EVERY starting number will eventually arrive at 1 or 89.                       c
c                                                                                                 c
c  How many starting numbers below ten million will arrive at 89?                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num_89, number, base, digit(1000), ndigits
      
c initialize
      num_89 = 0
      base = 10
      
      do x1=2,10000000
        call get_digits(x1,base,digit,ndigits)
        number = x1
        
        do while ((number.ne.1).and.(number.ne.89))
          number = 0
          do x2=1,ndigits
            number = number + digit(x2)**2
          enddo
          
          call get_digits(number,base,digit,ndigits)
        enddo
        
        if (number.eq.89) num_89 = num_89 + 1
      enddo
      
      write(*,*) 'THe number of numbers that go to 89 is ',num_89
      
      end    