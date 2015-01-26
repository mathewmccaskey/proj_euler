c-------------------------------------------------------------------------------------------------c
      program project_euler_119
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The number 512 is interesting because it is equal to the sum of its digits raised to some      c
c  power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a number with this property is         c
c  614656 = 28^4.                                                                                 c
c                                                                                                 c
c  We shall define an to be the nth term of this sequence and insist that a number must contain   c
c  at least two digits to have a sum.                                                             c
c                                                                                                 c
c  You are given that a2 = 512 and a10 = 614656.                                                  c
c                                                                                                 c
c  Find a30.                                                                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program
      integer*8 nums(30), num_index, base, number, digit(1000), ndigits, sum, power, dummy
      logical done_sorting
      
c initialize everything
      num_index = 0
      base = 10
      number = 10
      
      do while(num_index.lt.30)
        do x1=2,600
          do x2=1,50
            number = x1**x2
            
            if (number.gt.10) then
              call get_digits(number,base,digit,ndigits)
              
              sum = 0
              do x3=1,ndigits
                sum = sum + digit(x3)
              enddo
        
              if (sum.eq.x1) then
                num_index = num_index + 1
                nums(num_index) = number
                write(*,*) num_index, number
              endif
            endif
          enddo
        enddo
      enddo
      
      done_sorting = .false.
      do while(not(done_sorting))
        done_sorting = .true.
        do x1=1,num_index-1
          if (nums(x1).gt.nums(x2)) then
            done_sorting = .false.
            dummy = nums(x1)
            nums(x1) = nums(x2)
            nums(x2) = dummy
          endif
        enddo
      enddo

      write(*,*) 'The 30th number that works is ',nums(30)
      
      end