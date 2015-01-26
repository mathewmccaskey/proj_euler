c-------------------------------------------------------------------------------------------------c
      program project_euler_112
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Working from left-to-right if no digit is exceeded by the digit to its left it is called an    c
c  increasing number; for example, 134468.                                                        c
c                                                                                                 c
c  Similarly if no digit is exceeded by the digit to its right it is called a decreasing number;  c
c  for example, 66420.                                                                            c
c                                                                                                 c
c  We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number;  c
c  for example, 155349.                                                                           c
c                                                                                                 c
c  Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the        c
c  numbers below one-thousand (525) are bouncy. In fact, the least number for which the           c
c  proportion of bouncy numbers first reaches 50% is 538.                                         c
c                                                                                                 c
c  Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the    c
c  proportion of bouncy numbers is equal to 90%.                                                  c
c                                                                                                 c
c  Find the least number for which the proportion of bouncy numbers is exactly 99%.               c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 nums(10), n_digits, num_bouncy, num_not_bouncy
      logical increasing, decreasing
      
c start with 2
      do x1=1,10
        nums(x1) = 0
      enddo
      nums(1) = 1
      n_digits = 1
      
c initialize the results
      num_bouncy = 0
      num_not_bouncy = 1
      
      do while((num_not_bouncy*99).ne.num_bouncy)

c increment the digit
        nums(1) = nums(1) + 1
        do x1=1,n_digits
          if ((nums(x1).eq.10).and.(x1.eq.n_digits)) then
            nums(x1) = nums(x1) - 10
            nums(x1+1) = nums(x1+1) + 1
            n_digits = n_digits + 1
          else if(nums(x1).eq.10) then
            nums(x1) = nums(x1) - 10
            nums(x1+1) = nums(x1+1) + 1
          endif
        enddo
C         write(*,fmt='(10(I3))') (nums(x1), x1=10,1,-1)
C         write(*,*) num_bouncy, num_not_bouncy

c check the digits to see if they are either increasing or decreasing
        increasing = .false.
        decreasing = .false.
        do x1=1,n_digits-1
          if (nums(x1).lt.nums(x1+1)) increasing = .true.
          if (nums(x1).gt.nums(x1+1)) decreasing = .true.
        enddo

c check to see if it is bouncy        
        if(increasing.and.decreasing) then
          num_bouncy = num_bouncy + 1
        else
          num_not_bouncy = num_not_bouncy + 1
        endif
      enddo
      
      write(*,*) 'The number where bouncy/non_bouncy = 99 is ',num_bouncy+num_not_bouncy
      
      end