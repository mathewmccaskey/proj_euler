c-------------------------------------------------------------------------------------------------c
      program project_euler_80
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It is well known that if the square root of a natural number is not an integer, then it is     c
c  irrational. The decimal expansion of such square roots is infinite without any repeating       c
c  pattern at all.                                                                                c
c                                                                                                 c
c  The square root of two is 1.41421356237309504880..., and the digital sum of the first one      c
c  hundred decimal digits is 475.                                                                 c
c                                                                                                 c
c  For the first one hundred natural numbers, find the total of the digital sums of the first     c
c  one hundred decimal digits for all the irrational square roots.                                c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 power, root_digits(1000), nroot_digits, sq_digits(1000), nsq_digits
      integer*8 sq_compare(1000), nsq_compare, sub_answer, answer
      logical square, found_digit
      
c initialize the answer
      answer = 0
      
c loop over the natural numbers
      do x1=1,100
        square = .false.
        do x2=1,10
          if (x2**2.eq.x1) square = .true.
        enddo
        
c get rid of the squares        
        if (.not.square) then

c resetting the big numbers
          do x2=1,1000
            root_digits(x2) = 0
            sq_compare(x2) = 0
          enddo
          nroot_digits = 0
          nsq_compare = 0

c set up the ones digit of the square root (1-9)
          root_digits(1) = int(dsqrt(dble(x1)))
          nroot_digits = 1

c set up the number to compare the square root
          if (x1.lt.10) then
            sq_compare(1) = x1
            nsq_compare = 1
          else
            sq_compare(2) = x1/10
            sq_compare(1) = x1-(x1/10)*10
            nsq_compare = 2
          endif

c loop over all the digits we need to find          
          do x2=1,100
c shift over both the square root and the compare numbers
            do x3=nroot_digits,1,-1
              root_digits(x3+1) = root_digits(x3)
            enddo
            root_digits(1) = 0
            nroot_digits = nroot_digits + 1
            do x3=nsq_compare,1,-1
              sq_compare(x3+2) = sq_compare(x3)
            enddo
            sq_compare(2) = 0
            sq_compare(1) = 0
            nsq_compare = nsq_compare + 2

c loop over the first digit to find the correct one in the square root            
            found_digit = .false.
            do while(.not.found_digit)
              root_digits(1) = root_digits(1) + 1
              call big_number_product(root_digits,nroot_digits,root_digits,nroot_digits,sq_digits,nsq_digits)
              found_digit = .true.
              do x3=nsq_compare,1,-1
                if (sq_digits(x3).lt.sq_compare(x3)) then
                  found_digit = .false.
                  exit
                else if (sq_digits(x3).gt.sq_compare(x3)) then
                  root_digits(1) = root_digits(1) -1
                  exit
                endif
              enddo
              if (root_digits(1).gt.9) then
                root_digits(1) = 9
                exit
              endif
            enddo           
          enddo
          
          write(*,fmt='(101(I1))') (root_digits(x3), x3=nroot_digits,1,-1)
C It looks like we didn't have to carry over the last decimal....
C c carry over the 100th decimal if needed
C           if (root_digits(1).ge.5) then
C             root_digits(1) = 0
C             root_digits(2) = root_digits(2) + 1
C             do x2=2,101
C               if (root_digits(x2).eq.10) then
C                 root_digits(x2) = root_digits(x2) - 10
C                 root_digits(x2+1) = root_digits(x2+1) + 1
C               endif
C             enddo
C           else
C             root_digits(1) = 0
C           endif
C           write(*,fmt='(101(I1))') (root_digits(x3), x3=nroot_digits,1,-1)
          
c add the first 100 digits of the expansion to the answer
          sub_answer = 0
          do x2=2,101
            sub_answer = sub_answer + root_digits(x2)
          enddo
          answer = answer + sub_answer
          write(*,*) x1, sub_answer, answer
C           read(*,*)        
        endif
      enddo
      end 