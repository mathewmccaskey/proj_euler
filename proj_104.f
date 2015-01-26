c-------------------------------------------------------------------------------------------------c
      program project_euler_104
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The Fibonacci sequence is defined by the recurrence relation:                                  c
c                                                                                                 c
c    F_n = F_n−1 + F_n−2, where F_1 = 1 and F_2 = 1.                                              c
c                                                                                                 c
c  It turns out that F_541, which contains 113 digits, is the first Fibonacci number for which    c
c  the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in c
c  corder). And F_2749, which contains 575 digits, is the first Fibonacci number for which the    c
c  first nine digits are 1-9 pandigital.                                                          c
c                                                                                                 c
c  Given that F_k is the first Fibonacci number for which the first nine digits AND the last nine c
c  digits are 1-9 pandigital, find k.                                                             c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 fib1(100000), fib2(100000), fib3(100000), n1, n2, n3, k
      logical done, front_digits(9), front_good, back_digits(9), back_good
      
c initialize some parameters
      do x1=1,100000
        fib1(x1) = 0
        fib2(x1) = 0
        fib3(x1) = 0
      enddo
      fib1(1) = 1
      n1=1
      fib2(1) = 1
      n2=1
      n3=1
      k=2
      done = .false.
      
      do while(.not. done)
        
c add fib1 and fib2 to get the next fibonacci number
        k = k + 1
        do x1=1,10000
          fib3(x1) = 0
        enddo
        
        do x1=1,n2
          fib3(x1) = fib1(x1) + fib2(x1)
        enddo
        do x1=1,n2
          if (fib3(x1).ge.10) then
            fib3(x1) = fib3(x1) - 10
            fib3(x1+1) = fib3(x1+1) + 1
          endif
        enddo
        if (fib3(n2+1).gt.0) n3 = n2 + 1
        
C         write(*,fmt='(100(I1))') (fib3(x1),x1=n3,1,-1)
C         read(*,*)
        
c check to see if fib3 fits the criteria
        do x1=1,9
          front_digits(x1) = .false.
          back_digits(x1) = .false.
        enddo
        
        if (n2.gt.18) then
          do x1=1,9
            back_digits(fib3(x1)) = .true.
            front_digits(fib3(n3+1-x1)) = .true.
          enddo
          
          front_good = .true.
          back_good = .true.
          do x1=1,9
            if (.not. front_digits(x1)) front_good = .false.
            if (.not. back_digits(x1)) back_good = .false.
          enddo
          
          if (back_good) then
            write(*,*) 'Back good at k = ',k,n3
c            write(*,fmt='(1000(I1))') (fib3(x1),x1=n3,1,-1)
c            read(*,*)
          endif
          
          if (front_good) then
            write(*,*) 'Front good at k = ',k,n3
c            write(*,fmt='(1000(I1))') (fib3(x1),x1=n3,1,-1)
c            read(*,*)
          endif
          
          if (front_good .and. back_good) then
            write(*,*) 'found it at k = ',k,n3
c            write(*,fmt='(1000(I1))') (fib3(x1),x1=n3,1,-1)
            done = .true.
          endif
        endif
        
c switch the places of fib1, fib2, and fib3
        do x1=1,n3
          fib1(x1) = fib2(x1)
          fib2(x1) = fib3(x1)
        enddo
        n1 = n2
        n2 = n3
      enddo
      
      end