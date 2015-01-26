c-------------------------------------------------------------------------------------------------c
      program project_euler_473
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Let φ be the golden ratio: φ = (1+√5)2.                                                        c
c                                                                                                 c
c  Remarkably it is possible to write every positive integer as a sum of powers of φ even if we   c
c  require that every power of φ is used at most once in this sum.                                c
c  Even then this representation is not unique.                                                   c
c  We can make it unique by requiring that no powers with consecutive exponents are used and that c
c  the representation is finite.  E.g: 2=φ+φ^−2 and 3=φ^2+φ^−2                                    c
c                                                                                                 c
c  To represent this sum of powers of φ we use a string of 0's and 1's with a point to indicate   c
c  where the negative exponents start.                                                            c
c  We call this the representation in the phigital numberbase.                                    c
c  So 1=1_φ, 2=10.01_φ, 3=100.01_φ and 14=100100.001001_φ.                                        c
c  The strings representing 1, 2 and 14 in the phigital number base are palindromic, while the    c
c  string representating 3 is not.                                                                c
c  (the phigital point is not the middle character).                                              c
c                                                                                                 c
c  The sum of the positive integers not exceeding 1000 whose phigital representation is           c
c  palindromic is 4345.                                                                           c
c                                                                                                 c
c  Find the sum of the positive integers not exceeding 10^10 whose phigital representation is     c
c  palindromic.                                                                                   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters that are used in this program only
      integer*8 n, np1, i
      double precision ones_dp(50), root5_dp(50), factor, combo_div2

c parameters for finding this sum
      integer index, base(25), powers(24,2)
      double precision base_nums(24), sum, total
      logical legit, used(47)
      
c initialize everything
      do x1=1,50
        ones_dp(x1) = 0.d0
        root5_dp(x1) = 0.d0
      enddo
      
c loooooops to calculate \phi^{n} + \phi^{-(n+1)} since these will be numbers in this palindrome form
      do n=1,50
        np1 = n + 1
        do i=0,n
          if (((i/2)*2).eq.i) then
            factor = 5**(i/2)*combo_div2(n,i)
            ones_dp(n) = ones_dp(n) + factor
          else
            factor = 5**((i-1)/2)*combo_div2(n,i)
            root5_dp(n) = root5_dp(n) + factor
          endif
        enddo
        
        do i=0,np1            
          if (((i/2)*2).eq.i) then            
            factor = 5**(i/2)*combo_div2(np1,i)
            if (((n/2)*2).eq.n) then
              ones_dp(n) = ones_dp(n) - factor
            else
              ones_dp(n) = ones_dp(n) + factor
            endif
          else
            factor = 5**((i-1)/2)*combo_div2(np1,i)
            if (((n/2)*2).eq.n) then
              root5_dp(n) = root5_dp(n) + factor
            else
              root5_dp(n) = root5_dp(n) - factor
            endif
          endif
        enddo
C         write(*,*) n, ones_dp(n), root5_dp(n)
C         write(*,*) ones_dp(n)+root5_dp(n)*dsqrt(5.d0)
      enddo

c set up the base numbers
      base_nums(1) = 1
      powers(1,1) = 0
      powers(1,2) = 0
C       write(*,*) 1, base_nums(1), 1.d0
      base_nums(2) = 2
      powers(2,1) = 1
      powers(2,1) = 1
C       write(*,*) 2, base_nums(2), 2.d0
      index = 3
      do x1=2,47
        if ((dabs(root5_dp(x1)+root5_dp(x1+3)).lt.1.d-5).and.((ones_dp(x1)+ones_dp(x1+3)).lt.1.d10)) then
          base_nums(index) = ones_dp(x1) + ones_dp(x1+3)
          powers(index,1) = x1
          powers(index,2) = x1+3
C           write(*,*) index, base_nums(index), (ones_dp(x1) + ones_dp(x1+3)), x1, x1+3
          index = index + 1          
        endif
      enddo      
C       read(*,*)
      
c now we tabulate the total
      total = 1.d0
      do x1=1,25
        base(x1) = 0
      enddo
      base(2) = 1
      
      do while(base(25).eq.0)
c we aren't allowed to have consecutive powers
        legit = .true.
        do x1=1,47
          used(x1) = .false.
        enddo
        
        if (base(2).eq.1) used(1) = .true.
        do x1=3,24
          if (base(x1).eq.1) then
            if (.not.used(powers(x1,1))) then
              used(powers(x1,1)) = .true.
            else
              legit = .false.
            endif
            
            if (.not.used(powers(x1,2))) then
              used(powers(x1,2)) = .true.
            else
              legit = .false.
            endif
          endif
        enddo
          
        do x1=1,47
          if (used(x1).and.(used(x1+1))) legit = .false.
        enddo
        
C         write(*,fmt='(24(I1))') (base(x1), x1=1,24)
C         write(*,fmt='(24(L1))') (used(x1), x1=1,24)
C         write(*,*) legit
C         read(*,*)
        
        if (legit) then
          sum = 0.d0
          do x1=1,24
            if (base(x1).eq.1) sum = sum + base_nums(x1)
          enddo
          
          if (sum.lt.1.0d10) then
            total = total + sum
C             write(*,*) sum, total
C             write(*,fmt='(47(L1))') (used(x1), x1=1,47)
C             read(*,*)
          endif
        endif
        
        base(2) = base(2) + 1
        do x1=1,24
          if (base(x1).eq.2) then
            base(x1) = 0
            base(x1+1) = base(x1+1) + 1
          endif
        enddo
      enddo
      
      write(*,*) 'total = ',total
      end



c-------------------------------------------------------------------------------------------------c
      function combo_div2(n,r)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the combination of n things taken r at a time divided by 2^(n)           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 n, r
      double precision combo_div2

c parameters used in this function only
      integer numer_index, denom_index
      
c check to see what kind of index we're looking for in our denominator
      if (r.le.(n/2)) then
        denom_index = r
        numer_index = n-r+1
      else
        denom_index = n-r
        numer_index = r+1
      endif
      
      combo_div2 = 1.d0/2.d0**(n)
      do x1=n,numer_index,-1
        combo_div2 = combo_div2*x1
      enddo
      do x1=1,denom_index
        combo_div2 = combo_div2/x1
      enddo
            
      return
      end      