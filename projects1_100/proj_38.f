c-------------------------------------------------------------------------------------------------c
      program project_euler_38
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Take the number 192 and multiply it by each of 1, 2, and 3:                                    c
c                                                                                                 c
c     192 × 1 = 192                                                                               c
c     192 × 2 = 384                                                                               c
c     192 × 3 = 576                                                                               c
c                                                                                                 c
c  By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576  c
c  the concatenated product of 192 and (1,2,3)                                                    c
c                                                                                                 c
c  The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the   c
c  pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).                 c
c                                                                                                 c
c  What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated    c
c  product of an integer with (1,2, ... , n) where n > 1?                                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 base_number, base, base_digits(1000), n_digits, which_digits_base(0:9)
      integer*8 factor_digits(1000), n_factor_digits, which_digits(0:9)
      integer*8 total_digits, factor(1000), one, cat_digits(9), cat_number, max_number, max_base
      logical valid_number
      
c initialize
      max_number = 0
      base = 10
      one = 1
      
c loop over the base number
      do base_number=1,49999

        do x1=1,9
          cat_digits(x1) = 0
        enddo

c get the digits of the base number
        call get_digits(base_number,base,base_digits,n_digits)
        call get_which_digits(base_digits,n_digits,which_digits_base)
c        write(*,fmt='(10(I2))') (which_digits_base(x1), x1=0,9)

c initalize variables
        valid_number = .true.
        total_digits = n_digits
        factor(1) = 2
        do x1=1,total_digits
          cat_digits(x1) = base_digits(total_digits-x1+1)
        enddo

c if the base number has a zero or has two common digits then the number automatically fails
        if (which_digits_base(0).ne.0) valid_number = .false.
        do x1=1,9
          if (which_digits_base(x1).gt.2) valid_number = .false.
        enddo
        
        do while((valid_number).and.(total_digits.ne.9))
          
          call big_number_product(base_digits,n_digits,factor,one,factor_digits,n_factor_digits)
          call get_which_digits(factor_digits,n_factor_digits,which_digits)

          do x1=0,9
            which_digits_base(x1) = which_digits_base(x1) + which_digits(x1)
          enddo
          
c          write(*,fmt='(10(I2))') (which_digits_base(x1), x1=0,9)
          
          if (which_digits_base(0).ne.0) valid_number = .false.
          do x1=1,9
            if (which_digits_base(x1).gt.1) valid_number = .false.
          enddo          
          
          do x1=1,n_factor_digits
            cat_digits(total_digits+x1) = factor_digits(n_factor_digits-x1+1)
          enddo
          
          total_digits = total_digits + n_factor_digits
          factor(1) = factor(1) + 1
        enddo
        
        if (valid_number) then
          cat_number = 0
          do x1=1,9
            cat_number = cat_number + cat_digits(x1)*10**(9-x1)
          enddo
          
          if (cat_number.gt.max_number) then
            max_number = cat_number
            max_base = base_number
          endif
        endif
        
c        write(*,*) base_number, valid_number, total_digits
c        write(*,fmt='(9(I2))') (cat_digits(x1), x1=1,9)
c        if (base_number.eq.10) read(*,*)
      enddo
      
      write(*,*) 'The largest pandigital number starts with base ',max_base
      write(*,*) max_number
      
      end