c-------------------------------------------------------------------------------------------------c
      program project_euler_32
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n     c
c  exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.               c
c                                                                                                 c
c  The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand,        c
c  multiplier, and product is 1 through 9 pandigital.                                             c
c                                                                                                 c
c  Find the sum of all products whose multiplicand/multiplier/product identity can be written     c
c  as a 1 through 9 pandigital.  HINT: Some products can be obtained in more than one way so be   c
c  sure to only include it once in your sum.                                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 prod, digits1(1000), ndigits1, which1(0:9), digits2(1000), ndigits2, which2(0:9)
      integer*8 digits_prod(1000), ndigits_prod, which_prod(0:9), base, result
      integer*8 n_products, products(10)
      logical pass_test, unique_prod

c initialize parameters      
      base = 10
      result = 0
      n_products = 0
      do x1=1,10
        products(x1) = 0
      enddo
      
c looping over numbers to multiply
      do x1=1,10000
        do x2=x1+1,10000
          prod = x1*x2
          
          call get_digits(x1,base,digits1,ndigits1)
          call get_digits(x2,base,digits2,ndigits2)
          call get_digits(prod,base,digits_prod,ndigits_prod)
          call get_which_digits(digits1,ndigits1,which1)
          call get_which_digits(digits2,ndigits2,which2)
          call get_which_digits(digits_prod,ndigits_prod,which_prod)
          
          pass_test = .true.
          if ((which1(0)+which2(0)+which_prod(0)).ne.0) pass_test = .false.
          do x3=1,9
            if ((which1(x3)+which2(x3)+which_prod(x3)).ne.1) pass_test = .false.
          enddo
          
          if (pass_test) then
            write(*,*) x1,' x ',x2,' = ',prod
            unique_prod = .true.
            do x3=1,n_products
              if (products(x3).eq.prod) unique_prod = .false.
            enddo
            if (unique_prod) then
              result = result + prod
              n_products = n_products + 1
              products(n_products) = prod
            endif
              
          endif
        enddo
      enddo
      
      write(*,*) 'RESULT',result
      
      end