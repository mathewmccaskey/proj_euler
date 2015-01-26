c-------------------------------------------------------------------------------------------------c
      program project_euler_88
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A natural number, N, that can be written as the sum and product of a given set of at least     c
c  two natural numbers, {a1, a2, ... , ak} is called a product-sum number:                        c
c  N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.                                                   c
c                                                                                                 c
c  For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.                                                        c
c                                                                                                 c
c  For a given set of size, k, we shall call the smallest N with this property a minimal          c
c  product-sum number. The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6    c
c  are as follows.                                                                                c
c                                                                                                 c
c  k=2: 4 = 2 × 2 = 2 + 2                                                                         c
c  k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3                                                                 c
c  k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4                                                         c
c  k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2                                                 c
c  k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6                                        c
c                                                                                                 c
c  Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8  c
c  is only counted once in the sum.                                                               c
c                                                                                                 c
c  In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is                      c
c  {4, 6, 8, 12, 15, 16}, the sum is 61.                                                          c
c                                                                                                 c
c  What is the sum of all the minimal product-sum numbers for 2≤k≤12000?                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 k_min(2:12000), factor, prod, sum, result
      logical unique
      
c initailze the variables
      factor = 24000
      do x1=2,12000
        k_min(x1) = 2*x1
      enddo

      write(*,*) 'initialization done'
c loop over two multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/2.d0))
        do x2=2,factor/x1+1
          prod = x1*x2
          sum  = x1+x2
          if ((2+prod-sum).le.12000) then
            if (prod .lt. k_min(2+prod-sum)) then
              k_min(2+prod-sum) = prod
            endif
          endif
        enddo
      enddo
      write(*,*) 'two done'
      
c loop over three multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/3.d0))
        do x2=2,factor/(2*x1)+1
          do x3=2,factor/(x1*x2)+1
            prod = x1*x2*x3
            sum  = x1+x2+x3
            if ((3+prod-sum).le.12000) then
              if (prod .lt. k_min(3+prod-sum)) then
                k_min(3+prod-sum) = prod
              endif
            endif
          enddo
        enddo
      enddo
      write(*,*) 'three done'
      
c loop over four multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/4.d0))
        do x2=2,factor/((2**2)*x1)+1
          do x3=2,factor/(2*x1*x2)+1
            do x4=2,factor/(x1*x2*x3)+1
              prod = x1*x2*x3*x4 
              sum  = x1+x2+x3+x4
              if ((4+prod-sum).le.12000) then
                if (prod .lt. k_min(4+prod-sum)) then
                  k_min(4+prod-sum) = prod
                endif
              endif
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'four done'


c loop over five multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/5.d0))
        do x2=2,factor/((2**3)*x1)+1
          do x3=2,factor/((2**2)*x1*x2)+1
            do x4=2,factor/(2*x1*x2*x3)+1
              do x5=2,factor/(x1*x2*x3*x4)+1
                prod = x1*x2*x3*x4*x5
                sum  = x1+x2+x3+x4+x5
                if ((5+prod-sum).le.12000) then
                  if (prod .lt. k_min(5+prod-sum)) then
                    k_min(5+prod-sum) = prod
                  endif
                endif
              enddo
            enddo
          enddo
        enddo
      enddo 
      write(*,*) 'five done'

c loop over six multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/6.d0))
        do x2=2,factor/((2**4)*x1)+1
          do x3=2,factor/((2**3)*x1*x2)+1
            do x4=2,factor/((2**2)*x1*x2*x3)+1
              do x5=2,factor/(2*x1*x2*x3*x4)+1
                do x6=2,factor/(x1*x2*x3*x4*x5)+1
                  prod = x1*x2*x3*x4*x5*x6
                  sum  = x1+x2+x3+x4+x5+x6
                  if ((6+prod-sum).le.12000) then
                    if (prod .lt. k_min(6+prod-sum)) then
                      k_min(6+prod-sum) = prod
                    endif
                  endif
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo      
      write(*,*) 'six done'

c loop over seven multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/7.d0))
        do x2=2,factor/((2**5)*x1)+1
          do x3=2,factor/((2**4)*x1*x2)+1
            do x4=2,factor/((2**3)*x1*x2*x3)+1
              do x5=2,factor/((2**2)*x1*x2*x3*x4)+1
                do x6=2,factor/(2*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/(x1*x2*x3*x4*x5*x6)+1
                    prod = x1*x2*x3*x4*x5*x6*x7
                    sum  = x1+x2+x3+x4+x5+x6+x7
                    if ((7+prod-sum).le.12000) then
                      if (prod .lt. k_min(7+prod-sum)) then
                        k_min(7+prod-sum) = prod
                      endif
                    endif
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'seven done'

c loop over eight multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/8.d0))
        do x2=2,factor/((2**6)*x1)+1
          do x3=2,factor/((2**5)*x1*x2)+1
            do x4=2,factor/((2**4)*x1*x2*x3)+1
              do x5=2,factor/((2**3)*x1*x2*x3*x4)+1
                do x6=2,factor/((2**2)*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/(2*x1*x2*x3*x4*x5*x6)+1
                    do x8=2,factor/(x1*x2*x3*x4*x5*x6*x7)+1
                      prod = x1*x2*x3*x4*x5*x6*x7*x8
                      sum  = x1+x2+x3+x4+x5+x6+x7+x8
                      if ((8+prod-sum).le.12000) then
                        if (prod .lt. k_min(8+prod-sum)) then
                          k_min(8+prod-sum) = prod
                        endif
                      endif
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'eight done'

c loop over nine multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/9.d0))
        do x2=2,factor/((2**7)*x1)+1
          do x3=2,factor/((2**6)*x1*x2)+1
            do x4=2,factor/((2**5)*x1*x2*x3)+1
              do x5=2,factor/((2**4)*x1*x2*x3*x4)+1
                do x6=2,factor/((2**3)*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/((2**2)*x1*x2*x3*x4*x5*x6)+1
                    do x8=2,factor/(2*x1*x2*x3*x4*x5*x6*x7)+1
                      do x9=2,factor/(x1*x2*x3*x4*x5*x6*x7*x8)+1
                        prod = x1*x2*x3*x4*x5*x6*x7*x8*x9
                        sum  = x1+x2+x3+x4+x5+x6+x7+x8+x9
                        if ((9+prod-sum).le.12000) then
                          if (prod .lt. k_min(9+prod-sum)) then
                            k_min(9+prod-sum) = prod
                          endif
                        endif
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'nine done'

c loop over ten multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/10.d0))
        do x2=2,factor/((2**8)*x1)+1
          do x3=2,factor/((2**7)*x1*x2)+1
            do x4=2,factor/((2**6)*x1*x2*x3)+1
              do x5=2,factor/((2**5)*x1*x2*x3*x4)+1
                do x6=2,factor/((2**4)*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/((2**3)*x1*x2*x3*x4*x5*x6)+1
                    do x8=2,factor/((2**2)*x1*x2*x3*x4*x5*x6*x7)+1
                      do x9=2,factor/(2*x1*x2*x3*x4*x5*x6*x7*x8)+1
                        do x10=2,factor/(x1*x2*x3*x4*x5*x6*x7*x8*x9)+1
                          prod = x1*x2*x3*x4*x5*x6*x7*x8*x9*x10
                          sum  = x1+x2+x3+x4+x5+x6+x7+x8+x9+x10
                          if ((10+prod-sum).le.12000) then
                            if (prod .lt. k_min(10+prod-sum)) then
                              k_min(10+prod-sum) = prod
                            endif
                          endif
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'ten done'

c loop over eleven multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/11.d0))
        do x2=2,factor/((2**9)*x1)+1
          do x3=2,factor/((2**8)*x1*x2)+1
            do x4=2,factor/((2**7)*x1*x2*x3)+1
              do x5=2,factor/((2**6)*x1*x2*x3*x4)+1
                do x6=2,factor/((2**5)*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/((2**4)*x1*x2*x3*x4*x5*x6)+1
                    do x8=2,factor/((2**3)*x1*x2*x3*x4*x5*x6*x7)+1
                      do x9=2,factor/((2**2)*x1*x2*x3*x4*x5*x6*x7*x8)+1
                        do x10=2,factor/(2*x1*x2*x3*x4*x5*x6*x7*x8*x9)+1
                          do x11=2,factor/(x1*x2*x3*x4*x5*x6*x7*x8*x9*x10)+1
                            prod = x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11
                            sum  = x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11
                            if ((11+prod-sum).le.12000) then
                              if (prod .lt. k_min(11+prod-sum)) then
                                k_min(11+prod-sum) = prod
                              endif
                            endif
                          enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'eleven done'

c loop over twelve multiplicative factors
      do x1=2,factor/int((dble(factor))**(1.d0/12.d0))
        do x2=2,factor/((2**10)*x1)+1
          do x3=2,factor/((2**9)*x1*x2)+1
            do x4=2,factor/((2**8)*x1*x2*x3)+1
              do x5=2,factor/((2**7)*x1*x2*x3*x4)+1
                do x6=2,factor/((2**6)*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/((2**5)*x1*x2*x3*x4*x5*x6)+1
                    do x8=2,factor/((2**4)*x1*x2*x3*x4*x5*x6*x7)+1
                      do x9=2,factor/((2**3)*x1*x2*x3*x4*x5*x6*x7*x8)+1
                        do x10=2,factor/((2**2)*x1*x2*x3*x4*x5*x6*x7*x8*x9)+1
                          do x11=2,factor/(2*x1*x2*x3*x4*x5*x6*x7*x8*x9*x10)+1
                            do x12=2,factor/(x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11)+1
                              prod = x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11*x12
                              sum  = x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12
                              if ((12+prod-sum).le.12000) then
                                if (prod .lt. k_min(12+prod-sum)) then
                                  k_min(12+prod-sum) = prod
                                endif
                              endif
                            enddo
                          enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'twelve done'

c loop over thirteen multiplicative factors
      do x1=2,int((dble(factor))**(1.d0/13.d0))
        do x2=2,factor/((2**11)*x1)+1
          do x3=2,factor/((2**10)*x1*x2)+1
            do x4=2,factor/((2**9)*x1*x2*x3)+1
              do x5=2,factor/((2**8)*x1*x2*x3*x4)+1
                do x6=2,factor/((2**7)*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/((2**6)*x1*x2*x3*x4*x5*x6)+1
                    do x8=2,factor/((2**5)*x1*x2*x3*x4*x5*x6*x7)+1
                      do x9=2,factor/((2**4)*x1*x2*x3*x4*x5*x6*x7*x8)+1
                        do x10=2,factor/((2**3)*x1*x2*x3*x4*x5*x6*x7*x8*x9)+1
                          do x11=2,factor/((2**2)*x1*x2*x3*x4*x5*x6*x7*x8*x9*x10)+1
                            do x12=2,factor/(2*x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11)+1
                              do x13=2,factor/(x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11*x12)+1
                                prod = x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11*x12*x13
                                sum  = x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13
                                if ((13+prod-sum).le.12000) then
                                  if (prod .lt. k_min(13+prod-sum)) then
                                    k_min(13+prod-sum) = prod
                                  endif
                                endif
                              enddo
                            enddo
                          enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'thirteen done'

c loop over fourteen multiplicative factors (last one because 2**15 > 24000)
      do x1=2,int((dble(factor))**(1.d0/14.d0))+1
        do x2=2,factor/((2**12)*x1)+1
          do x3=2,factor/((2**11)*x1*x2)+1
            do x4=2,factor/((2**10)*x1*x2*x3)+1
              do x5=2,factor/((2**9)*x1*x2*x3*x4)+1
                do x6=2,factor/((2**8)*x1*x2*x3*x4*x5)+1
                  do x7=2,factor/((2**7)*x1*x2*x3*x4*x5*x6)+1
                    do x8=2,factor/((2**6)*x1*x2*x3*x4*x5*x6*x7)+1
                      do x9=2,factor/((2**5)*x1*x2*x3*x4*x5*x6*x7*x8)+1
                        do x10=2,factor/((2**4)*x1*x2*x3*x4*x5*x6*x7*x8*x9)+1
                          do x11=2,factor/((2**3)*x1*x2*x3*x4*x5*x6*x7*x8*x9*x10)+1
                            do x12=2,factor/((2**2)*x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11)+1
                              do x13=2,factor/(2*x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11*x12)+1
                                do x14=2,factor/(x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11*x12*x13)+1
                                  prod = x1*x2*x3*x4*x5*x6*x7*x8*x9*x10*x11*x12*x13*x14
                                  sum  = x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14
                                  if ((14+prod-sum).le.12000) then
                                    if (prod .lt. k_min(14+prod-sum)) then
                                      k_min(14+prod-sum) = prod
                                    endif
                                  endif
                                enddo
                              enddo
                            enddo
                          enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      write(*,*) 'fourteen done'

c now to put together the result
      result = 0
      do x1=2,12000
        unique = .true.
        do x2=(x1-1),2,-1
          if (k_min(x1).eq.k_min(x2)) unique = .false.
        enddo
        
        if (unique) result = result + k_min(x1)
      enddo
        
      write(*,*) result
      
      end