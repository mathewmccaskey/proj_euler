c-------------------------------------------------------------------------------------------------c
      program project_euler_41
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n     c
c  exactly once. For example, 2143 is a 4-digit pandigital and is also prime.                     c
c                                                                                                 c
c  What is the largest n-digit pandigital prime that exists?                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 number
      
c we know that 8-digit and 9-digit pandigital numbers can't be prime
c the sum of 1-8 is 36 and the sum of 1-9 is 45 which means both pandigital numbers will be divisible by 3
c so we only need to find the largest 7 digit pandigital that's prime
      do x1=7,1,-1
        do x2=7,1,-1
          if (x1.ne.x2) then
          do x3=7,1,-1
            if ((x1.ne.x3).and.(x2.ne.x3)) then
            do x4=7,1,-1
              if ((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)) then
              do x5=7,1,-1
                if ((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)) then
                do x6=7,1,-1
                  if ((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                  do x7=7,1,-1
                    if ((x1.ne.x7).and.(x2.ne.x7).and.(x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                      number = x1*1000000 + x2*100000 + x3*10000 + x4*1000 + x5*100 + x6*10 + x7
                      if (is_prime(number)) then
                        write(*,*) 'The largest n-digit pandigital prime is ',number
                        stop
                      endif
                    endif
                  enddo
                  endif
                enddo
                endif
              enddo
              endif
            enddo
            endif
          enddo
          endif
        enddo
      enddo
                    
      end