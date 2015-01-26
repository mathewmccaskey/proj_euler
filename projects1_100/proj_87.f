c-------------------------------------------------------------------------------------------------c
      program project_euler_87
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The smallest number expressible as the sum of a prime square, prime cube, and prime fourth     c
c  power is 28. In fact, there are exactly four numbers below fifty that can be expressed in      c
c  such a way:                                                                                    c
c                                                                                                 c
c  28 = 2^2 + 2^3 + 2^4                                                                           c
c  33 = 3^2 + 2^3 + 2^4                                                                           c
c  49 = 5^2 + 2^3 + 2^4                                                                           c
c  47 = 2^2 + 3^3 + 2^4                                                                           c
c                                                                                                 c
c  How many numbers below fifty million can be expressed as the sum of a prime square, prime      c
c  cube, and prime fourth power?                                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c  Parameters used in this program only      
      integer*8 quarts(23), cubes(73), squares(908), sums(908,73,23), result, index, index1, index2, index3
      logical unique(908,73,23)
      
c  initializing the prime powers
      index = 0
      do x1=1,84
        if (is_prime(x1)) then
          index = index + 1
          quarts(index) = x1**4
        endif
      enddo
      
      index = 0
      do x1=1,368
        if (is_prime(x1)) then
          index = index + 1
          cubes(index) = x1**3
        endif
      enddo
      
      index = 0
      do x1=1,7071
        if (is_prime(x1)) then
          index = index + 1
          squares(index) = x1**2
        endif
      enddo

c  Tabulate all the sums in the 3 dimensional array
      do x1=1,908
        do x2=1,73
          do x3=1,23
            sums(x1,x2,x3) = squares(x1) + cubes(x2) + quarts(x3)
            unique(x1,x2,x3) = .true.  
          enddo
        enddo
      enddo

c  Loop over all the sums and check if it is unique
      result = 0
      do x1=1,908
        do x2=1,73
          do x3=1,23
            if ((unique(x1,x2,x3)).and.(sums(x1,x2,x3).le.50000000)) then
              result = result + 1
              if (((result/1000)*1000).eq.result) then
                write(*,fmt='(5(5I,1X))') x1,x2,x3,sums(x1,x2,x3),result
              endif
              index1 = 1
              do while((squares(index1).lt.sums(x1,x2,x3)).and.(index1.le.908))
                index2 = 1
                do while(((squares(index1)+cubes(index2)).lt.sums(x1,x2,x3)).and.(index2.le.73))
                  index3 = 1
                  do while(((squares(index1)+cubes(index2)+quarts(index3)).le.sums(x1,x2,x3)).and.(index3.le.23))
                    if (((x1.ne.index1).or.(x2.ne.index2).or.(x3.ne.index3)).and.
     .    (sums(index1,index2,index3).eq.sums(x1,x2,x3))) then
                      unique(index1,index2,index3) = .false.
c                      write(*,fmt='(4(5I,1X))') index1,index2,index3,sums(index1,index2,index3)
                    endif
                    index3 = index3 + 1
                  enddo  
                  index2 = index2 + 1
                enddo
                index1 = index1 + 1
              enddo
            endif
          enddo
        enddo
      enddo
                
      write(*,*) 'The number of numbers less than 50 million that can be written as a sum of '
      write(*,*) 'a prime square, cube, and quart is ',result
      
      end