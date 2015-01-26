c-------------------------------------------------------------------------------------------------c
      program project_euler_51
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate    c
c  (polygonal) numbers and are generated by the following formulae:                               c
c  Triangle 	  	P3,n=n(n+1)/2 	  	1, 3, 6, 10, 15, ...                                        c
c  Square    	  	P4,n=n^2 	  	      1, 4, 9, 16, 25, ...                                        c
c  Pentagonal 	  P5,n=n(3n−1)/2 	  	1, 5, 12, 22, 35, ...                                       c
c  Hexagonal 	  	P6,n=n(2n−1) 	  	  1, 6, 15, 28, 45, ...                                       c
c  Heptagonal   	P7,n=n(5n−3)/2 	  	1, 7, 18, 34, 55, ...                                       c
c  Octagonal 	  	P8,n=n(3n−2) 	  	  1, 8, 21, 40, 65, ...                                       c
c                                                                                                 c
c  The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.  c
c                                                                                                 c
c  The set is cyclic, in that the last two digits of each number is the first two digits of the   c
c  next number (including the last number with the first).                                        c
c                                                                                                 c
c                                                                                                 c
c  Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882), c
c  is represented by a different number in the set.                                               c
c  This is the only set of 4-digit numbers with this property.                                    c
c                                                                                                 c
c  Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal    c
c  type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a  c
c  different number in the set.                                                                   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 numbers(351), first(351), last(351), type(351), counter

c here are the ranges of the polygonal numbers that produces a 4 digit number
c triangle numbers:   45-140
c square numbers:     32-99
c pentagon numbers:   26-81
c hexagonal numbers:  23-70
c heptagonal numbers: 21-63
c octagonal numbers:  19-58

c initialize the numbers
      counter = 0
c triangle
      do x1=45,140
        counter = counter + 1
        numbers(counter) = x1*(x1+1)/2
        type(counter) = 3
      enddo
c square
      do x1=32,99
        counter = counter + 1
        numbers(counter) = x1**2
        type(counter) = 4
      enddo
c pentagon
      do x1=26,81
        counter = counter + 1
        numbers(counter) = x1*(3*x1-1)/2
        type(counter) = 5
      enddo
c hexagon
      do x1=23,70
        counter = counter + 1
        numbers(counter) = x1*(2*x1-1)
        type(counter) = 6
      enddo
c heptagon
      do x1=21,63
        counter = counter + 1
        numbers(counter) = x1*(5*x1-3)/2
        type(counter) = 7
      enddo
c octagon
      do x1=19,58
        counter = counter + 1
        numbers(counter) = x1*(3*x1-2)
        type(counter) = 8
      enddo

c break the numbers into the first two digits and the last two digits      
      do x1=1,351
        first(x1) = numbers(x1)/100
        last(x1) = numbers(x1) - (numbers(x1)/100)*100
      enddo

c loop to find the combination of 6 numbers that are completely cyclical and cover all polygonal functions
      do x1=1,351
        do x2=1,351
          if ((x1.ne.x2).and.
     .        (type(x1).ne.type(x2)).and.
     .        (last(x1).eq.first(x2))) then
          do x3=1,351
            if (((x1.ne.x3).and.(x2.ne.x3)).and.
     .          ((type(x1).ne.type(x3)).and.(type(x2).ne.type(x3))).and.
     .          (last(x2).eq.first(x3))) then
            do x4=1,351
              if (((x1.ne.x4).and.(x2.ne.x4).and.(x3.ne.x4)).and.
     .            ((type(x1).ne.type(x4)).and.(type(x2).ne.type(x4)).and.(type(x3).ne.type(x4))).and.
     .            (last(x3).eq.first(x4))) then
              do x5=1,351
                if (((x1.ne.x5).and.(x2.ne.x5).and.(x3.ne.x5).and.(x4.ne.x5)).and.
     .              ((type(x1).ne.type(x5)).and.(type(x2).ne.type(x5)).and.(type(x3).ne.type(x5))
     .                .and.(type(x4).ne.type(x5))).and.
     .              (last(x4).eq.first(x5))) then
                do x6=1,351
                  if (((x1.ne.x6).and.(x2.ne.x6).and.(x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)).and.
     .              ((type(x1).ne.type(x6)).and.(type(x2).ne.type(x6)).and.(type(x3).ne.type(x6))
     .                .and.(type(x4).ne.type(x6)).and.(type(x5).ne.type(x6))).and.
     .                (last(x5).eq.first(x6))) then
                    if (last(x6).eq.first(x1)) then
                      write(*,*) 'SOLUTION FOUND'
                      write(*,*) numbers(x1), type(x1)
                      write(*,*) numbers(x2), type(x2)
                      write(*,*) numbers(x3), type(x3)
                      write(*,*) numbers(x4), type(x4)
                      write(*,*) numbers(x5), type(x5)
                      write(*,*) numbers(x6), type(x6)
                      write(*,*) 'Sum = ',numbers(x1)+numbers(x2)+numbers(x3)+numbers(x4)+numbers(x5)+numbers(x6)
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
      enddo
      
      end