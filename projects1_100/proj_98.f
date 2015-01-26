c-------------------------------------------------------------------------------------------------c
      program project_euler_98
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, we form a  c
c  square number: 1296 = 36^2. What is remarkable is that, by using the same digital              c
c  substitutions, the anagram, RACE, also forms a square number: 9216 = 96^2. We shall call CARE  c
c  (and RACE) a square anagram word pair and specify further that leading zeroes are not          c
c  permitted, neither may a different letter have the same digital value as another letter.       c
c                                                                                                 c
c  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly  c
c  two-thousand common English words, find all the square anagram word pairs (a palindromic word  c
c  is NOT considered to be an anagram of itself).                                                 c
c                                                                                                 c
c  What is the largest square number formed by any member of such a pair?                         c
c                                                                                                 c
c  NOTE: All anagrams formed must be contained in the given text file.                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 value, letters(1786,0:26), n_letters(1786), uni_letters, digits1(8), digits2(8), num1, num2
      integer*8 test_square1, test_square2, max_square
      character*20 words(1786), word, word1, word2
      character chars(8)
      logical anagram, is_square

c initialize some parameters
      max_square = 0
      do x1=1,1786
        do x2=0,26
          letters(x1,x2) = 0
        enddo
      enddo
      
c open up the input file
      open(unit=42, file='input_98.txt')
      do x1=1,1786
        read(42,*) word
        
c Initialize the letters for each word
        do x2=1,20
          value = character_to_number(word(x2:x2))
          letters(x1,value) = letters(x1,value) + 1
        enddo

        words(x1) = word
        n_letters(x1) = 20-letters(x1,0)
      enddo
      close(42)
      
      do x1=1,1785
        do x2=x1+1,1786
          anagram = .true.
          do x3=0,26
            if (letters(x1,x3).ne.letters(x2,x3)) anagram = .false.
          enddo
          if (anagram) then
            write(*,*) 'ANAGRAM: ',words(x1),words(x2)

c find the number of unique letters and 
            uni_letters = 0
            do x3=1,26
              if (letters(x1,x3).ne.0) then
                uni_letters = uni_letters + 1
                chars(uni_letters) = number_to_character(x3)
              endif
            enddo
c            write(*,*) uni_letters, (chars(x3),x3=1,uni_letters)
              
c two digit squares (as well as three digit squares with only two unique letters) 
c don't have any anagrams so we start with 3 unique numbers
            if (uni_letters.eq.3) then
              do x3=0,9
                do x4=0,9
                  if (x3.ne.x4) then
                  do x5=0,9
                    if ((x3.ne.x5).and.(x4.ne.x5)) then

                      word1 = words(x1)
                      do x10=1,n_letters(x1)
                        if (word1(x10:x10).eq.chars(1)) then
                          digits1(x10) = x3
                        else if (word1(x10:x10).eq.chars(2)) then
                          digits1(x10) = x4
                        else if (word1(x10:x10).eq.chars(3)) then
                          digits1(x10) = x5
                        endif
                      enddo
                      num1 = 0
                      do x10=1,n_letters(x1)
                        num1 = num1 + digits1(x10)*10**(n_letters(x1)-x10)
                      enddo
                      
                      test_square1 = 10
                      do while ((test_square1**2).lt.num1)
                        test_square1 = test_square1 + 1
                      enddo
                    
                      if (((test_square1**2).eq.num1).and.(digits1(1).ne.0)) then
                        word2 = words(x2)
                        do x10=1,n_letters(x2)
                          if (word2(x10:x10).eq.chars(1)) then
                            digits2(x10) = x3
                          else if (word2(x10:x10).eq.chars(2)) then
                            digits2(x10) = x4
                          else if (word2(x10:x10).eq.chars(3)) then
                            digits2(x10) = x5
                          endif
                        enddo
                        num2 = 0
                        do x10=1,n_letters(x2)
                          num2 = num2 + digits2(x10)*10**(n_letters(x2)-x10)
                        enddo
                      
                        test_square2 = 10
                        do while ((test_square2**2).lt.num2)
                          test_square2 = test_square2 + 1
                        enddo

                        if (((test_square2**2).eq.num2).and.(digits2(1).ne.0)) then
C                           write(*,*) 'word 1 = ',word1
C                           write(*,*) 'number 1 = ',num1
C                           write(*,*) 'word 2 = ',word2
C                           write(*,*) 'number 2 = ',num2
C                           write(*,*)
C                           write(*,*) 'SQUARE ANAGRAM'
C                           write(*,*) 'number 1 = ',test_square1,' squared'
C                           write(*,*) 'number 2 = ',test_square2,' squared'
C                           read(*,*)
                          if (num1.gt.max_square) max_square = num1
                          if (num2.gt.max_square) max_square = num2
                        endif
                      endif
                    
                    endif
                  enddo
                  endif
                enddo
              enddo

c four unique letters                                     
            else if (uni_letters.eq.4) then
              do x3=0,9
                do x4=0,9
                  if (x3.ne.x4) then
                  do x5=0,9
                    if ((x3.ne.x5).and.(x4.ne.x5)) then
                    do x6=0,9
                      if ((x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then

                        word1 = words(x1)
                        do x10=1,n_letters(x1)
                          if (word1(x10:x10).eq.chars(1)) then
                            digits1(x10) = x3
                          else if (word1(x10:x10).eq.chars(2)) then
                            digits1(x10) = x4
                          else if (word1(x10:x10).eq.chars(3)) then
                            digits1(x10) = x5
                          else if (word1(x10:x10).eq.chars(4)) then
                            digits1(x10) = x6
                          endif  
                        enddo
                        num1 = 0
                        do x10=1,n_letters(x1)
                          num1 = num1 + digits1(x10)*10**(n_letters(x1)-x10)
                        enddo
                      
                        test_square1 = 31
                        do while ((test_square1**2).lt.num1)
                          test_square1 = test_square1 + 1
                        enddo
                    
                        if (((test_square1**2).eq.num1).and.(digits1(1).ne.0)) then
                          word2 = words(x2)
                          do x10=1,n_letters(x2)
                            if (word2(x10:x10).eq.chars(1)) then
                              digits2(x10) = x3
                            else if (word2(x10:x10).eq.chars(2)) then
                              digits2(x10) = x4
                            else if (word2(x10:x10).eq.chars(3)) then
                              digits2(x10) = x5
                            else if (word2(x10:x10).eq.chars(4)) then
                              digits2(x10) = x6
                            endif
                          enddo
                          num2 = 0
                          do x10=1,n_letters(x2)
                            num2 = num2 + digits2(x10)*10**(n_letters(x2)-x10)
                          enddo
                      
                          test_square2 = 31
                          do while ((test_square2**2).lt.num2)
                            test_square2 = test_square2 + 1
                          enddo
              
                          if (((test_square2**2).eq.num2).and.(digits2(1).ne.0)) then
C                             write(*,*) 'word 1 = ',word1
C                             write(*,*) 'number 1 = ',num1
C                             write(*,*) 'word 2 = ',word2
C                             write(*,*) 'number 2 = ',num2
C                             write(*,*)
C                             write(*,*) 'SQUARE ANAGRAM'
C                             write(*,*) 'number 1 = ',test_square1,' squared'
C                             write(*,*) 'number 2 = ',test_square2,' squared'
C                             read(*,*)
                            if (num1.gt.max_square) max_square = num1
                            if (num2.gt.max_square) max_square = num2
                          endif
                        endif
                      endif
                    enddo
                    endif
                  enddo
                  endif
                enddo
              enddo

c five unique letters              
            else if (uni_letters.eq.5) then
              do x3=0,9
                do x4=0,9
                  if (x3.ne.x4) then
                  do x5=0,9
                    if ((x3.ne.x5).and.(x4.ne.x5)) then
                    do x6=0,9
                      if ((x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                      do x7=0,9
                        if ((x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then

                          word1 = words(x1)
                          do x10=1,n_letters(x1)
                            if (word1(x10:x10).eq.chars(1)) then
                              digits1(x10) = x3
                            else if (word1(x10:x10).eq.chars(2)) then
                              digits1(x10) = x4
                            else if (word1(x10:x10).eq.chars(3)) then
                              digits1(x10) = x5
                            else if (word1(x10:x10).eq.chars(4)) then
                              digits1(x10) = x6
                            else if (word1(x10:x10).eq.chars(5)) then
                              digits1(x10) = x7
                            endif  
                          enddo
                          num1 = 0
                          do x10=1,n_letters(x1)
                            num1 = num1 + digits1(x10)*10**(n_letters(x1)-x10)
                          enddo
                      
                          test_square1 = 100
                          do while ((test_square1**2).lt.num1)
                            test_square1 = test_square1 + 1
                          enddo
                    
                          if (((test_square1**2).eq.num1).and.(digits1(1).ne.0)) then
                            word2 = words(x2)
                            do x10=1,n_letters(x2)
                              if (word2(x10:x10).eq.chars(1)) then
                                digits2(x10) = x3
                              else if (word2(x10:x10).eq.chars(2)) then
                                digits2(x10) = x4
                              else if (word2(x10:x10).eq.chars(3)) then
                                digits2(x10) = x5
                              else if (word2(x10:x10).eq.chars(4)) then
                                digits2(x10) = x6
                              else if (word2(x10:x10).eq.chars(5)) then
                                digits2(x10) = x7
                              endif
                            enddo
                            num2 = 0
                            do x10=1,n_letters(x2)
                              num2 = num2 + digits2(x10)*10**(n_letters(x2)-x10)
                            enddo
                      
                            test_square2 = 100
                            do while ((test_square2**2).lt.num2)
                              test_square2 = test_square2 + 1
                            enddo
              
                            if (((test_square2**2).eq.num2).and.(digits2(1).ne.0)) then
C                               write(*,*) 'word 1 = ',word1
C                               write(*,*) 'number 1 = ',num1
C                               write(*,*) 'word 2 = ',word2
C                               write(*,*) 'number 2 = ',num2
C                               write(*,*)
C                               write(*,*) 'SQUARE ANAGRAM'
C                               write(*,*) 'number 1 = ',test_square1,' squared'
C                               write(*,*) 'number 2 = ',test_square2,' squared'
C                               read(*,*)
                              if (num1.gt.max_square) max_square = num1
                              if (num2.gt.max_square) max_square = num2
                            endif
                          endif
                        endif
                      enddo
                      endif
                    enddo
                    endif
                  enddo
                  endif
                enddo
              enddo

c six unique letters            
            else if (uni_letters.eq.6) then
              do x3=0,9
                do x4=0,9
                  if (x3.ne.x4) then
                  do x5=0,9
                    if ((x3.ne.x5).and.(x4.ne.x5)) then
                    do x6=0,9
                      if ((x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                      do x7=0,9
                        if ((x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                        do x8=0,9
                          if ((x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8).and.(x7.ne.x8)) then
                            word1 = words(x1)
                            do x10=1,n_letters(x1)
                              if (word1(x10:x10).eq.chars(1)) then
                                digits1(x10) = x3
                              else if (word1(x10:x10).eq.chars(2)) then
                                digits1(x10) = x4
                              else if (word1(x10:x10).eq.chars(3)) then
                                digits1(x10) = x5
                              else if (word1(x10:x10).eq.chars(4)) then
                                digits1(x10) = x6
                              else if (word1(x10:x10).eq.chars(5)) then
                                digits1(x10) = x7
                              else if (word1(x10:x10).eq.chars(6)) then
                                digits1(x10) = x8
                              endif  
                            enddo
                            num1 = 0
                            do x10=1,n_letters(x1)
                              num1 = num1 + digits1(x10)*10**(n_letters(x1)-x10)
                            enddo
                      
                            test_square1 = 316
                            do while ((test_square1**2).lt.num1)
                              test_square1 = test_square1 + 1
                            enddo
                    
                            if (((test_square1**2).eq.num1).and.(digits1(1).ne.0)) then
                              word2 = words(x2)
                              do x10=1,n_letters(x2)
                                if (word2(x10:x10).eq.chars(1)) then
                                  digits2(x10) = x3
                                else if (word2(x10:x10).eq.chars(2)) then
                                  digits2(x10) = x4
                                else if (word2(x10:x10).eq.chars(3)) then
                                  digits2(x10) = x5
                                else if (word2(x10:x10).eq.chars(4)) then
                                  digits2(x10) = x6
                                else if (word2(x10:x10).eq.chars(5)) then
                                  digits2(x10) = x7
                                else if (word2(x10:x10).eq.chars(6)) then
                                  digits2(x10) = x8
                                endif
                              enddo
                              num2 = 0
                              do x10=1,n_letters(x2)
                                num2 = num2 + digits2(x10)*10**(n_letters(x2)-x10)
                              enddo
                      
                              test_square2 = 316
                              do while ((test_square2**2).lt.num2)
                                test_square2 = test_square2 + 1
                              enddo
              
                              if (((test_square2**2).eq.num2).and.(digits2(1).ne.0)) then
C                                 write(*,*) 'word 1 = ',word1
C                                 write(*,*) 'number 1 = ',num1
C                                 write(*,*) 'word 2 = ',word2
C                                 write(*,*) 'number 2 = ',num2
C                                 write(*,*)
C                                 write(*,*) 'SQUARE ANAGRAM'
C                                 write(*,*) 'number 1 = ',test_square1,' squared'
C                                 write(*,*) 'number 2 = ',test_square2,' squared'
C                                 read(*,*)
                                if (num1.gt.max_square) max_square = num1
                                if (num2.gt.max_square) max_square = num2
                              endif
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

c seven unique letters              
            else if (uni_letters.eq.7) then
              do x3=0,9
                do x4=0,9
                  if (x3.ne.x4) then
                  do x5=0,9
                    if ((x3.ne.x5).and.(x4.ne.x5)) then
                    do x6=0,9
                      if ((x3.ne.x6).and.(x4.ne.x6).and.(x5.ne.x6)) then
                      do x7=0,9
                        if ((x3.ne.x7).and.(x4.ne.x7).and.(x5.ne.x7).and.(x6.ne.x7)) then
                        do x8=0,9
                          if ((x3.ne.x8).and.(x4.ne.x8).and.(x5.ne.x8).and.(x6.ne.x8).and.(x7.ne.x8)) then
                          do x9=0,9
                            if ((x3.ne.x9).and.(x4.ne.x9).and.(x5.ne.x9).and.(x6.ne.x9).and.
     .    (x7.ne.x9).and.(x8.ne.x9)) then
                            
                              word1 = words(x1)
                              do x10=1,n_letters(x1)
                                if (word1(x10:x10).eq.chars(1)) then
                                  digits1(x10) = x3
                                else if (word1(x10:x10).eq.chars(2)) then
                                  digits1(x10) = x4
                                else if (word1(x10:x10).eq.chars(3)) then
                                  digits1(x10) = x5
                                else if (word1(x10:x10).eq.chars(4)) then
                                  digits1(x10) = x6
                                else if (word1(x10:x10).eq.chars(5)) then
                                  digits1(x10) = x7
                                else if (word1(x10:x10).eq.chars(6)) then
                                  digits1(x10) = x8
                                else if (word1(x10:x10).eq.chars(7)) then
                                  digits1(x10) = x9
                                endif  
                              enddo
                              num1 = 0
                              do x10=1,n_letters(x1)
                                num1 = num1 + digits1(x10)*10**(n_letters(x1)-x10)
                              enddo
                      
                              test_square1 = 1000
                              do while ((test_square1**2).lt.num1)
                                test_square1 = test_square1 + 1
                              enddo
                    
                              if (((test_square1**2).eq.num1).and.(digits1(1).ne.0)) then
                                word2 = words(x2)
                                do x10=1,n_letters(x2)
                                  if (word2(x10:x10).eq.chars(1)) then
                                    digits2(x10) = x3
                                  else if (word2(x10:x10).eq.chars(2)) then
                                    digits2(x10) = x4
                                  else if (word2(x10:x10).eq.chars(3)) then
                                    digits2(x10) = x5
                                  else if (word2(x10:x10).eq.chars(4)) then
                                    digits2(x10) = x6
                                  else if (word2(x10:x10).eq.chars(5)) then
                                    digits2(x10) = x7
                                  else if (word2(x10:x10).eq.chars(6)) then
                                    digits2(x10) = x8
                                  else if (word2(x10:x10).eq.chars(7)) then
                                    digits2(x10) = x9
                                  endif
                                enddo
                                num2 = 0
                                do x10=1,n_letters(x2)
                                  num2 = num2 + digits2(x10)*10**(n_letters(x2)-x10)
                                enddo
                      
                                test_square2 = 1000
                                do while ((test_square2**2).lt.num2)
                                  test_square2 = test_square2 + 1
                                enddo
              
                                if (((test_square2**2).eq.num2).and.(digits2(1).ne.0)) then
                                  write(*,*) 'word 1 = ',word1
                                  write(*,*) 'number 1 = ',num1
                                  write(*,*) 'word 2 = ',word2
                                  write(*,*) 'number 2 = ',num2
                                  write(*,*)
                                  write(*,*) 'SQUARE ANAGRAM'
                                  write(*,*) 'number 1 = ',test_square1,' squared'
                                  write(*,*) 'number 2 = ',test_square2,' squared'
                                  read(*,*)
                                  if (num1.gt.max_square) max_square = num1
                                  if (num2.gt.max_square) max_square = num2
                                endif
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
              
            else if (uni_letters.eq.8) then
            endif
          endif
        enddo
      enddo
      
      write(*,*) 'The largest sqaure formed by a square anagram is ',max_square
      
      end