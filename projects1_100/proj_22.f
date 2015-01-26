c-------------------------------------------------------------------------------------------------c
      program project_euler_26
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over    c
c  five-thousand first names, begin by sorting it into alphabetical order. Then working out the   c
c  alphabetical value for each name, multiply this value by its alphabetical position in the      c
c  list to obtain a name score.                                                                   c
c                                                                                                 c
c  For example, when the list is sorted into alphabetical order, COLIN, which is worth            c
c  3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of    c
c  938 × 53 = 49714.                                                                              c
c                                                                                                 c
c  What is the total of all the name scores in the file?                                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      character names(5163)*20, dummy*20
      integer*8 order(5163), value(5163), character_to_number, total, num_names

c initialize the number of names      
      num_names = 5163

c open up the file and read the names
      open(unit=42, file='input_22.txt')
      do x1=1,5163
        read(42,*) names(x1)
      enddo
      close(42)
      
c insertion sort
      do x1=1,num_names
        order(x1) = 0
      enddo
      order(1) = 1
      
      do x1=2,num_names
        do x2=1,x1-1
          if ((names(x1).lt.names(order(x2))).and.(order(x1).eq.0)) then
            do x3=x1,x2,-1
              order(x3) = order(x3-1)
            enddo
            order(x2) = x1
          endif
        enddo
        
        if (order(x1).eq.0) order(x1) = x1
      enddo

      do x1=1,num_names
        value(x1) = 0
        do x2=1,20
          value(x1) = value(x1) + character_to_number(names(x1)(x2:x2))
        enddo
      enddo

      write(*,*) 'order of first 10 names and values'
      do x1=1,10
        write(*,fmt='(I5,1X,20A,1X,20A,1X,I5)') x1, names(x1), names(order(x1)), value(order(x1))
        write(*,*) value(order(x1))
      enddo

      total = 0
      do x1=1,num_names
        total = total + value(order(x1))*x1
      enddo
      
      write(*,*) names(order(938)), value(order(938)), 938*value(order(938))
      
      write(*,*) 'The total of the name order * value is ',total
      
      end
  
  
      
c-------------------------------------------------------------------------------------------------c
      function character_to_number(char)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function takes an input character and returns the value associated with that character    c
c  ' ' = 0, 'A' = 1, 'B' = 2, 'C' = 3, etc.                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      character char
      
c function definition
      integer*8 character_to_number
      
      if (char.eq.' ') character_to_number = 0
      if (char.eq.'A') character_to_number = 1
      if (char.eq.'B') character_to_number = 2
      if (char.eq.'C') character_to_number = 3
      if (char.eq.'D') character_to_number = 4
      if (char.eq.'E') character_to_number = 5
      if (char.eq.'F') character_to_number = 6
      if (char.eq.'G') character_to_number = 7
      if (char.eq.'H') character_to_number = 8
      if (char.eq.'I') character_to_number = 9
      if (char.eq.'J') character_to_number = 10
      if (char.eq.'K') character_to_number = 11
      if (char.eq.'L') character_to_number = 12
      if (char.eq.'M') character_to_number = 13
      if (char.eq.'N') character_to_number = 14
      if (char.eq.'O') character_to_number = 15
      if (char.eq.'P') character_to_number = 16
      if (char.eq.'Q') character_to_number = 17
      if (char.eq.'R') character_to_number = 18
      if (char.eq.'S') character_to_number = 19
      if (char.eq.'T') character_to_number = 20
      if (char.eq.'U') character_to_number = 21
      if (char.eq.'V') character_to_number = 22
      if (char.eq.'W') character_to_number = 23
      if (char.eq.'X') character_to_number = 24
      if (char.eq.'Y') character_to_number = 25
      if (char.eq.'Z') character_to_number = 26
      
      return
      end