c-------------------------------------------------------------------------------------------------c
      program project_euler_55
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.                           c
c                                                                                                 c
c  Not all numbers produce palindromes so quickly. For example,                                   c
c                                                                                                 c
c  349 + 943 = 1292                                                                               c
c  1292 + 2921 = 4213                                                                             c
c  4213 + 3124 = 7337                                                                             c
c                                                                                                 c
c  That is, 349 took three iterations to arrive at a palindrome.                                  c
c                                                                                                 c
c  Although no one has proved it yet, it is thought that some numbers, like 196, never produce a  c
c  palindrome. A number that never forms a palindrome through the reverse and add process is      c
c  called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose   c
c  of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition  c
c  you are given that for every number below ten-thousand, it will either (i) become a palindrome c
c  in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has  c
c  managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to    c
c  require over fifty iterations before producing a palindrome: 4668731596684224866951378664      c
c  (53 iterations, 28-digits).                                                                    c
c                                                                                                 c
c  Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first     c
c  example is 4994.                                                                               c
c                                                                                                 c
c  How many Lychrel numbers are there below ten-thousand?                                         c
c                                                                                                 c
c  NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of    c
c  Lychrel numbers.                                                                               c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num_lychrel, a(1000), len_a, b(1000), len_b, c(1000), len_c, base, counter
      logical is_lychrel

c initializing
      num_lychrel = 0
      base = 10

c loop over the integers up to 10000
      do x1=1,9999
        call get_digits(x1,base,a,len_a)
        
        counter = 0
        is_lychrel = .true.
        
        do while ((counter.le.50).and.(is_lychrel))

c make b the reverse of a
          len_b = len_a
          do x2=1,len_b
            b(len_b-x2+1) = a(x2)
          enddo

c add the number with its reverse          
          call big_number_sum(a,len_a,b,len_b,c,len_c)
          counter = counter + 1

c check if the result is a palindrome
          if (is_palindrome(c,len_c)) is_lychrel = .false.

c set the sum to the new a          
          len_a = len_c
          do x2=1,len_c
            a(x2) = c(x2)
          enddo
        enddo
        
        if (is_lychrel) num_lychrel = num_lychrel + 1
      enddo
      
      write(*,*) 'The number of lychrel numbers below 10000 is ',num_lychrel
      
      end