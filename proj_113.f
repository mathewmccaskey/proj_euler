c-------------------------------------------------------------------------------------------------c
      program project_euler_113
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Working from left-to-right if no digit is exceeded by the digit to its left it is called an    c
c  increasing number; for example, 134468.                                                        c
c                                                                                                 c
c  Similarly if no digit is exceeded by the digit to its right it is called a decreasing number;  c
c  for example, 66420.                                                                            c
c                                                                                                 c
c  We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number;  c
c  for example, 155349.                                                                           c
c                                                                                                 c
c  As n increases, the proportion of bouncy numbers below n increases such that there are only    c
c  12951 numbers below one-million that are not bouncy and only 277032 non-bouncy numbers below   c
c  10^10.                                                                                         c
c                                                                                                 c
c  How many numbers below a googol (10^100) are not bouncy?                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 answer, n, r
      
c solving this on pen and paper the result is C(109,9) + C(110,10) - 1002
      n = 109
      r = 9
      answer = combo(n,r)
      
      n = 110
      r = 10
      answer = answer + combo(n,r) - 1002
      
      write(*,*) 'The answer is ',answer
c      write(*,*) combo(50,2)
      
      end