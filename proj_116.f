c-------------------------------------------------------------------------------------------------c
      program project_euler_116
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  A row of five black square tiles is to have a number of its tiles replaced with coloured       c
c  oblong tiles chosen from red (length two), green (length three), or blue (length four).        c
c                                                                                                 c
c  If red tiles are chosen there are exactly seven ways this can be done.	                        c
c                                                                                                 c
c  If green tiles are chosen there are three ways.                                                c
c                                                                                                 c
c  And if blue tiles are chosen there are two ways.                                               c
c                                                                                                 c
c  Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of replacing the black     c
c  tiles in a row measuring five units in length.                                                 c
c                                                                                                 c
c  How many different ways can the black tiles in a row measuring fifty units in length be        c
c  replaced if colours cannot be mixed and at least one coloured tile must be used?               c
c                                                                                                 c
c  NOTE: This is related to problem 117.                                                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 spots, n, r, answer

c initialize the answer
      spots = 50
      answer = 0
      
c red tiles      
      do x1=1,spots/2
        n = spots-x1
        r = x1
        answer = answer + combo(n,r)
      enddo
      
c green tiles
      do x1=1,spots/3
        n = spots-2*x1
        r = x1
        answer = answer + combo(n,r)
      enddo

c blue tiles
      do x1=1,spots/4
        n = spots-3*x1
        r = x1
        answer = answer + combo(n,r)
      enddo
      
      write(*,*) 'The answer is ',answer
      
      end