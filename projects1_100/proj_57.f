c-------------------------------------------------------------------------------------------------c
      program project_euler_57
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It is possible to show that the square root of two can be expressed as an infinite continued   c
c  fraction.                                                                                      c
c                                                                                                 c
c  √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...                                           c
c                                                                                                 c
c  By expanding this for the first four iterations, we get:                                       c
c                                                                                                 c
c  1 + 1/2 = 3/2 = 1.5                                                                            c
c  1 + 1/(2 + 1/2) = 7/5 = 1.4                                                                    c
c  1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...                                                   c
c  1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...                                           c
c                                                                                                 c
c  The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, c
c  is the first example where the number of digits in the numerator exceeds the number of digits  c
c  in the denominator.                                                                            c
c                                                                                                 c
c  In the first one-thousand expansions, how many fractions contain a numerator with more digits  c
c  than denominator?                                                                              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 numer(1000), n_numer, denom(1000), n_denom, new_numer(1000), nn_numer, new_denom(1000), nn_denom
      integer*8 expand(0:1000), counter

c initialize the count      
      counter = 0
      
c set up the continued fraction values
      expand(0) = 1
      do x1=1,1000
        expand(x1) = 2
      enddo

c loop over the expansions
      do x1=1,1000

c initialize the damn numbers
        do x2=1,1000
          numer(x2) = 0
          denom(x2) = 0
          new_numer(x2) = 0
          new_denom(x2) = 0
        enddo
        n_denom = 1
        denom(1) = expand(x1)
        n_numer = 1
        numer(1) = 1

c loop over the terms in the continued fraction
        do x2=x1-1,1,-1
          do x3=1,n_denom
            new_denom(x3) = expand(x2)*denom(x3) + numer(x3)
            new_numer(x3) = denom(x3)
          enddo
          
c carry over the 10's
          nn_denom = n_denom
          do x3=1,n_denom
            do while (new_denom(x3).ge.10)
              new_denom(x3) = new_denom(x3) - 10
              new_denom(x3+1) = new_denom(x3+1) + 1
              if (x3.eq.n_denom) nn_denom = n_denom + 1
            enddo
          enddo
          
c copy over the new numerator and denominator to the old
          n_numer = n_denom
          n_denom = nn_denom
          do x3=1,n_numer
            numer(x3) = new_numer(x3)
          enddo
          do x3=1,n_denom
            denom(x3) = new_denom(x3)
          enddo
C           write(*,fmt='(100(I1))') (numer(x3), x3=n_numer,1,-1)
C           write(*,fmt='(100(I1))') (denom(x3), x3=n_denom,1,-1)
        enddo
        
c the final part of the continued fraction is slightly different
        do x2=1,max(n_denom,n_numer)
          new_numer(x2) = numer(x2) + denom(x2)
        enddo
        
c carry over the 10's
        nn_numer = max(n_denom,n_numer)
        do x2=1,nn_numer
          if (new_numer(x2).ge.10) then
            new_numer(x2) = new_numer(x2) - 10
            new_numer(x2+1) = new_numer(x2+1) + 1
            if (x2.eq.nn_numer) nn_numer = nn_numer + 1
          endif
        enddo
        
C         write(*,*) '----------------------------------------'
C         write(*,fmt='(100(I1))') (new_numer(x2), x2=nn_numer,1,-1)
C         write(*,fmt='(100(I1))') (denom(x2), x2=n_denom,1,-1)
C         write(*,*) '----------------------------------------'
C         write(*,*) '----------------------------------------'
        
        if (nn_numer.gt.n_denom) counter = counter + 1
      enddo

      write(*,*) 'The number of iterations where the numerator has more digits than the denominator is ',counter

      end