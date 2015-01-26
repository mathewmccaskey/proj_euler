c-------------------------------------------------------------------------------------------------c
      program project_euler_33
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to   c
c  simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by         c
c  cancelling the 9s.                                                                             c
c                                                                                                 c
c  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.                         c
c                                                                                                 c
c  There are exactly four non-trivial examples of this type of fraction, less than one in value,  c
c  and containing two digits in the numerator and denominator.                                    c
c                                                                                                 c
c  If the product of these four fractions is given in its lowest common terms, find the value of  c
c  the denominator.                                                                               c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 numer(2), denom(2)
      
c initialize numbers
      numer(1) = 1
      numer(2) = 0
      
c loop over numerator
      do while((numer(1).ne.9).or.(numer(2).ne.9))
        
c initialize denomenator
        denom(2) = numer(2) + 1
        if (denom(2).eq.10) then
          denom(2) = 0
          denom(1) = numer(1) + 1
        else
          denom(1) = numer(1)
        endif
        
c loop over denomenator
        do while((denom(1).ne.9).or.(denom(2).ne.9))
          
c check if we can just cancel the 1st digit
          if ((denom(2)*(10*numer(1)+numer(2))).eq.(numer(2)*(10*denom(1)+denom(2)))) then
            if (numer(1).eq.denom(1)) then
              write(*,*) 'FOUND ONE: cancel 1st digit'
              write(*,fmt='(2(I1),1A,2(I1))') numer(1),numer(2),'/',denom(1),denom(2)
            endif
          endif
          
c check if we can just cancel the 2nd digit (and that digit isn't 0)
          if ((denom(1)*(10*numer(1)+numer(2))).eq.(numer(1)*(10*denom(1)+denom(2)))) then
            if ((denom(2).ne.0).and.(numer(2).ne.0).and.(numer(2).eq.denom(2))) then
              write(*,*) 'FOUND ONE: cancel 2nd digit'
              write(*,fmt='(2(I1),1A,2(I1))') numer(1),numer(2),'/',denom(1),denom(2)
            endif
          endif
          
c check if we can cancel the first digit of the numerator and the second digit of the denom
          if ((denom(1)*(10*numer(1)+numer(2))).eq.(numer(2)*(10*denom(1)+denom(2)))) then
            if (numer(1).eq.denom(2)) then
              write(*,*) 'FOUND ONE: 1st digit numerator and 2nd digit denomenator'
              write(*,fmt='(2(I1),1A,2(I1))') numer(1),numer(2),'/',denom(1),denom(2)
            endif
          endif

c check if we can cancel the second digit of the numerator and the first digit of the denom
          if ((denom(2)*(10*numer(1)+numer(2))).eq.(numer(1)*(10*denom(1)+denom(2)))) then
            if (numer(2).eq.denom(1)) then
              write(*,*) 'FOUND ONE: 2nd digit numerator and 1st digit denomenator'
              write(*,fmt='(2(I1),1A,2(I1))') numer(1),numer(2),'/',denom(1),denom(2)
            endif
          endif
          
c increment denominator
          denom(2) = denom(2) + 1
          if (denom(2).eq.10) then
            denom(2) = 0
            denom(1) = denom(1) + 1
          endif
        enddo
        
c increment numerator
        numer(2) = numer(2) + 1
        if (numer(2).eq.10) then
          numer(2) = 0
          numer(1) = numer(1) + 1
        endif
      enddo
      
      end