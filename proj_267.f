c-------------------------------------------------------------------------------------------------c
      program project_euler_267
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  You are given a unique investment opportunity.                                                 c
c                                                                                                 c
c  Starting with £1 of capital, you can choose a fixed proportion, f, of your capital to bet on a c
c  fair coin toss repeatedly for 1000 tosses.                                                     c
c                                                                                                 c
c  Your return is double your bet for heads and you lose your bet for tails.                      c
c                                                                                                 c
c  For example, if f = 1/4, for the first toss you bet £0.25, and if heads comes up you win £0.5  c
c  and so then have £1.5. You then bet £0.375 and if the second toss is tails, you have £1.125.   c
c                                                                                                 c
c  Choosing f to maximize your chances of having at least £1,000,000,000 after 1,000 flips, what  c
c  is the chance that you become a billionaire?                                                   c
c                                                                                                 c
c  All computations are assumed to be exact (no rounding), but give your answer rounded to 12     c
c  digits behind the decimal point in the form 0.abcdefghijkl.                                    c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      double precision combo_div2, total
      integer*8 n, r, min_flips
      
c so I calculated the minimum number of flips we would need to reach 1 billion which is 431
      n = 1000
      min_flips = 431
      total = 1.d0
      
      do r=0,min_flips
        total = total - combo_div2(n,r)
      enddo
      
      write(*,*) 'Probability to earn 1 billion quid = ',total
      
      end



c-------------------------------------------------------------------------------------------------c
      function combo_div2(n,r)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the combination of n things taken r at a time divided by 2^(n)           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 n, r
      double precision combo_div2

c parameters used in this function only
      integer numer_index, denom_index
      
c check to see what kind of index we're looking for in our denominator
      if (r.le.(n/2)) then
        denom_index = r
        numer_index = n-r+1
      else
        denom_index = n-r
        numer_index = r+1
      endif
      
      combo_div2 = 1.d0/2.d0**(n)
      do x1=n,numer_index,-1
        combo_div2 = combo_div2*x1/(n-x1+1)
      enddo
            
      return
      end      