c-------------------------------------------------------------------------------------------------c
      program project_euler_264
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Barbara is a mathematician and a basketball player. She has found that the probability of      c
c  scoring a point when shooting from a distance x is exactly (1 - x/q), where q is a real        c
c  constant greater than 50.                                                                      c
c                                                                                                 c
c  During each practice run, she takes shots from distances x = 1, x = 2, ..., x = 50 and,        c
c  according to her records, she has precisely a 2 % chance to score a total of exactly 20 points.c
c                                                                                                 c
c  Find q and give your answer rounded to 10 decimal places.                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      double precision get_prob, low, prob_low, high, prob_high, mid, prob_mid
      logical done

      low = 50.d0
      prob_low = get_prob(low)
      high = 60.d0
      prob_high = get_prob(high)
      write(*,*) prob_low, prob_high
      read(*,*)
      
      done = .false.
      do while(.not.done)
        mid = (low+high)/2.d0
        prob_mid = get_prob(mid)
        
        if ((prob_mid-0.02d0).gt.0.d0) then
          low = mid
        else
          high = mid
        endif
        
        if ((high-low).lt.1.0d-13) then
          done = .true.
          write(*,*) prob_mid
        endif
      enddo
      
      write(*,*) high
      
      end



c-------------------------------------------------------------------------------------------------c
      function get_prob(q)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the probability to make 20 baskets given factor q                        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters and function definition
      double precision q, get_prob
      
c parameters used in this function only
      double precision prob(0:50,0:50)
      
c initialize the probabilities      
      do x1=0,50
        do x2=0,50
          prob(x1,x2) = 0.d0
        enddo
      enddo
      
      prob(0,0) = 1.d0
c loop over the number of shots
      do x1=1,50
        prob(0,x1) = 0.d0
c loop over the number of shots made
        do x2=0,x1
          prob(x1,x2) = prob(x1-1,x2-1)*(1.d0-dble(x1)/q) + prob(x1-1,x2)*dble(x1)/q
        enddo
      enddo
      
      get_prob = prob(50,20)
      
      return
      end