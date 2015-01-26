c-------------------------------------------------------------------------------------------------c
      program project_euler_64
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c The first ten continued fraction representations of (irrational) square roots are:              c
c                                                                                                 c
c  √2=[1;(2)], period=1                                                                           c
c  √3=[1;(1,2)], period=2                                                                         c
c  √5=[2;(4)], period=1                                                                           c
c  √6=[2;(2,4)], period=2                                                                         c
c  √7=[2;(1,1,1,4)], period=4                                                                     c
c  √8=[2;(1,4)], period=2                                                                         c
c  √10=[3;(6)], period=1                                                                          c
c  √11=[3;(3,6)], period=2                                                                        c
c  √12= [3;(2,6)], period=2                                                                       c
c  √13=[3;(1,1,1,1,6)], period=5                                                                  c
c                                                                                                 c
c  Exactly four continued fractions, for N ≤ 13, have an odd period.                              c
c                                                                                                 c
c  How many continued fractions for N ≤ 10000 have an odd period?                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 m_n(1000), d_n(1000), a_n(1000), index, pattern_len, result
      logical pattern_found

c initialize the result
      result = 0
      
c loop over all the numbers
      do x1=1,10000

c initialize the pattern_found and get rid of all squares
        pattern_found = .false.
        do x2=1,100
          if (x1.eq.x2**2) then
            pattern_found = .true.
            pattern_len = 0
          endif
        enddo
        
        index = 1
        m_n(index) = 0
        d_n(index) = 1
        a_n(index) = int(sqrt(dble(x1)))
        
        do while(.not.(pattern_found))
          index = index + 1
          m_n(index) = d_n(index-1)*a_n(index-1) - m_n(index-1)
          d_n(index) = (x1-m_n(index)**2)/d_n(index-1)
          a_n(index) = int((sqrt(dble(x1))+dble(m_n(index)))/dble(d_n(index)))
          
          do x2=1,index-1
            if ((m_n(x2).eq.m_n(index)).and.(d_n(x2).eq.d_n(index)).and.(a_n(x2).eq.a_n(index))) then
              pattern_found = .true.
              pattern_len = index-x2
              exit
            endif
          enddo
        enddo
          
        if (((pattern_len/2)*2).ne.pattern_len) then
          result = result + 1
        endif
                  
c        write(*,*) x1, pattern_len, result
      enddo
      
      write(*,*) 'The number of continued fractions with odd period is ',result
      
      end