c-------------------------------------------------------------------------------------------------c
      program project_euler_66
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Consider quadratic Diophantine equations of the form:                                          c
c                                                                                                 c
c  x^2 – Dy^2 = 1                                                                                 c
c                                                                                                 c
c  For example, when D=13, the minimal solution in x is 649^2 – 13×180^2 = 1.                     c
c                                                                                                 c
c  It can be assumed that there are no solutions in positive integers when D is square.           c
c                                                                                                 c
c  By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:            c
c                                                                                                 c
c  3^2 – 2×2^2 = 1                                                                                c
c  2^2 – 3×1^2 = 1                                                                                c
c  9^2 – 5×4^2 = 1                                                                                c
c  5^2 – 6×2^2 = 1                                                                                c
c  8^2 – 7×3^2 = 1                                                                                c
c                                                                                                 c
c  Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.    c
c                                                                                                 c
c  Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is       c
c  obtained.                                                                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 D, max_x(1000), nmax_x, max_D, x_min(1000), nx_min
      integer*8 m_n(0:1000), d_n(0:1000), a_n(0:1000), index
      integer*8 numer(1000), n_numer, denom(1000), n_denom, new_numer(1000), nn_numer, new_denom(1000), nn_denom
      integer*8 numer_sq(1000), nnumer_sq, denom_sq(1000), ndenom_sq
      logical found_min_x, works
      
c initialize parameters
      max_x = 0

c loop over the value of D
      do D=1,1000
C         write(*,*) 'D = ',D
        found_min_x = .false.

c if it is a square then there is no solution
        if (int(dsqrt(dble(D)))**2 .eq. D) then
          x_min = -1
          found_min_x = .true.
        else

c initialize the arrays to find the continued fraction representation of \sqrt{D}          
          index = 0
          m_n(index) = 0
          d_n(index) = 1
          a_n(index) = int(sqrt(dble(D)))
        
          do while(.not.(found_min_x))          
c get the next number in the continued fraction expansion
            index = index + 1
            m_n(index) = d_n(index-1)*a_n(index-1) - m_n(index-1)
            d_n(index) = (D-m_n(index)**2)/d_n(index-1)
            a_n(index) = int((sqrt(dble(D))+dble(m_n(index)))/dble(d_n(index)))
c            write(*,*) m_n(index), d_n(index), a_n(index)

c set up the numerator and denominator of the approximation to \sqrt{D}          
            do x1=1,1000
              numer(x1) = 0
              denom(x1) = 0
            enddo
            n_denom = 1
            denom(1) = a_n(index)
            n_numer = 1
            numer(1) = 1

            do x1=index-1,1,-1
              
              do x2=1,1000
                new_numer(x2) = 0
                new_denom(x2) = 0
              enddo
              
              do x2=1,n_denom
                new_denom(x2) = a_n(x1)*denom(x2) + numer(x2)
                new_numer(x2) = denom(x2)
              enddo
          
c carry over the 10's
              nn_numer = n_denom
              nn_denom = n_denom
              do x2=1,1000
                do while (new_denom(x2).ge.10)
                  new_denom(x2) = new_denom(x2) - 10
                  new_denom(x2+1) = new_denom(x2+1) + 1
                  if (x2.eq.n_denom) nn_denom = nn_denom + 1
                enddo
                do while (new_numer(x2).ge.10)
                  new_numer(x2) = new_numer(x2) - 10
                  new_numer(x2+1) = new_numer(x2+1) + 1
                  if (x2.eq.n_numer) nn_numer = nn_numer + 1
                enddo
              enddo

c copy over the new numerator and denominator to the old
              n_numer = nn_numer
              n_denom = nn_denom
              do x2=1,n_numer
                numer(x2) = new_numer(x2)
              enddo
              do x2=1,n_denom
                denom(x2) = new_denom(x2)
              enddo
            enddo
        
c the final part of the continued fraction is slightly different
            do x1=1,1000
              new_numer(x1) = 0
            enddo
            
            do x1=1,max(n_denom,n_numer)
              new_numer(x1) = numer(x1) + denom(x1)*a_n(0)
            enddo
        
c carry over the 10's
            nn_numer = max(n_denom,n_numer)
            do x1=1,1000
              do while (new_numer(x1).ge.10)
                new_numer(x1) = new_numer(x1) - 10
                new_numer(x1+1) = new_numer(x1+1) + 1
                if (x1.eq.nn_numer) nn_numer = nn_numer + 1
              enddo
            enddo

c copy over the new_numerator to the first numerator
            n_numer = nn_numer
            do x1=1,n_numer
              numer(x1) = new_numer(x1)
            enddo
            
c check to see if the leading number is in fact zero
            if (numer(n_numer).eq.0) n_numer = n_numer - 1
            if (denom(n_denom).eq.0) n_denom = n_denom - 1

C             write(*,*) 'numerator and denomenator'
C             write(*,fmt='(100(I1))') (numer(x1),x1=n_numer,1,-1)
C             write(*,fmt='(100(I1))') (denom(x1),x1=n_denom,1,-1)

c initialize the numer_sq and denom_sq
            nnumer_sq = 0
            ndenom_sq = 0
            do x1=1,1000
              numer_sq(x1) = 0
              denom_sq(x1) = 0
            enddo
            
c get the numerator squared
            call big_number_product(numer,n_numer,numer,n_numer,numer_sq,nnumer_sq)
c subtract 1 from the numerator squared
            if (numer_sq(1).eq.0) then
              numer_sq(1) = 9
              do x1=2,nnumer_sq
                if (numer_sq(x1).ne.0) then
                  numer_sq(x1) = numer_sq(x1) - 1
                  if (numer_sq(nnumer_sq).eq.0) nnumer_sq = nnumer_sq - 1
                  exit
                else
                  numer_sq(x1) = 9
                endif
              enddo
            else
              numer_sq(1) = numer_sq(1) - 1
            endif

c get the denomenator squared
            call big_number_product(denom,n_denom,denom,n_denom,denom_sq,ndenom_sq)
c multiply the denom square by D^2
            do x1=1,ndenom_sq
              denom_sq(x1) = denom_sq(x1)*D
            enddo
            do x1=1,1000
              do while (denom_sq(x1).ge.10)
                denom_sq(x1) = denom_sq(x1) - 10
                if (x1.ne.ndenom_sq) then
                  denom_sq(x1+1) = denom_sq(x1+1) + 1
                else
                  ndenom_sq = ndenom_sq + 1
                  denom_sq(x1+1) = 1
                endif
              enddo
            enddo

C             write(*,*) 'numer^2-1 and D*denom**2'
C             write(*,fmt='(100(I1))') (numer_sq(x1),x1=nnumer_sq,1,-1)
C             write(*,fmt='(100(I1))') (denom_sq(x1),x1=ndenom_sq,1,-1)
C             read(*,*)

            works = .true.
            do x1=1,ndenom_sq
              if (denom_sq(x1).ne.numer_sq(x1)) works = .false.
            enddo
            
            if (works) then
              found_min_x = .true.
              do x1=1000,1,-1
                if (numer(x1).gt.max_x(x1)) then
                  max_D = D
                  do x2=1,1000
                    max_x(x2) = numer(x2)
                  enddo
                  nmax_x = n_numer
C                   write(*,*) 'New answer ',D
C                   write(*,fmt='(100(I1))') (max_x(x2),x2=nmax_x,1,-1)
                  exit
                else if (numer(x1).lt.max_x(x1)) then
                  exit
                endif
              enddo
            endif
          enddo
        endif
      enddo
      write(*,*) 'The value of D in which the min solution has the max value of x is ',max_d
      
      end