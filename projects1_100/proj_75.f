c-------------------------------------------------------------------------------------------------c
      program project_euler_75
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It turns out that 12 cm is the smallest length of wire that can be bent to form an integer     c
c  sided right angle triangle in exactly one way, but there are many more examples.               c
c                                                                                                 c
c  12 cm: (3,4,5)                                                                                 c
c  24 cm: (6,8,10)                                                                                c
c  30 cm: (5,12,13)                                                                               c
c  36 cm: (9,12,15)                                                                               c
c  40 cm: (8,15,17)                                                                               c
c  48 cm: (12,16,20)                                                                              c
c                                                                                                 c
c  In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right   c
c  angle triangle, and other lengths allow more than one solution to be found; for example, using c
c  120 cm it is possible to form exactly three different integer sided right angle triangles.     c
c                                                                                                 c
c  120 cm: (30,40,50), (20,48,52), (24,45,51)                                                     c
c                                                                                                 c
c  Given that L is the length of the wire, for how many values of L ≤ 1,500,000 can exactly one   c
c  integer sided right angle triangle be formed?                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this subroutine only
      integer*8 num_lens, index, m, n, k, a, b, c, lens(1500000)
      logical co_prime, unique

c initailze some parameters
      num_lens = 0
      index = 0

c tabulate some pythagorean triplets
c  n goes from 1 to 856 because if n=866 then smallest pythagorean triplet has a perimeter larger than 1.5 mil
c  m is incremented by 2 because n and m cannot be both even or both odd
      do n=1,865
        do m=n+1,866,2
c  check if n and m are co_prime
          co_prime = .true.
          if (n.ne.1) then
            do x1=2,n
              if (((x1*(n/x1)).eq.n).and.((x1*(m/x1)).eq.m)) co_prime = .false.
            enddo
          endif
          if (co_prime) then
            a = (m**2 - n**2)
            b = 2*m*n
            c = (m**2 + n**2)
c  loop over all possible factors k
            if ((a+b+c).le.1500000) then
              do k=1,1500000/(a+b+c)
                index = index + 1
                lens(index) = k*(a+b+c)
              enddo
            endif            
          endif
        enddo
      enddo
      
c loop over the generated lengths and see how many are unique
      do x1=1,index
        unique = .true.
        do x2=1,index
          if (x1.ne.x2) then
            if (lens(x1).eq.lens(x2)) then
              unique = .false.
              goto 100
            endif
          endif
        enddo
 100    continue
        if (unique) then
          num_lens = num_lens + 1
          if (((num_lens/10000)*10000).eq.num_lens) write(*,*) x1,num_lens
        endif
      enddo
      
      write(*,*) 'Number of values of L with one intereger sided right triangle is ',num_lens
      
      end