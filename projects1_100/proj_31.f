c-------------------------------------------------------------------------------------------------c
      program project_euler_31
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  In England the currency is made up of pound, £, and pence, p, and there are eight coins in     c
c  general circulation:                                                                           c
c                                                                                                 c
c    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).                                          c
c                                                                                                 c
c  It is possible to make £2 in the following way:                                                c
c                                                                                                 c
c    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p                                                    c
c                                                                                                 c
c How many different ways can £2 be made using any number of coins?                               c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 num_combos, total
      
c initialize the number of combinations (start off with a 2 pounder)
      num_combos = 1

c 1 pounder
      do x1=0,2
c 50 pence
        do x2=0,4
c 20 pence
          do x3=0,10
c 10 pence
            do x4=0,20
c 5 pence
              do x5=0,40
c 2 pence
                do x6=0,100
c 1 pence
                  do x7=0,200
                    total = 100*x1 + 50*x2 + 20*x3 + 10*x4 + 5*x5 + 2*x6 + x7
                    if (total.eq.200) num_combos = num_combos + 1
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
      
      write(*,*) 'Number of ways to make 2 pounds using any number of coins is ',num_combos
      
      end 