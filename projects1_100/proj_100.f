c-------------------------------------------------------------------------------------------------c
      program project_euler_100
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, c
c  and two discs were taken at random, it can be seen that the probability of taking two blue     c
c  discs, P(BB) = (15/21)×(14/20) = 1/2.                                                          c
c                                                                                                 c
c  The next such arrangement, for which there is exactly 50% chance of taking two blue discs at   c
c  random, is a box containing eighty-five blue discs and thirty-five red discs.                  c
c                                                                                                 c
c  By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total,     c
c  determine the number of blue discs that the box would contain.                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this program only
      integer*8 x_blue, N, temp_x, temp_N
      
      x_blue = 15
      N = 21
      do while(N.lt.1000000000000)
        temp_x = x_blue
        temp_N = N
        x_blue = 3*temp_x + 2*temp_N - 2
        N = 4*temp_x + 3*temp_N - 3
      enddo
      
      write(*,*) 'The total number of blue discs needed is ',x_blue
      
      end