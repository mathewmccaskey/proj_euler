c-------------------------------------------------------------------------------------------------c
      program project_euler_99
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Comparing two numbers written in index form like 2^11 and 3^7 is not difficult, as any         c
c  calculator would confirm that 2^11 = 2048 < 3^7 = 2187.                                        c
c                                                                                                 c
c  However, confirming that 6^32382518061 > 5^19432525806 would be much more difficult, as both   c
c  numbers contain over three million digits.                                                     c
c                                                                                                 c
c  Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one  c
c  thousand lines with a base/exponent pair on each line, determine which line number has the     c
c  greatest numerical value.                                                                      c
c                                                                                                 c
c  NOTE: The first two lines in the file represent the numbers in the example given above.        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 bases(1000), exponents(1000), line_num, n_primes, primes(20), powers(20)
      double precision log_test, max_log
      
c open file and read
      open(unit=42, file='input_99.txt')
      do x1=1,1000
        read(42,*) bases(x1), exponents(x1)
      enddo
      close(42)

c initialize
      max_log = 0.d0
      
c get the prime factors of each base
      do x1=1,1000
        do x2=1,20
          primes(x2) = 1
          powers(x2) = 0
        enddo
        call get_prime_factors(bases(x1),n_primes,primes,powers)
        
        log_test = 0.d0
        do x2=1,n_primes
          log_test = log_test + exponents(x1)*powers(x2)*dlog(dble(primes(x2)))
        enddo
        
        if (log_test.gt.max_log) then
          max_log = log_test
          line_num = x1
c          write(*,*) 'MAX ',max_log
        else
c          write(*,*) log_test
        endif
      enddo
      
      write(*,*) 'The line with the largest number is ',line_num
      
      end