c-------------------------------------------------------------------------------------------------c
      function ASCII_to_number(char)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function will convert an ASCII character to a number.  For simplicity we will only        c
c  ASCII characters from 32-126.                                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameteres
      character char
      
c big case of ifs
      if (char.eq.' ') ASCII_to_number = 32
      if (char.eq.'!') ASCII_to_number = 33
      if (char.eq.'"') ASCII_to_number = 34
      if (char.eq.'#') ASCII_to_number = 35
      if (char.eq.'$') ASCII_to_number = 36
      if (char.eq.'%') ASCII_to_number = 37
      if (char.eq.'&') ASCII_to_number = 38
      if (char.eq.'`') ASCII_to_number = 39
      if (char.eq.'(') ASCII_to_number = 40
      if (char.eq.')') ASCII_to_number = 41
      if (char.eq.'*') ASCII_to_number = 42
      if (char.eq.'+') ASCII_to_number = 43
      if (char.eq.',') ASCII_to_number = 44
      if (char.eq.'-') ASCII_to_number = 45
      if (char.eq.'.') ASCII_to_number = 46
      if (char.eq.'/') ASCII_to_number = 47
      if (char.eq.'0') ASCII_to_number = 48
      if (char.eq.'1') ASCII_to_number = 49
      if (char.eq.'2') ASCII_to_number = 50
      if (char.eq.'3') ASCII_to_number = 51
      if (char.eq.'4') ASCII_to_number = 52
      if (char.eq.'5') ASCII_to_number = 53
      if (char.eq.'6') ASCII_to_number = 54
      if (char.eq.'7') ASCII_to_number = 55
      if (char.eq.'8') ASCII_to_number = 56
      if (char.eq.'9') ASCII_to_number = 57
      if (char.eq.':') ASCII_to_number = 58
      if (char.eq.';') ASCII_to_number = 59
      if (char.eq.'<') ASCII_to_number = 60
      if (char.eq.'=') ASCII_to_number = 61
      if (char.eq.'>') ASCII_to_number = 62
      if (char.eq.'?') ASCII_to_number = 63
      if (char.eq.'@') ASCII_to_number = 64
      if (char.eq.'A') ASCII_to_number = 65
      if (char.eq.'B') ASCII_to_number = 66
      if (char.eq.'C') ASCII_to_number = 67
      if (char.eq.'D') ASCII_to_number = 68
      if (char.eq.'E') ASCII_to_number = 69
      if (char.eq.'F') ASCII_to_number = 70
      if (char.eq.'G') ASCII_to_number = 71
      if (char.eq.'H') ASCII_to_number = 72
      if (char.eq.'I') ASCII_to_number = 73
      if (char.eq.'J') ASCII_to_number = 74
      if (char.eq.'K') ASCII_to_number = 75
      if (char.eq.'L') ASCII_to_number = 76
      if (char.eq.'M') ASCII_to_number = 77
      if (char.eq.'N') ASCII_to_number = 78
      if (char.eq.'O') ASCII_to_number = 79
      if (char.eq.'P') ASCII_to_number = 80
      if (char.eq.'Q') ASCII_to_number = 81
      if (char.eq.'R') ASCII_to_number = 82
      if (char.eq.'S') ASCII_to_number = 83
      if (char.eq.'T') ASCII_to_number = 84
      if (char.eq.'U') ASCII_to_number = 85
      if (char.eq.'V') ASCII_to_number = 86
      if (char.eq.'W') ASCII_to_number = 87
      if (char.eq.'X') ASCII_to_number = 88
      if (char.eq.'Y') ASCII_to_number = 89
      if (char.eq.'Z') ASCII_to_number = 90
      if (char.eq.'[') ASCII_to_number = 91
      if (char.eq.'\') ASCII_to_number = 92
      if (char.eq.']') ASCII_to_number = 93
      if (char.eq.'^') ASCII_to_number = 94
      if (char.eq.'_') ASCII_to_number = 95
c      if (char.eq.''') ASCII_to_number = 96
      if (char.eq.'a') ASCII_to_number = 97
      if (char.eq.'b') ASCII_to_number = 98
      if (char.eq.'c') ASCII_to_number = 99
      if (char.eq.'d') ASCII_to_number = 100
      if (char.eq.'e') ASCII_to_number = 101
      if (char.eq.'f') ASCII_to_number = 102
      if (char.eq.'g') ASCII_to_number = 103
      if (char.eq.'h') ASCII_to_number = 104
      if (char.eq.'i') ASCII_to_number = 105
      if (char.eq.'j') ASCII_to_number = 106
      if (char.eq.'k') ASCII_to_number = 107
      if (char.eq.'l') ASCII_to_number = 108
      if (char.eq.'m') ASCII_to_number = 109
      if (char.eq.'n') ASCII_to_number = 110
      if (char.eq.'o') ASCII_to_number = 111
      if (char.eq.'p') ASCII_to_number = 112
      if (char.eq.'q') ASCII_to_number = 113
      if (char.eq.'r') ASCII_to_number = 114
      if (char.eq.'s') ASCII_to_number = 115
      if (char.eq.'t') ASCII_to_number = 116
      if (char.eq.'u') ASCII_to_number = 117
      if (char.eq.'v') ASCII_to_number = 118
      if (char.eq.'w') ASCII_to_number = 119
      if (char.eq.'x') ASCII_to_number = 120
      if (char.eq.'y') ASCII_to_number = 121
      if (char.eq.'z') ASCII_to_number = 122
      if (char.eq.'{') ASCII_to_number = 123
      if (char.eq.'|') ASCII_to_number = 124
      if (char.eq.'}') ASCII_to_number = 125
      if (char.eq.'~') ASCII_to_number = 126

      return
      end


      
c-------------------------------------------------------------------------------------------------c
      subroutine big_number_sum(a, len_a, b, len_b, result, len_result)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine takes two strings of integers a and b and add them integer by integer.         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 a(1000), len_a, b(1000), len_b, result(1000), len_result

c initialize the result
      do x1=1,1000
        result(x1) = -1
      enddo
      
c start looping over the digits and add
      do x1=1,max(len_a,len_b)
        result(x1) = a(x1) + b(x1)
      enddo

c once this is done then start carying numbers
      do x1=1,1000
        do while(result(x1).ge.10)
          result(x1) = result(x1) - 10
          if (result(x1+1).eq.-1) then
            result(x1+1) = 1
          else
            result(x1+1) = result(x1+1) + 1
          endif
        enddo
      enddo

c find the length of the result
      do x1=1,1000
        if (result(x1).eq.-1) then
          len_result = x1-1
          return
        endif
      enddo
      
      len_result = 1000

      return
      end



c-------------------------------------------------------------------------------------------------c
      subroutine big_number_product(a, len_a, b, len_b, result, len_result)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine takes two strings of integers a and b and multiplies them integer by integer.  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 a(1000), len_a, b(1000), len_b, result(1000), len_result
      
c initialize the result
      do x1=1,1000
        result(x1) = -1
      enddo      
      
c start looping over the digits and add
      do x1=1,(len_a+len_b-1)
        do x2=1,x1
          if (result(x1).eq.-1) then
            result(x1) = a(x2)*b(x1-x2+1)
          else
            result(x1) = result(x1) + a(x2)*b(x1-x2+1)
          endif
        enddo
      enddo

c once this is done then start carying numbers
      do x1=1,1000
        do while (result(x1).ge.10)
          result(x1) = result(x1) - 10
          if (result(x1+1).eq.-1) then
            result(x1+1) = 1
          else
            result(x1+1) = result(x1+1) + 1
          endif
        enddo
      enddo

c find the length of the result
      do x1=1,1000
        if (result(x1).eq.-1) then
          len_result = x1-1
          return
        endif
      enddo
      
      len_result = 1000

      return
      end      
      
      

c-------------------------------------------------------------------------------------------------c
      function character_to_number(char)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function takes an input character and returns the value associated with that character    c
c  ' ' = 0, 'A' = 1, 'B' = 2, 'C' = 3, etc.                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      character char
      
      if (char.eq.' ') character_to_number = 0
      if (char.eq.'A') character_to_number = 1
      if (char.eq.'B') character_to_number = 2
      if (char.eq.'C') character_to_number = 3
      if (char.eq.'D') character_to_number = 4
      if (char.eq.'E') character_to_number = 5
      if (char.eq.'F') character_to_number = 6
      if (char.eq.'G') character_to_number = 7
      if (char.eq.'H') character_to_number = 8
      if (char.eq.'I') character_to_number = 9
      if (char.eq.'J') character_to_number = 10
      if (char.eq.'K') character_to_number = 11
      if (char.eq.'L') character_to_number = 12
      if (char.eq.'M') character_to_number = 13
      if (char.eq.'N') character_to_number = 14
      if (char.eq.'O') character_to_number = 15
      if (char.eq.'P') character_to_number = 16
      if (char.eq.'Q') character_to_number = 17
      if (char.eq.'R') character_to_number = 18
      if (char.eq.'S') character_to_number = 19
      if (char.eq.'T') character_to_number = 20
      if (char.eq.'U') character_to_number = 21
      if (char.eq.'V') character_to_number = 22
      if (char.eq.'W') character_to_number = 23
      if (char.eq.'X') character_to_number = 24
      if (char.eq.'Y') character_to_number = 25
      if (char.eq.'Z') character_to_number = 26
      
      return
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      function combination(n,r)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the combination of n things taken r at a time                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 n, r
      
c nCr = n!/((n-r)!r!)
      combination = factorial(n)/(factorial(n-r)*factorial(r))
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      function combo(n,r)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the combination of n things taken r at a time                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 n, r

c parameters used in this function only
      logical used(1000)
      integer*8 denom_index, numer_index

c initialize some parameters
      do x1=1,1000
        used(x1) = .false.
      enddo
      
c check to see what kind of index we're looking for in our denominator
      if (r.le.(n/2)) then
        denom_index = r
        numer_index = n-r+1
      else
        denom_index = n-r
        numer_index = r+1
      endif
      
      combo = 1
      do x1=n,numer_index,-1
        combo = combo*x1
        do x2=1,denom_index
          if ((.not.used(x2)).and.(((combo/x2)*x2).eq.combo)) then
            combo = combo/x2
            used(x2) = .true.
          endif
        enddo
      enddo
      
      return
      end      
      
      
      
c-------------------------------------------------------------------------------------------------c
      function factorial(n)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns n!                                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 n
      
      if (n .le. 1) then
        factorial = 1
        return
      else
        factorial = 1
        do x1=n,2,-1
          factorial = factorial*x1
        enddo
      endif
      
      return
      end
      
      
            
c-------------------------------------------------------------------------------------------------c
      function fibonacci(n)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the nth Fibonacci number.                                                c
c                                                                                                 c
c  F(0) = 0, F(1) = 1, and F(n+2) = F(n) + F(n+1)                                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer n

c parameters used in this function only
      integer*8 f_n, f_np1, dummy

c if n = 1 or 2 then we can return the result
      if ((n .eq. 1) .or. (n.eq.2)) then
        fibonacci = 1
        return
      endif
      
c initialize the fibonacci sequence
      f_n = 0
      f_np1 = 1

c if n > 2 then add up the sequence up to the nth number
      if (n .gt. 2) then
        do x1=1, n-1
          dummy = f_np1
          f_np1 = f_n+f_np1
          f_n = dummy
        enddo

      else if (n .lt. 1) then
c if n < 1 then sumbract the sequence up to the -nth number
        do x1=0, n, -1
          dummy = f_np1
          f_np1 = f_np1-f_n
          f_n = dummy
        enddo
        
      endif
      
      fibonacci = f_np1
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      subroutine get_digits(number, base, digit, ndigits)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine returns the number of digits and the digits themselves given an input number   c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 number, base, digit(1000), ndigits
      
c parameters used in this subroutine only
      integer*8 factor, dummy

c initialize the digits
      do x1=1,1000
        digit(x1) = 0
      enddo
      
c find out where to start finding the digits of number
      factor = 1
      dummy = number
      ndigits = 0

      do while (dummy.ge.factor)
        ndigits = ndigits + 1
        digit(ndigits) = (dummy - (dummy/(factor*base))*(factor*base))/factor
        dummy = dummy - digit(ndigits)*factor
        factor = factor*base
      enddo
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      subroutine get_number_from_digits(number, base, digit, ndigits)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine returns the value of a number given the input digits, ndigits, and base.       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 number, base, digit(1000), ndigits
      
      number = 0
      do x1=1,ndigits
        number = number + digit(x1)*base**(x1-1)
      enddo
      
      return
      end
    
    

c-------------------------------------------------------------------------------------------------c
      subroutine get_which_digits(digit, ndigit, which_digit)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Given an input set of digits this subroutine sorts them into how many of which digit one has.  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 digit(1000), ndigit, which_digit(0:9)
      
c reset which_digit
      do x1=0,9
        which_digit(x1) = 0
      enddo
      
c loop over all the digits and sort
      do x1=1,ndigit
        which_digit(digit(x1)) = which_digit(digit(x1)) + 1
      enddo
      
      return
      end
      
      
c-------------------------------------------------------------------------------------------------c
      subroutine get_prime_factors(number, n_primes, primes, power)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine returns the prime factors of any input number.                                 c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 number, n_primes, primes(100), power(100)

c parameters used in this subroutine only
      integer*8 dummy, prime_work
      
c initialize the number of primes
      n_primes = 0
      prime_work = 2
      dummy = number
      
      do while(dummy.gt.1)
        if ((dummy/prime_work)*prime_work.eq.dummy) then
          n_primes = n_primes + 1
          primes(n_primes) = prime_work
          
          do while ((dummy/prime_work)*prime_work.eq.dummy)
            dummy = dummy/prime_work
            power(n_primes) = power(n_primes) + 1
          enddo
        endif
        
        prime_work = prime_work + 1
        do while(.not.(is_prime(prime_work)))
          prime_work = prime_work + 1
        enddo
      enddo
      
      return
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      function has_common_multiple(number1, number2)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This subroutine returns true is number1 and number2 have a common multiple.                    c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 number1, number2
      
c loop over all the possible divisors
      has_common_multiple = .false.
      do x1=2,int(dble(max(number1,number2)))
        if ((is_multiple(number1,x1)).and.(is_multiple(number2,x1))) then
          has_common_multiple = .true.
          return
        endif
      enddo
      
      return
      end
      
      

c-------------------------------------------------------------------------------------------------c
      function is_multiple(number, multiple)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns true if the integer number is a multiple of the integer multiple.        c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters
      integer*8 number, multiple

c take advantage of integer multiplication and division to find out if it is a multiple
      if ((number/multiple)*multiple .eq. number) then
        is_multiple = .true.
      else
        is_multiple = .false.
      endif
      
      return
      end
      
      

c-------------------------------------------------------------------------------------------------c
      function is_palindrome(digit, ndigits)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns true if the number is a palindrome.                                      c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 digit(1000), ndigits
      
c initialize the function
      is_palindrome = .true.
      
c loop over all the digits
      do x1=1,ndigits
        if (digit(x1).ne.digit(ndigits-x1+1)) is_palindrome = .false.
      enddo
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      function is_prime(number)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns true if the integer number is prime.                                     c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 number

c check if the number is positive or negative
      if (number.le.1) then
        is_prime = .false.
        return
      endif

c if the number is 2 then automatically return true
      if (number.eq.2) then
        is_prime = .true.
        return
      endif

c loop over all the integers from 2 up to int(sqrt(number))+1
      do x1=2,int(dsqrt(dble(number)))+1
        if (is_multiple(number,x1)) then
          is_prime = .false.
          return
        endif
      enddo

c if it made it here then the number is prime
      is_prime = .true.

      return
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      function num_factors(number, factor)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns how many times the integer factor factors into the integer number.       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 number, factor
      
c parameters used in this function only
      integer*8 dummy
      
c initialize the number of factors and the dummy variable
      num_factors = 0
      dummy = number
      
c keep looping while factor is still a multiple of number
      do while (is_multiple(dummy, factor))
        num_factors = num_factors + 1
        dummy = dummy/factor
      enddo
      
      return
      end
      
      

c-------------------------------------------------------------------------------------------------c
      function number_to_ASCII(num)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function will convert a number to an ASCII character.  For simplicity we will only        c
c  ASCII characters from 32-126.                                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameteres
      integer*8 num
      
c big case of ifs
      if (num.eq.32) number_to_ASCII = ' '
      if (num.eq.33) number_to_ASCII = '!'
      if (num.eq.34) number_to_ASCII = '"'
      if (num.eq.35) number_to_ASCII = '#'
      if (num.eq.36) number_to_ASCII = '$'
      if (num.eq.37) number_to_ASCII = '%'
      if (num.eq.38) number_to_ASCII = '&'
      if (num.eq.39) number_to_ASCII = '`'
      if (num.eq.40) number_to_ASCII = '('
      if (num.eq.41) number_to_ASCII = ')'
      if (num.eq.42) number_to_ASCII = '*'
      if (num.eq.43) number_to_ASCII = '+'
      if (num.eq.44) number_to_ASCII = ','
      if (num.eq.45) number_to_ASCII = '-'
      if (num.eq.46) number_to_ASCII = '.'
      if (num.eq.47) number_to_ASCII = '/'
      if (num.eq.48) number_to_ASCII = '0'
      if (num.eq.49) number_to_ASCII = '1'
      if (num.eq.50) number_to_ASCII = '2'
      if (num.eq.51) number_to_ASCII = '3'
      if (num.eq.52) number_to_ASCII = '4'
      if (num.eq.53) number_to_ASCII = '5'
      if (num.eq.54) number_to_ASCII = '6'
      if (num.eq.55) number_to_ASCII = '7'
      if (num.eq.56) number_to_ASCII = '8'
      if (num.eq.57) number_to_ASCII = '9'
      if (num.eq.58) number_to_ASCII = ':'
      if (num.eq.59) number_to_ASCII = ';'
      if (num.eq.60) number_to_ASCII = '<'
      if (num.eq.61) number_to_ASCII = '='
      if (num.eq.62) number_to_ASCII = '>'
      if (num.eq.63) number_to_ASCII = '?'
      if (num.eq.64) number_to_ASCII = '@'
      if (num.eq.65) number_to_ASCII = 'A'
      if (num.eq.66) number_to_ASCII = 'B'
      if (num.eq.67) number_to_ASCII = 'C'
      if (num.eq.68) number_to_ASCII = 'D'
      if (num.eq.69) number_to_ASCII = 'E'
      if (num.eq.70) number_to_ASCII = 'F'
      if (num.eq.71) number_to_ASCII = 'G'
      if (num.eq.72) number_to_ASCII = 'H'
      if (num.eq.73) number_to_ASCII = 'I'
      if (num.eq.74) number_to_ASCII = 'J'
      if (num.eq.75) number_to_ASCII = 'K'
      if (num.eq.76) number_to_ASCII = 'L'
      if (num.eq.77) number_to_ASCII = 'M'
      if (num.eq.78) number_to_ASCII = 'N'
      if (num.eq.79) number_to_ASCII = 'O'
      if (num.eq.80) number_to_ASCII = 'P'
      if (num.eq.81) number_to_ASCII = 'Q'
      if (num.eq.82) number_to_ASCII = 'R'
      if (num.eq.83) number_to_ASCII = 'S'
      if (num.eq.84) number_to_ASCII = 'T'
      if (num.eq.85) number_to_ASCII = 'U'
      if (num.eq.86) number_to_ASCII = 'V'
      if (num.eq.87) number_to_ASCII = 'W'
      if (num.eq.88) number_to_ASCII = 'X'
      if (num.eq.89) number_to_ASCII = 'Y'
      if (num.eq.90) number_to_ASCII = 'Z'
      if (num.eq.91) number_to_ASCII = '['
      if (num.eq.92) number_to_ASCII = '\'
      if (num.eq.93) number_to_ASCII = ']'
      if (num.eq.94) number_to_ASCII = '^'
      if (num.eq.95) number_to_ASCII = '_'
c      if (num.eq.96) number_to_ASCII = '''
      if (num.eq.97) number_to_ASCII = 'a'
      if (num.eq.98) number_to_ASCII = 'b'
      if (num.eq.99) number_to_ASCII = 'c'
      if (num.eq.100) number_to_ASCII = 'd'
      if (num.eq.101) number_to_ASCII = 'e'
      if (num.eq.102) number_to_ASCII = 'f'
      if (num.eq.103) number_to_ASCII = 'g'
      if (num.eq.104) number_to_ASCII = 'h'
      if (num.eq.105) number_to_ASCII = 'i'
      if (num.eq.106) number_to_ASCII = 'j'
      if (num.eq.107) number_to_ASCII = 'k'
      if (num.eq.108) number_to_ASCII = 'l'
      if (num.eq.109) number_to_ASCII = 'm'
      if (num.eq.110) number_to_ASCII = 'n'
      if (num.eq.111) number_to_ASCII = 'o'
      if (num.eq.112) number_to_ASCII = 'p'
      if (num.eq.113) number_to_ASCII = 'q'
      if (num.eq.114) number_to_ASCII = 'r'
      if (num.eq.115) number_to_ASCII = 's'
      if (num.eq.116) number_to_ASCII = 't'
      if (num.eq.117) number_to_ASCII = 'u'
      if (num.eq.118) number_to_ASCII = 'v'
      if (num.eq.119) number_to_ASCII = 'w'
      if (num.eq.120) number_to_ASCII = 'x'
      if (num.eq.121) number_to_ASCII = 'y'
      if (num.eq.122) number_to_ASCII = 'z'
      if (num.eq.123) number_to_ASCII = '{'
      if (num.eq.124) number_to_ASCII = '|'
      if (num.eq.125) number_to_ASCII = '}'
      if (num.eq.126) number_to_ASCII = '~'

      return
      end



c-------------------------------------------------------------------------------------------------c
      function number_to_character(num)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function takes an input character and returns the value associated with that character    c
c  ' ' = 0, 'A' = 1, 'B' = 2, 'C' = 3, etc.                                                       c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameter
      integer*8 num
      
      if (num.eq.0) number_to_character = ' '
      if (num.eq.1) number_to_character = 'A'
      if (num.eq.2) number_to_character = 'B'
      if (num.eq.3) number_to_character = 'C'
      if (num.eq.4) number_to_character = 'D'
      if (num.eq.5) number_to_character = 'E'
      if (num.eq.6) number_to_character = 'F'
      if (num.eq.7) number_to_character = 'G'
      if (num.eq.8) number_to_character = 'H'
      if (num.eq.9) number_to_character = 'I'
      if (num.eq.10) number_to_character = 'J'
      if (num.eq.11) number_to_character = 'K'
      if (num.eq.12) number_to_character = 'L'
      if (num.eq.13) number_to_character = 'M'
      if (num.eq.14) number_to_character = 'N'
      if (num.eq.15) number_to_character = 'O'
      if (num.eq.16) number_to_character = 'P'
      if (num.eq.17) number_to_character = 'Q'
      if (num.eq.18) number_to_character = 'R'
      if (num.eq.19) number_to_character = 'S'
      if (num.eq.20) number_to_character = 'T'
      if (num.eq.21) number_to_character = 'U'
      if (num.eq.22) number_to_character = 'V'
      if (num.eq.23) number_to_character = 'W'
      if (num.eq.24) number_to_character = 'X'
      if (num.eq.25) number_to_character = 'Y'
      if (num.eq.26) number_to_character = 'Z'
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      function permutation(n,r)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the permutation of n things taken r at a time                            c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c input parameters
      integer*8 n, r
      
c nPr = n!/(n-r)!
      permutation = factorial(n)/factorial(n-r)
      
      return
      end
      

      
c-------------------------------------------------------------------------------------------------c
      function triangle(number)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the 'number'th triangle number                                           c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters
      integer*8 number
      
c triangle number
      triangle = number*(number+1)/2
      
      return
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      function pentagonal(number)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the 'number'th pentagonal number                                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters
      integer*8 number
      
c triangle number
      pentagonal = number*(3*number-1)/2
      
      return
      end
      
      
      
c-------------------------------------------------------------------------------------------------c
      function hexagonal(number)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function returns the 'number'th hexagonal number                                          c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters
      integer*8 number
      
c triangle number
      hexagonal = number*(2*number-1)
      
      return
      end



c-------------------------------------------------------------------------------------------------c
      function xor(num1,num2)
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  This function inputs two numbers and then does a bitwise exclusive OR.                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c input parameters
      integer*8 num1, num2

c parameters used in this function only
      integer*8 base, ndigits1, digits1(1000), ndigits2, digits2(1000), ndigits3, digits3(1000)
      
c get the binary digits of the two input numbers
      base = 2
      call get_digits(num1,base,digits1,ndigits1)
      call get_digits(num2,base,digits2,ndigits2)
      ndigits3 = max(ndigits1,ndigits2)
      
      do x1=1,ndigits3
        if (digits1(x1).eq.digits2(x1)) then
          digits3(x1) = 0
        else
          digits3(x1) = 1
        endif
      enddo
      
      call get_number_from_digits(xor,base,digits3,ndigits3)
      
      return
      end